package ucesoft.mac

import com.formdev.flatlaf.FlatLightLaf
import ucesoft.mac.Clock.Clockable
import ucesoft.mac.audio.AudioDevice
import ucesoft.mac.cpu.m68k.{M68000, M6800X0}
import ucesoft.mac.debugger.Debugger
import ucesoft.mac.io.MacVIA
import ucesoft.mac.keyboard.MacKeyboard
import ucesoft.mac.misc.{DNDHandler, Preferences}
import ucesoft.mac.mmu.MMU
import ucesoft.mac.mouse.QuadMouse
import ucesoft.mac.rtc.RTC
import ucesoft.mac.scsi.{NCR5380, SCSIHardDrive}
import ucesoft.mac.serial.Z8530
import ucesoft.mac.storage.{IWM, MacDiskImage}
import ucesoft.mac.ui.StoragePanel
import ucesoft.mac.video.MacVideo
import ucesoft.mac.video.MacVideo.VideoSignalListener

import java.awt.{Dimension, FlowLayout}
import java.io.File
import java.util.Properties
import java.util.logging.Level
import javax.swing.*

/**
 * @author Alessandro Abbruzzetti
 *         Created on 21/11/2024 15:58  
 */
object MAC extends MACComponent with Clockable with VideoSignalListener with M6800X0.BusAccessListener:
  private inline val SYSTEM_CLOCK_FREQ  = 7_833_600
  private inline val VIA_CLOCK_DIVIDER  = 10
  private inline val VIA_CLOCK_FREQ     = SYSTEM_CLOCK_FREQ / VIA_CLOCK_DIVIDER

  private val masterClock = new Clock("MasterClock",SYSTEM_CLOCK_FREQ,autoClockIncrement = true)
  final val scsi = new NCR5380
  final val rtc = new RTC
  final val scc = new Z8530(sccIRQLow)
  final val iwm = new IWM
  final val video = new MacVideo
  final val mouse = new QuadMouse(zoomFactor = 2)
  final val keyboard = new MacKeyboard
  final val audio = new AudioDevice((370 * 60.15).toInt) // 22.25 Khz
  final val via = new MacVIA(viaIRQLow,audio,video,mouse,rtc,iwm,keyboard,setOverlay)
  final val mmu = new MMU(scc,iwm,via,scsi)
  final val m68k = new M68000(mmu)

  private val viaClockable : Clockable = cycles => {
    via.clock(cycles)
    // 1 sec interrupt handling
    via1SecCycleCounter += 1
    if via1SecCycleCounter == VIA_CLOCK_FREQ then
      via1SecCycleCounter = 0
      via.CA2In(state = true)
      via.CA2In(state = false)
      rtc.oneSecondTrigger()
  }

  private var via1SecCycleCounter = 0
  private var via1CycleCounter = 0L
  private var m68kIRQLevel = 0
  private var mouseCycles = 0
  private var videoBusAcquired = false
  private var warpMode = false

  private def sccIRQLow(low:Boolean): Unit =
    if low then m68kIRQLevel |= 2 else m68kIRQLevel &= ~2
    checkIRQ()
  private def viaIRQLow(low:Boolean): Unit =
    if low then m68kIRQLevel |= 1 else m68kIRQLevel &= ~1
    checkIRQ()
  private def checkIRQ(): Unit =
    m68k.interrupt(if m68kIRQLevel == 3 then 2 else m68kIRQLevel)

  private def setOverlay(overlayOn:Boolean): Unit = mmu.setOverlay(overlayOn)

  override def init(): Unit =
    add(scsi)
    add(masterClock)
    add(rtc)
    add(scc)
    add(iwm)
    add(video)
    add(mouse)
    add(keyboard)
    add(audio)
    add(via)
    add(mmu)
    add(m68k)

    setLogger(log)

    m68k.setBusAccessListener(this)
    masterClock.setClockable(this)

    //video.setBusAcquiring(acquire => m68k.setBUSAvailable(!acquire))
    video.setBusAcquiring(acquire => videoBusAcquired = acquire)
    video.addVideoSignalListener(this)

    m68k.setResetDeviceListener(() => {
      println("Resetting devices...")
      resetComponent()
    })
  end init

  override def onMessage(msg: MessageBus.Message): Unit =
    msg match
      case MessageBus.FloppyEjected(_,diskName,error) =>
        // TODO
      case MessageBus.WarpMode(_,enabled) =>
        warpMode = enabled
      case _ =>

  // =========================== MAIN LOOP ===========================
  override def busAccess(address:Int,mode: M6800X0.BusAccessMode, cycles: Int): Unit =
    var cycleCount = cycles

    masterClock.addCycles(cycles)

    while cycleCount > 0 do
      if address < 0x40_0000 then // access to RAM
        while !warpMode && videoBusAcquired do
          masterClock.addCycles(1)
          loopDevices()
      loopDevices()
      cycleCount -= 1
    end while
  end busAccess

  private def loopDevices(): Unit =
    // VIA
    via1CycleCounter += 1
    if (via1CycleCounter % 10) == 0 then
      via.clock(via1CycleCounter)
      // 1 sec interrupt handling
      via1SecCycleCounter += 1
      if via1SecCycleCounter == VIA_CLOCK_FREQ then
        via1SecCycleCounter = 0
        via.CA2In(state = true)
        via.CA2In(state = false)
        rtc.oneSecondTrigger()
    // VIDEO
    video.cycle()
    // IWM
    iwm.cycle()
    // Mouse
    if mouse.isReady then
      mouseCycles += 1
      if mouseCycles == 1250 then
        mouseCycles = 0
        mouse.quad()
        scc.setDCD(0, mouse.X1 != 0)
        scc.setDCD(1, mouse.Y1 != 0)
  end loopDevices

  override def clock(cycles: Long): Unit =
    m68k.execute()
  end clock

//  override def clock(cycles: Long): Unit =
//    // CPU
//    if m68WaitCycles == 0 then
//      m68WaitCycles = m68k.execute() - 1
//    else
//      m68WaitCycles -= 1
//    // VIDEO
//    video.cycle()
//    // IWM
//    iwm.cycle()
//    // Mouse
//    if mouse.isReady then
//      mouseCycles += 1
//      if mouseCycles == 1250 then
//        mouseCycles = 0
//        mouse.quad()
//        scc.setDCD(0,mouse.X1 != 0)
//        scc.setDCD(1, mouse.Y1 != 0)
  // ==================================================================

  override def onVBlank(on: Boolean): Unit = via.CA1In(!on)
  override def onHBlank(on: Boolean, vblank: Boolean, videoLine: Int): Unit =
    if on then
      val audioBuffer = audio.getAudioBufferAddress
      val ram = mmu.getRAM
      val offset = (audioBuffer + (videoLine << 1)) & (ram.length - 1)
      // audio sample
      val audioSample = ram(offset).toByte // 0x00 minimum value, 0xFF maximum value
      audio.newSample(audioSample)
      // pwm sample
      val pwm = ram(offset + 1)
      iwm.updatePWMSample(pwm)
  end onHBlank

  def main(args:Array[String]): Unit =
    var model : MacModel = null

    ROM.loadROM("""G:\My Drive\Emulatori\Macintosh\1986-03 - 4D1F8172 - MacPlus v3.ROM""") match
      case Right(ROM(file,rom,hash,_model)) =>
        log.info("ROM '%s' loaded [%s]: model %s",file,hash,_model)
        mmu.setROM(rom)
        model = _model
      case Left(err) =>
        println(err)
        sys.exit(1)

    model.setTotalRamInK(model.ramSizesInK(2))

    FlatLightLaf.setup()
    JFrame.setDefaultLookAndFeelDecorated(false)
    JDialog.setDefaultLookAndFeelDecorated(false)
    UIManager.setLookAndFeel("com.formdev.flatlaf.themes.FlatMacLightLaf")

    val debugger = new Debugger(m68k = m68k, video = video, () => {}, video,via,rtc,iwm,scc,audio,keyboard,scsi)
    val logger = Logger.setLogger(debugger.log)
    logger.setCPU(m68k)
    setLogger(logger)

    val frame = new JFrame("Testing Macintosh")
    val display = new Display(model.videoSettings.horizontalPixels,model.videoSettings.verticalLines,frame.getTitle,frame,masterClock)
    display.setFocusable(true)
    display.addKeyListener(keyboard)
    display.setPreferredSize(new Dimension(model.videoSettings.horizontalPixels * 2,model.videoSettings.verticalLines * 2))
    display.addMouseListener(mouse)
    display.addMouseMotionListener(mouse)
    frame.getContentPane.add("Center",display)

    mouse.setCapture(on = true,display)

    initComponent()
    resetComponent()
    setComponentModel(model)

    debugger.setRAM(mmu.getRAM)

    logger.setLevel(Level.SEVERE)

    video.setRAM(mmu.getRAM)
    video.setDisplay(display)

    keyboard.setModel(MacKeyboard.KeyboardModel.M0110A)

    debugger.setROM(mmu.getROM)

    masterClock.setErrorHandler(t => {
      t.printStackTrace()
      sys.exit(1)
    })

    val southPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val warp = new JToggleButton("Warp")
    warp.setFocusable(false)

    southPanel.add(warp)
    val storagePanel = new StoragePanel
    southPanel.add(storagePanel)
    storagePanel.setDiskette(model.floppySettings.drivesNumber)
    iwm.addDiskControllerListener(storagePanel)

    warp.addActionListener(_ => {
      masterClock.setWarpMode(warp.isSelected)
      MessageBus.send(MessageBus.WarpMode(this,warp.isSelected))
    })

    frame.getContentPane.add("South",southPanel)

    // floppy
//    val system = new MacDiskImage("""G:\My Drive\Emulatori\Macintosh\system 7.1\Install.img""")
//    iwm.insertFloppy(0, system)
//    iwm.insertFloppy(1, new MacDiskImage("""C:\Users\ealeame\OneDrive - Ericsson\Desktop\Lemmings demo.img"""))

    // scsi
    val s1 = new SCSIHardDrive(3,"""C:\Users\ealeame\OneDrive - Ericsson\Desktop\sMac\HD20_512-MacPlus.hda""")//"""C:\temp\PCE\hd1.img""") //"""C:\Users\ealeame\OneDrive - Ericsson\Desktop\hdd""")
    val s2 = new SCSIHardDrive(4,"""C:\temp\PCE\hd1.img""") //"""C:\Users\ealeame\Documents\GitHub\snow\target\release\hdd1.img""")
    val s3 = new SCSIHardDrive(2,"""C:\Users\ealeame\Documents\GitHub\snow\target\release\hdd1.img""")
    scsi.setTarget(s1)
    scsi.setTarget(s2)
    scsi.setTarget(s3)
    scsi.setSCSIListener(storagePanel)
    storagePanel.setSCSI(s1)
    storagePanel.setSCSI(s2)
    storagePanel.setSCSI(s3)

    // DND
    frame.setTransferHandler(new DNDHandler((file,copy) => {
      val index = if copy then 1 else 0
      println(s"Dragged[$index] $file")
      iwm.insertFloppy(index, new MacDiskImage(file.toString))
    }))

    // patch ram for skipping memory test
    // if skipping memory test is needed
    model.skipMemoryTestAddress.foreach { case (address, value, size) =>
      mmu.patchRAM(address,value,size)
    }

    // bus
    registerOnBus()

    // configuration
    val pref = new Preferences
    val prop = new Properties()
    val conf = ConfigContext(homeDir = new File("""C:\Users\ealeame\OneDrive - Ericsson\Desktop\sMac"""),pref = pref,conf = prop)
    MessageBus.send(MessageBus.Configuration(this,conf))

    frame.addWindowListener(new java.awt.event.WindowAdapter {
      override def windowClosing(e: java.awt.event.WindowEvent): Unit = {
        println("Shutting down...")
        MessageBus.send(MessageBus.Shutdown(this,conf))
        sys.exit(0)
      }
    })

    SwingUtilities.invokeLater(() => {
      frame.pack()
      frame.setVisible(true)
      debugger.enableTracing(true)
      audio.start()
      masterClock.start()
    })



