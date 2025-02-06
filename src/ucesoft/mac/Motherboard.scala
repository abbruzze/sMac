package ucesoft.mac

import ucesoft.mac.Clock.Clockable
import ucesoft.mac.adb.ADBTransceiver
import ucesoft.mac.audio.AudioDevice
import ucesoft.mac.cpu.m68k.{M68000, M6800X0}
import ucesoft.mac.io.MacVIA
import ucesoft.mac.keyboard.MacKeyboard
import ucesoft.mac.mmu.MMU
import ucesoft.mac.mouse.QuadMouse
import ucesoft.mac.rtc.RTC
import ucesoft.mac.scsi.NCR5380
import ucesoft.mac.serial.Z8530
import ucesoft.mac.storage.{DiskController, IWM, SWIM}
import ucesoft.mac.video.MacVideo
import ucesoft.mac.video.MacVideo.VideoSignalListener

/**
 * @author Alessandro Abbruzzetti
 *         Created on 06/01/2025 15:48  
 */
class Motherboard extends MACComponent with Clockable with VideoSignalListener with M6800X0.BusAccessListener:
  private inline val SYSTEM_CLOCK_FREQ = 7_833_600
  private inline val VIA_CLOCK_DIVIDER = 10
  private var VIA_CLOCK_FREQ = SYSTEM_CLOCK_FREQ / VIA_CLOCK_DIVIDER

  final val masterClock = new Clock("MasterClock", SYSTEM_CLOCK_FREQ, autoClockIncrement = true)
  final val adb = new ADBTransceiver
  final val scsi = new NCR5380
  final val rtc = new RTC
  final val scc = new Z8530(sccIRQLow)
  final val iwm : DiskController = new SWIM
  final val video = new MacVideo
  final val mouse = new QuadMouse(zoomFactorX = 2,zoomFactorY = 2)
  final val keyboard = new MacKeyboard
  final val audio = new AudioDevice((370 * 60.15).toInt) // 22.25 Khz
  final val via = new MacVIA(viaIRQLow, audio, video, mouse, rtc, iwm, keyboard, adb, setOverlay)
  final val mmu = new MMU(scc, iwm, via, scsi)
  final val m68k = new M68000(mmu)

  private var via1SecCycleCounter = 0
  private var via1CycleCounter = 0L
  private var m68kIRQLevel = 0
  private var mouseCycles = 0
  private var videoBusAcquired = false
  private var warpMode = false
  private var viaIRQ = false
  private var scsiIRQ = false

  private var pendingSwitchIRQ = false

  def pushSwitch(): Unit =
    pendingSwitchIRQ = true

  private def sccIRQLow(low: Boolean): Unit =
    if low then m68kIRQLevel |= 2 else m68kIRQLevel &= ~2
    checkIRQ()

  private def viaIRQLow(low: Boolean): Unit =
    viaIRQ = low
    if viaIRQ || scsiIRQ then m68kIRQLevel |= 1 else m68kIRQLevel &= ~1
    checkIRQ()

  private def scsiIRQLow(low: Boolean): Unit =
    if via.isSCSIIRQEnabled then
      scsiIRQ = low
      if viaIRQ || scsiIRQ then m68kIRQLevel |= 1 else m68kIRQLevel &= ~1
      checkIRQ()

  private def checkIRQ(): Unit =
    m68k.interrupt(if m68kIRQLevel == 3 then 2 else m68kIRQLevel)

  private def setOverlay(overlayOn: Boolean): Unit = mmu.setOverlay(overlayOn)

  override protected def setModel(model: MacModel): Unit = 
    super.setModel(model)
    masterClock.setFrequency(model.clockRateMhz)
    VIA_CLOCK_FREQ = model.clockRateMhz / VIA_CLOCK_DIVIDER

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
    add(adb)

    setLogger(log)

    m68k.setBusAccessListener(this)
    masterClock.setClockable(this)

    adb.addDevice(mouse.getADBMouse)
    adb.addDevice(keyboard.getADBKeyboard)

    scsi.setIRQHandler(scsiIRQLow)

    video.setBusAcquiring(acquire => videoBusAcquired = acquire)
    video.addVideoSignalListener(this)

    m68k.setResetDeviceListener(() => resetComponent())
  end init

  override def onMessage(msg: MessageBus.Message): Unit =
    msg match
      case MessageBus.FloppyEjected(_, diskName, error) =>
      // TODO
      case MessageBus.WarpMode(_, enabled) =>
        warpMode = enabled
      case _ =>

  // =========================== MAIN LOOP ===========================
  override final def busAccess(address: Int, mode: M6800X0.BusAccessMode, cycles: Int): Unit =
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
    if macModel.ordinal < MacModel.SE.ordinal then
      if mouse.isReady then
        mouseCycles += 1
        if mouseCycles == 1250 then
          mouseCycles = 0
          mouse.quad()
          scc.setDCD(0, mouse.X1 != 0)
          scc.setDCD(1, mouse.Y1 != 0)
  end loopDevices

  override final def clock(cycles: Long): Unit = 
    m68k.execute()
    if pendingSwitchIRQ then
      pendingSwitchIRQ = false
      m68k.interrupt(4)
      m68k.setInterruptAckListener(_ => {
        m68k.setInterruptAckListener(null)
      })

  override def onVBlank(on: Boolean): Unit = via.CA1In(!on)

  override def onHBlank(on: Boolean, vblank: Boolean, videoLine: Int): Unit =
    if on then
      val audioBuffer = audio.getAudioBufferAddress
      val ram = mmu.getRAM
      val offset = (audioBuffer + (videoLine << 1)) & (ram.length - 1)
      // audio sample
      val audioSample = ram(offset).toByte
      audio.newSample(audioSample)
      // pwm sample
      val pwm = ram(offset + 1)
      iwm.updatePWMSample(pwm)
  end onHBlank
