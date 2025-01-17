package ucesoft.mac.ui

import com.formdev.flatlaf.FlatLightLaf
import ucesoft.mac.debugger.Debugger
import ucesoft.mac.misc.{DNDHandler, FullScreenMode, GIFPanel, Preferences}
import ucesoft.mac.scsi.SCSIHardDrive
import ucesoft.mac.storage.MacDiskImage
import ucesoft.mac.*

import java.awt.event.{KeyAdapter, KeyEvent, WindowAdapter}
import java.awt.{Dimension, FlowLayout, Point, Toolkit}
import java.io.File
import java.util.Properties
import javax.swing.*
import scala.collection.mutable.ListBuffer
import scala.compiletime.uninitialized

object MacGUI:
  def main(args:Array[String]): Unit =
    if System.getProperty("swing.defaultlaf") == null then
      FlatLightLaf.setup()
      JFrame.setDefaultLookAndFeelDecorated(false)
      JDialog.setDefaultLookAndFeelDecorated(false)
      UIManager.setLookAndFeel("com.formdev.flatlaf.FlatDarculaLaf")

    val preBuildLogs = new ListBuffer[String]
    Logger.setLogger(msg => preBuildLogs += msg)
    val mac = new MacGUI
    preBuildLogs.toList.foreach(Logger.getLogger.addLog)
    try
      mac.configure(args)
      mac.boot()
      mac.run()
    catch
      case i: Preferences.PreferenceIllegalArgumentException =>
        println(s"Bad command line argument: ${i.getMessage}")
        sys.exit(100)
      case t: Throwable =>
        mac.errorHandler(t)
        sys.exit(1)

/**
 * @author Alessandro Abbruzzetti
 *         Created on 14/01/2025 19:12  
 */
class MacGUI extends MessageBus.MessageListener:
  // menu
  private val debugMenuItem = new JCheckBoxMenuItem("Debugger")
  private val warpItem = new JCheckBoxMenuItem("Warp mode")
  private val mouseCapItem = new JCheckBoxMenuItem("Mouse capture")
  private val autoWarpItem = new JCheckBoxMenuItem("Auto-warp")

  private val mac = new Motherboard
  private val debugger = new Debugger(m68k = mac.m68k, video = mac.video, () => debugMenuItem.setSelected(false), mac.video,mac.via,mac.rtc,mac.iwm,mac.scc,mac.audio,mac.keyboard,mac.scsi,mac.adb)
  private var frame : JFrame = uninitialized
  private var display : Display = uninitialized
  private var model : MacModel = uninitialized
  private val pref = new Preferences
  private val conf = ConfigContext(homeDir = new File(System.getProperty("smacHome","")),pref = pref,conf = new Properties)

  private var romPath : String = uninitialized
  private val scsiDevPaths = Array.fill[String](7)(null) // index 0 is scsi 6, index 1 is scsi 5, etc.
  private val floppyPaths = Array.fill[String](3)(null)
  private var lastDirectory : String = uninitialized
  private var memoryBytes = -1

  private var autoWarp = false

  private var anyBootingError = false
  private var isShuttingDown = false

  private val storagePanel = new StoragePanel

  override def onMessage(msg: MessageBus.Message): Unit =
    msg match
      case MessageBus.FloppyEjected(_,diskName,Some(error)) if !isShuttingDown =>
        JOptionPane.showMessageDialog(frame,s"Cannot flush floppy disk '$diskName': $error","Floppy error",JOptionPane.ERROR_MESSAGE)
      case MessageBus.FloppyMotorOn(_,_,isOn) =>
        if autoWarp then
          warpMode(isOn)
      case _ =>

  private def reset(hard:Boolean): Unit = ???

  private def errorHandler(t: Throwable): Unit =
    t.printStackTrace()
    JOptionPane.showOptionDialog(
      frame,
      s"Unexpected error: $t",
      "Unexpected error",
      JOptionPane.YES_NO_CANCEL_OPTION,
      JOptionPane.ERROR_MESSAGE,
      null,
      Array("Ignore", "Open debugger", "Reset"),
      "Ignore"
    ) match
      case JOptionPane.NO_OPTION =>
        openDebugger(enable = true)
      case JOptionPane.CANCEL_OPTION =>
        mac.masterClock.pause()
        reset(hard = true)
        mac.masterClock.play()
      case _ => // do nothing
  end errorHandler

  private def openDebugger(enable:Boolean): Unit =
    if enable then
      debugger.enableTracing(true)
    debugger.showDebugger(true)

  private def closeDebugger(): Unit =
    debugger.enableTracing(false)
    debugger.showDebugger(false)

  private def boot(): Unit =
    if conf.homeDir.getName.isEmpty then
      println("home directory env variable not set")
      sys.exit(1)

    MessageBus.add(this)

    mac.masterClock.setErrorHandler(errorHandler)
    // main frame
    frame = new JFrame(s"sMac emulator v${Version.VERSION} ($model)")
    frame.setResizable(false)
    frame.setIconImage(new ImageIcon(getClass.getResource("/resources/logo.gif")).getImage)
    frame.addWindowListener(new java.awt.event.WindowAdapter {
      override def windowClosing(e: java.awt.event.WindowEvent): Unit = shutdown()
    })
    // display
    display = new Display(model.videoSettings.horizontalPixels, model.videoSettings.verticalLines, frame.getTitle, frame,mac.masterClock)
    display.setFocusable(true)
    display.addKeyListener(mac.keyboard)
    display.setPreferredSize(new Dimension(model.videoSettings.horizontalPixels * 2, model.videoSettings.verticalLines * 2))
    display.addMouseListener(mac.mouse)
    display.addMouseMotionListener(mac.mouse)
    // DND
    display.setTransferHandler(new DNDHandler((file, copy) => {
      val index = if copy then 1 else 0
      attachFloppy(index,Some(file.toString))
    }))
    mac.video.setDisplay(display)
    frame.getContentPane.add("Center", display)

    val southPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    southPanel.add(storagePanel)
    frame.getContentPane.add("South",southPanel)

    buildMenuBar()
  end boot

  private def configure(args:Array[String]): Unit =
    import Preferences.*
    
    pref.add(ROM_IMAGE, "rom image path", null.asInstanceOf[String]) { romPath =>
      this.romPath = romPath
    }
    for d <- 0 to 6 do
      pref.add(s"$SCSI_DEV$d", s"scsi #$d image path", null.asInstanceOf[String]) { scsiPath =>
        scsiDevPaths(d) = scsiPath
      }
    for d <- 0 to 2 do
      pref.add(s"$FLOPPY$d", s"floppy #$d image path", null.asInstanceOf[String]) { floppyPath =>
        floppyPaths(d) = floppyPath
      }
    pref.add(MEMORY, "set the amount of memory, expressed in Kbytes or Mbytes. Examples: 128k 2M 4M", null.asInstanceOf[String]) { mem =>
      val memory = mem.toUpperCase
      val mul = if memory.endsWith("K") then 1024 else if memory.endsWith("M") then 1024 * 1024 else -1
      if mul != -1 then
        try
          memoryBytes = memory.dropRight(1).toInt * mul
        catch
          case _:NumberFormatException =>
            println(s"Invalid memory setting: $mem")
    }
    pref.add(AUTO_WARP, "enabled automatically warp mode when accessing floppies", false) { aw =>
      autoWarp = aw
      autoWarpItem.setSelected(aw)
    }

    if pref.checkForHelp(args) then
      println(s"sMac emulator ver. ${Version.VERSION} (${Version.BUILD_DATE})")
      pref.printUsage("config file")
      sys.exit(0)

    pref.parseAndLoad(args, conf.conf) match
      case Some(configFile) =>
        try
          var file = new File(configFile)
          if file.getParentFile == null then
            file = new File(new File(conf.homeDir,"config"),configFile)
          val config = java.nio.file.Files.readAllLines(file.toPath)
          import scala.jdk.CollectionConverters.*
          val params = config.asScala.filterNot(_.startsWith("#")).toArray
          pref.parseAndLoad(params, conf.conf) match
            case Some(_) =>
              println("Configuration file cannot contain another configuration file")
              sys.exit(1)
            case None =>
        catch
          case t:Throwable =>
            println(s"Cannot read configuration file $configFile: $t")
            sys.exit(1)
      case None =>

    // debugger's logger
    val log = Logger.setLogger(debugger.log)
    mac.setLogger(log)
    log.setCPU(mac.m68k)

    // check if a rom has been set
    if romPath == null then
      val configDialog = new JDialog(null.asInstanceOf[JFrame],"sMac's Configuration",true)
      val configPanel = new ConfigPanel(configured => configDialog.dispose())
      val screen = Toolkit.getDefaultToolkit.getScreenSize
      configDialog.getContentPane.add("Center",configPanel)
      configDialog.pack()
      val dialogSize = configDialog.getSize
      configDialog.setLocation((screen.width - dialogSize.width) / 2,(screen.height - dialogSize.height) / 2)
      configDialog.setVisible(true)
      if configPanel.isCanceled then
        sys.exit(0)
      // ok, let's configure the system
      val rom = configPanel.getROM

      romPath = rom.file
      model = rom.model
      model.setTotalRamInK(model.ramSizesInK(configPanel.getRAMIndex))
      mac.mmu.setROM(rom.rom)

      log.info("Setting model %s with %s ram Kb",model,memoryBytes)
      for (s,i) <- configPanel.getSCSIPaths.zipWithIndex do
        log.info("Setting SCSI #%d with image %s",6 - i,s)
        scsiDevPaths(6 - i) = s
      for (f,i) <- configPanel.getFloppyPaths.zipWithIndex do
        log.info("Setting floppy #%d with image %s",i,f)
        floppyPaths(i) = f
    else
      ROM.loadROM(romPath) match
        case Right(ROM(file,rom,hash,_model)) =>
          log.info("ROM '%s' loaded [%s]: model %s",file,hash,_model)
          mac.mmu.setROM(rom)
          model = _model
        case Left(err) =>
          println(err)
          sys.exit(1)
      // check memory
      if memoryBytes != -1 then
        model.ramSizesInK.find(_ == memoryBytes) match
          case Some(i) =>
            model.setTotalRamInK(model.ramSizesInK(i))
          case None =>
            println(s"Cannot configure $memoryBytes bytes for model $model: available memory settings are ${model.ramSizesInK.map(m => s"${m}K").mkString(", ")}")
            model.setTotalRamInK(model.ramSizesInK(model.ramSizesInK.length - 1))
      else
        model.setTotalRamInK(model.ramSizesInK(model.ramSizesInK.length - 1))

    // let's configure scsi & floppy
    mac.scsi.setSCSIListener(storagePanel)
    mac.iwm.addDiskControllerListener(storagePanel)
    storagePanel.setDiskette(model.floppySettings.drivesNumber)

    for s <- 6 to 0 by -1 do
      if scsiDevPaths(s) != null then
        try
          val hd = new SCSIHardDrive(s,scsiDevPaths(s))
          mac.scsi.setTarget(hd)
          storagePanel.setSCSI(hd)
        catch
          case t:Throwable =>
            anyBootingError = true
            println(s"Cannot mount scsi drive #$s with image ${scsiDevPaths(s)}: $t")
            log.error("Cannot mount scsi drive #%d with image %s: %s",s,scsiDevPaths(s),t)
    for f <- 0 until model.floppySettings.drivesNumber do
      if floppyPaths(f) != null then
        try
          mac.iwm.insertFloppy(f,new MacDiskImage(floppyPaths(f)))
        catch
          case t:Throwable =>
            anyBootingError = true
            println(s"Cannot mount floppy drive #$f with image ${floppyPaths(f)}: $t")
            log.error("Cannot mount floppy drive #%d with image %s: %s",f,floppyPaths(f),t)
  end configure

  private def run(): Unit =
    val log = Logger.getLogger
    log.info("Building the system ...")

    mac.initComponent()
    mac.m68k.resetComponent()
    mac.setComponentModel(model)
    mac.video.setRAM(mac.mmu.getRAM)

    debugger.setRAM(mac.mmu.getRAM)
    debugger.setROM(mac.mmu.getROM)

    log.setLevel(java.util.logging.Level.SEVERE)

    mac.registerOnBus()

    // patch ram for skipping memory test
    // if skipping memory test is needed
    // TODO add pref variable to keep memory test
    model.skipMemoryTestAddress.foreach { case (address, value, size) =>
      mac.mmu.patchRAM(address, value, size)
    }

    MessageBus.send(MessageBus.Configuration(this,conf))

    SwingUtilities.invokeLater(() => {
      frame.pack()
      frame.setVisible(true)
      if anyBootingError then
        JOptionPane.showMessageDialog(frame,"Some drive mounting has failed, see log error for details","Boot error",JOptionPane.ERROR_MESSAGE)
      mac.audio.start()
      mac.masterClock.start()
    })
  end run

  private def pause(on:Boolean): Unit =
    if on then
      mac.masterClock.pause()
    else
      mac.masterClock.play()
  end pause

  private def shutdown(): Unit =
    isShuttingDown = true
    pause(on = true)
    new Thread() {
      override def run(): Unit =
        val shutdownDialog = new JDialog(frame, "sMac shutting down")
        val items = new JTextArea(10,50)
        items.setEditable(false)
        items.append("Shutting down ...")
        val closeButton = new JButton("Close")
        closeButton.addActionListener(_ => {
          shutdownDialog.dispose()
          sys.exit(1)
        })
        closeButton.setEnabled(false)
        val southPanel = new JPanel
        southPanel.add(closeButton)
        shutdownDialog.getContentPane.add("South", southPanel)
        shutdownDialog.getContentPane.add("Center", new JScrollPane(items))
        shutdownDialog.pack()
        val fSize = if frame.isVisible then frame.getSize else new Dimension(0,0)
        val fLoc = if frame.isVisible then
          frame.getLocationOnScreen
        else
          val center = Toolkit.getDefaultToolkit.getScreenSize
          new Point(center.width / 2,center.height / 2)
        val dSize = shutdownDialog.getSize
        fLoc.x += (fSize.width - dSize.width) / 2
        fLoc.y += (fSize.height - dSize.height) / 2
        shutdownDialog.setLocation(fLoc)
        shutdownDialog.setVisible(true)
        MessageBus.add {
          case MessageBus.ShuttingdownItem(_, item) =>
            items.append(s"\n$item")
          case MessageBus.FloppyEjected(_, diskName, Some(error)) =>
            closeButton.setEnabled(true)
            frame.setVisible(false)
            items.append(s"Error while flushing floppy '$diskName': $error")
          case _ =>
        }
        MessageBus.send(MessageBus.Shutdown(this, conf))

        if !closeButton.isEnabled then
          Thread.sleep(1000)
          sys.exit(0)
      end run
    }.start()
  end shutdown

  private def buildMenuBar(): Unit =
    val menubar = new JMenuBar
    frame.setJMenuBar(menubar)

    val fileMenu = new JMenu("File")
    val stateMenu = new JMenu("State")
    val debugMenu = new JMenu("Debug")
    val toolsMenu = new JMenu("Tools")
    val helpMenu = new JMenu("Help")

    menubar.add(fileMenu)
    menubar.add(stateMenu)
    menubar.add(debugMenu)
    menubar.add(toolsMenu)
    menubar.add(helpMenu)

    buildFileMenu(fileMenu)
    buildDebugMenu(debugMenu)
    buildToolsMenu(toolsMenu)
    buildHelpMenu(helpMenu)
  end buildMenuBar

  private def buildFileMenu(fileMenu:JMenu): Unit =
    for d <- 0 until model.floppySettings.drivesNumber do
      val attachItem = new JMenuItem(s"Attach floppy #$d")
      fileMenu.add(attachItem)
      attachItem.addActionListener(_ => attachFloppy(d,None))

    val resetItem = new JMenuItem("Reset")
    fileMenu.add(resetItem)
    resetItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_R, java.awt.event.InputEvent.ALT_DOWN_MASK))
    resetItem.addActionListener(_ => reset(hard = false))
    val hardResetItem = new JMenuItem("Power off/on")
    fileMenu.add(hardResetItem)
    hardResetItem.addActionListener(_ => reset(hard = true))

    warpItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_W, java.awt.event.InputEvent.ALT_DOWN_MASK))
    fileMenu.add(warpItem)
    warpItem.addActionListener(_ => warpMode(warpItem.isSelected))
    fileMenu.add(autoWarpItem)
    autoWarpItem.addActionListener(_ => autoWarp = autoWarpItem.isSelected)

    val exitItem = new JMenuItem("Exit")
    exitItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_X,java.awt.event.InputEvent.ALT_DOWN_MASK))
    exitItem.addActionListener(_ => shutdown() )
    fileMenu.add(exitItem)
  end buildFileMenu

  private def buildDebugMenu(debugMenu:JMenu): Unit =
    debugMenu.add(debugMenuItem)
    debugMenuItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_D, java.awt.event.InputEvent.ALT_DOWN_MASK))
    debugMenuItem.addActionListener(_ => if debugMenuItem.isSelected then openDebugger(enable = false) else closeDebugger())

  end buildDebugMenu

  private def buildToolsMenu(toolsMenu:JMenu): Unit =
    val switchItem = new JMenuItem("Press debug switch")
    toolsMenu.add(switchItem)
    switchItem.addActionListener(_ => mac.pushSwitch())
    mouseCapItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_M, java.awt.event.InputEvent.ALT_DOWN_MASK))
    toolsMenu.add(mouseCapItem)
    mouseCapItem.addActionListener(_ => mouseCapture(mouseCapItem.isSelected))

    val muteItem = new JCheckBoxMenuItem("Mute volume")
    muteItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_U, java.awt.event.InputEvent.ALT_DOWN_MASK))
    toolsMenu.add(muteItem)
    muteItem.addActionListener(_ =>
      mac.audio.mute(muteItem.isSelected))

    val gifItem = new JMenuItem("GIF recording ...")
    toolsMenu.add(gifItem)
    gifItem.addActionListener(_ => {
      GIFPanel.createGIFPanel(frame,Array(display),Array("Display")).setVisible(true)
    })

    val fullScreenItem = new JMenuItem("Full screen ...")
    toolsMenu.add(fullScreenItem)
    fullScreenItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_ENTER, java.awt.event.InputEvent.ALT_DOWN_MASK))
    fullScreenItem.addActionListener(_ => enableFullScreenMode(true))
  end buildToolsMenu

  private def buildHelpMenu(helpMenu:JMenu): Unit =
    val aboutItem = new JMenuItem("About")
    helpMenu.add(aboutItem)
    aboutItem.addActionListener(_ => AboutPanel.showAboutDialog(frame))
  // ==========================================
  private def zoomFactorChanged(dim:Dimension): Unit =
    val zx = dim.width / model.videoSettings.horizontalPixels
    val zy = dim.height / model.videoSettings.verticalLines
    MessageBus.send(MessageBus.ZoomFactorChanged(this,zx,zy))

  private def mouseCapture(on:Boolean): Unit =
    mouseCapItem.setSelected(on)
    mac.mouse.setCapture(on = on, display)

  private def warpMode(on:Boolean): Unit =
    warpItem.setSelected(on)
    mac.masterClock.setWarpMode(on)
    MessageBus.send(MessageBus.WarpMode(this, on))

  private def attachFloppy(drive:Int,floppy:Option[String]): Unit =
    val file = floppy match
      case Some(f) => f
      case None =>
        val fc = new JFileChooser()
        if lastDirectory != null then
          fc.setCurrentDirectory(new File(lastDirectory))
        fc.showOpenDialog(frame) match
          case JFileChooser.APPROVE_OPTION =>
            lastDirectory = fc.getCurrentDirectory.toString
            fc.getSelectedFile.toString
          case _ =>
            return

    try
      val floppy = new MacDiskImage(file)
      if !mac.iwm.insertFloppy(drive,floppy) then
        JOptionPane.showMessageDialog(frame,s"Drive #$drive is not empty: eject the floppy first","Error while mounting floppy",JOptionPane.ERROR_MESSAGE)
    catch
      case t:Throwable =>
        JOptionPane.showMessageDialog(frame,s"Invalid floppy format: ${t.getMessage}","Error while mounting floppy",JOptionPane.ERROR_MESSAGE)
  end attachFloppy

  private def enableFullScreenMode(selectScreen: Boolean): Unit =
    var selectedScreenIndex = 0
    val devices = FullScreenMode.getScreenDeviceIDs
    if devices.length > 1 then
      pause(on = true)
      try
        JOptionPane.showInputDialog(frame, "Select screen", "Screen selection for full screen mode", JOptionPane.INFORMATION_MESSAGE, null, devices.asInstanceOf[Array[Object]], devices(0)) match
          case null =>
            return
          case sel =>
            val index = devices.indexOf(sel.toString)
            if index != -1 then
              selectedScreenIndex = index
      finally
        pause(on = false)

    FullScreenMode.goFullScreen(selectedScreenIndex, frame, display, model.videoSettings.horizontalPixels, model.videoSettings.verticalLines) match
      case Some(w) =>
        zoomFactorChanged(w.newSize)
        display.addKeyListener(new KeyAdapter:
          override def keyPressed(e: KeyEvent): Unit =
            import KeyEvent.*
            e.getKeyCode match
              case VK_ESCAPE =>
                mouseCapture(on = false)
                FullScreenMode.restore(w)
                zoomFactorChanged(w.originalSize)
                display.removeKeyListener(this)
              case VK_R =>
                reset(hard = false)
              case VK_W =>
                warpMode(!warpItem.isSelected)
              case _ =>
        )
        mouseCapture(on = true)
      case None =>
  end enableFullScreenMode