package ucesoft.mac.debugger

import org.fife.ui.rsyntaxtextarea.{RSyntaxTextArea, SyntaxConstants}
import org.fife.ui.rtextarea.RTextScrollPane
import ucesoft.mac.{Logger, MACComponent}
import ucesoft.mac.cpu.m68k.*
import ucesoft.mac.cpu.m68k.RegisterType.PC
import ucesoft.mac.debugger.DebuggerUI.*
import ucesoft.mac.ui.MessageBoard
import ucesoft.mac.ui.MessageBoard.MessageBoardListener
import ucesoft.mac.video.MacVideo
import ucesoft.mac.video.MacVideo.VideoSignalListener

import java.awt.event.{MouseAdapter, MouseEvent, WindowAdapter, WindowEvent}
import java.awt.{BorderLayout, Dimension, FlowLayout, GridLayout}
import javax.swing.text.DefaultCaret
import javax.swing.*
import scala.compiletime.uninitialized

object Debugger:
  sealed trait BreakType

  case class AddressBreakType(address: Int, execute: Boolean = false, read: Boolean = false, write: Boolean = false, var enabled: Boolean = true) extends BreakType:
    override def toString: String =
      val sb = new StringBuilder()
      if read then sb += 'R'
      if write then sb += 'W'
      if execute then sb += 'E'
      sb.toString()

  case object ResetBreak extends BreakType
  case object HaltBreak extends BreakType
  case object StopBreak extends BreakType
  case class ExceptionBreak(number: Int) extends BreakType
  case class InterruptBreak(number: Int, label: String = "") extends BreakType

  trait BreakListener:
    def addBreak(break: AddressBreakType): Unit
    def removeBreak(address: Int): Unit

  trait DisassemblerBreakHandler:
    protected var breakListener: List[BreakListener] = Nil

    def addBreakListener(l: BreakListener): Unit =
      breakListener = l :: breakListener
    def hasBreakAt(address: Int): Boolean
    def addExecuteBreakAt(address: Int): Unit = addBreakAt(address, execute = true)
    def addBreakAt(address: Int, read: Boolean = false, write: Boolean = false, execute: Boolean = false): Unit
    def removeBreakAt(address: Int): Unit
    def getBreakStringAt(address: Int): Option[String]
    def getBreakEvent(eventName: String): Option[AnyRef]
    def addBreakEvent(eventName: String, value: AnyRef): Unit
    def removeBreakEvent(eventName: String): Unit

    protected def notifyBreakAdded(b: AddressBreakType): Unit =
      for l <- breakListener do
        l.addBreak(b)

    protected def notifyBreakRemoved(address: Int): Unit =
      for l <- breakListener do
        l.removeBreak(address)
/**
 * @author Alessandro Abbruzzetti
 *         Created on 19/11/2024 15:12  
 */
class Debugger(m68k:M68000,
               video:MacVideo,
               windowCloseOperation: () => Unit,
               componentsToObserve:MACComponent*) extends VideoSignalListener:
  import Debugger.*
  private inline val MAX_LOG_LINES = 1000
  private inline val DIS_LINES = 25
  private enum StepState:
    case NoStep, WaitReturn, WaitTarget

  private abstract class InternalDebugger extends JPanel:
    protected var stepOverPending, stepOutPending = StepState.NoStep
    protected var stepOverOutStopPending = false
    protected var stepOverTargetAddress = 0
    protected val semaphore = new Object//new Semaphore(0)
    protected val cpuEnabled = {
      val item = new JCheckBox("CPU enabled")
      item.setSelected(true)
      item.addActionListener(_ => onCPUEnabled(item.isSelected))
      item
    }
    protected val busAvailable = {
      val bus = new JCheckBox("BUS available")
      bus.setEnabled(false)
      bus
    }
    protected var tracingOnFile = false
    protected var tracingListener: TraceListener = scala.compiletime.uninitialized

    protected def onCPUEnabled(enabled:Boolean): Unit = {}

    def startTracingOnFile(tracingListener: TraceListener): Unit =
      this.tracingListener = tracingListener
      tracingOnFile = true
    def stopTracingOnFile(): Unit =
      tracingOnFile = false

    def enableTracing(enabled: Boolean): Unit
    def stepIn(): Unit
    def stepOver(): Unit
    def stepOut(): Unit
    def updateModels(): Unit
  end InternalDebugger

  private val frame = new JFrame("Debugger")
  private val componentToObserveModels = for c <- componentsToObserve yield new PropertiesTableModel(c,frame)
  private val logPanel = new RSyntaxTextArea(10,100)
  private val onOffButton = new JToggleButton(new ImageIcon(getClass.getResource("/resources/trace/on.png")))

  private var frameByFrameMode = false
  private val frameByFrameLock = new Object
  private var frameByFrameCond = false
  private var frameCount = 0

  private val m68kramMemoryDumpItem = new JCheckBoxMenuItem("68k RAM")
  private var m68KramDialog : JDialog = uninitialized
  private val romDumpItem = new JCheckBoxMenuItem("ROM")
  private var romDialog: JDialog = uninitialized
  private val m68KDisassemblerItem = new JCheckBoxMenuItem("M68K Disassembler")

  private val m68kDebugger = new M68KDebugger
  private val m68kDisassemblerPanel = new DisassemblerPanel("M68K",
    (model,a) => {
      val dis = m68k.disassemble(a)
      model.add(dis, false)
      a + dis.size
    },
    frame,
    m68kDebugger,
    () => m68KDisassemblerItem.setSelected(false))
  private val m68kDisassemblerDialog = m68kDisassemblerPanel.dialog

  private var selectedDebugger : InternalDebugger = m68kDebugger

  private val m68kBreakItem = new JCheckBoxMenuItem("68k breaks")
  private val m68kBreakDialog = new BreakMasterPanel(
    "M68K",
    frame,
    6,
    break => m68kDebugger.removeBreakAt(break.address),
    break => m68kDebugger.addBreakAt(break.address,read = break.read,write = break.write,execute = break.execute),
    new M68KBreakEventPanel(m68kDebugger),
    m68kDebugger,
    () => m68kBreakItem.setSelected(false)
  ).dialog

  private var messageBoard: MessageBoardListener = uninitialized
  private val tabbedPane = new JTabbedPane()
  private var logLines = 0

  // =============================================================================================================
  private trait GenericDebugger extends DisassemblerBreakHandler:
    def nextStep(): Unit
    def updateDisassembly(): Unit
    def isTracing: Boolean

  private class M68KDebugger extends InternalDebugger with GenericDebugger:
    private var stepInstruction : Instruction = uninitialized
    private var stepDisassemble : DisassembledInstruction = uninitialized
    private val dataRegisterTableModel = new M68KRegisterTableModel(m68k, data = true)
    private val addressRegisterTableModel = new M68KRegisterTableModel(m68k, data = false)
    private val statusRegisterTableModel = new M68KStatusRegisterTableModel(m68k)
    private val pcRegisterTableModel = new M68KPCTableModel(m68k)
    private val disassembledTableModel = new DisassembledTableModel(address => getBreakStringAt(address))
    private val componentPanel = new JTabbedPane()
    private val distable = new JTable(disassembledTableModel)
    private val commentedROMPanel = new CommentedROMPanel
    private var commentedROMEnabled = false
    private var commentedROMSplitPane : JSplitPane = uninitialized

    def enableCommentedROMPanel(enabled:Boolean): Unit =
      commentedROMEnabled = enabled
      if !enabled then
        commentedROMSplitPane.setRightComponent(null)
        commentedROMPanel.clear()
      else
        commentedROMSplitPane.setRightComponent(commentedROMPanel)
        commentedROMSplitPane.setDividerLocation(0.5)

    def commentedROMGotoAddress(address:Int): Unit = commentedROMPanel.gotoAddress(address)
    def commentedROMLoad(file:String): Unit =
      commentedROMPanel.load(file,m68k.getRegister(PC).get())

    override def startTracingOnFile(tracingListener: TraceListener): Unit =
      super.startTracingOnFile(tracingListener)
      debugger.setStepAlways(true)
      m68k.addEventListener(debugger)
      debugger.nextStep()

    override def stopTracingOnFile(): Unit =
      super.stopTracingOnFile()
      if !debugger.existsBreakPending then
        m68k.removeEventListener(debugger)
      debugger.setStepAlways(false)

    override protected def onCPUEnabled(enabled:Boolean): Unit =
      m68k.setComponentEnabled(enabled)
    override def getBreakEvent(eventName: String): Option[AnyRef] = debugger.getBreakEvent(eventName)
    override def addBreakEvent(eventName: String, value: AnyRef): Unit =
      debugger.addBreakEvent(eventName, value)

    override def removeBreakEvent(eventName: String): Unit =
      debugger.removeBreakEvent(eventName)
      if !debugger.existsBreakPending then
        enableTracing(false)
    override def hasBreakAt(address: Int): Boolean = debugger.hasBreakAt(address)
    override def addBreakAt(address:Int,read:Boolean,write:Boolean,execute:Boolean): Unit =
      debugger.addBreakAt(address,read,write,execute)
      notifyBreakAdded(AddressBreakType(address,read = read,write = write, execute = execute))
      disassembledTableModel.update()
      m68k.addEventListener(debugger)
    override def removeBreakAt(address: Int): Unit =
      debugger.removeBreakAt(address)
      notifyBreakRemoved(address)
      disassembledTableModel.update()
      if !debugger.existsBreakPending then
        m68k.removeEventListener(debugger)
    override def getBreakStringAt(address: Int): Option[String] = debugger.getBreakStringAt(address)

    override def isTracing: Boolean = debugger.isTracing

    private val debugger = new AbstractDebugger with GenericDebugger {
      override def getBreakEvent(eventName: String): Option[AnyRef] =
        eventName match
          case "reset" => if isBreakOnReset then Some(java.lang.Boolean.TRUE) else None
          case "halt" => if isBreakOnHalt then Some(java.lang.Boolean.TRUE) else None
          case "stop" => if isBreakOnStop then Some(java.lang.Boolean.TRUE) else None
          case "interrupt" =>
            val interrupt = getBreakOnInterruptLevel
            if interrupt == -1 then None else Some(Integer.valueOf(interrupt))
          case "exception" =>
            val ex = getBreakOnExceptionNumber
            if ex == -1 then None else Some(Integer.valueOf(ex))
          case _ => None
      override def addBreakEvent(eventName: String, value: AnyRef): Unit =
        m68k.addEventListener(this)
        eventName match
          case "reset" => setBreakOnReset(true)
          case "halt" => setBreakOnHalt(true)
          case "stop" => setBreakOnStop(true)
          case "interrupt" => setBreakOnInterrupt(value.asInstanceOf[Integer].intValue())
          case "exception" => setBreakOnExceptionNumber(value.asInstanceOf[Integer].intValue())
      override def removeBreakEvent(eventName: String): Unit =
        if !existsBreakPending then
          m68k.removeEventListener(this)
        eventName match
          case "reset" => setBreakOnReset(false)
          case "halt" => setBreakOnHalt(false)
          case "stop" => setBreakOnStop(false)
          case "interrupt" => setBreakOnInterrupt(-1)
          case "exception" => setBreakOnExceptionNumber(-1)
      override def hasBreakAt(address: Int): Boolean = m68kAddressBreaks.contains(address)
      override def addBreakAt(address:Int,r:Boolean,w:Boolean,e:Boolean): Unit =
        m68kAddressBreaks += address -> AddressBreak(BreakType(execute = e,read = r, write = w), address)
      override def removeBreakAt(address: Int): Unit = m68kAddressBreaks -= address
      override def getBreakStringAt(address: Int): Option[String] = m68kAddressBreaks.get(address).map(_.breakType.toString)

      override def nextStep(): Unit =
        //semaphore.release()
        semaphore.synchronized {
          semaphore.notify()
        }

      override def isTracing: Boolean = isStepByStep

      override def breakEpilogue(cpu: M6800X0): Unit = semaphore.synchronized {
        semaphore.wait()
      }//semaphore.acquire()

      override def onStepByStepChange(stepByStepEnabled: Boolean): Unit =
        if stepByStepEnabled then
          m68k.addEventListener(this)
          checkTracingState(true)
        else
          nextStep()
          if !existsBreakPending then
            m68k.removeEventListener(this)

      override protected def onInterrupted(cpu: M6800X0, level: Int): Unit =
        if !tracingOnFile then
          log(s"Break on 68K interrupt $level")
          stepOutPending = StepState.NoStep
          stepOverPending = StepState.NoStep
          checkTracingState(true)
          updateDisassembly(cpu)
          updateModels()
          breakEpilogue(cpu)
      override protected def onException(cpu: M6800X0, level: Int): Unit =
        if !tracingOnFile then
          log(s"Break on 68K exception $level")
          stepOutPending = StepState.NoStep
          stepOverPending = StepState.NoStep
          checkTracingState(true)
          updateDisassembly(cpu)
          updateModels()
          breakEpilogue(cpu)
      override protected def onReset(cpu: M6800X0): Unit =
        if !tracingOnFile then
          log(s"Break on 68K RESET")
          stepOutPending = StepState.NoStep
          stepOverPending = StepState.NoStep
          checkTracingState(true)
          updateDisassembly(cpu)
          updateModels()
          breakEpilogue(cpu)
      override protected def onHalt(cpu: M6800X0): Unit =
        if !tracingOnFile then
          log(s"Break on 68K HALT")
          stepOutPending = StepState.NoStep
          stepOverPending = StepState.NoStep
          checkTracingState(true)
          updateDisassembly(cpu)
          updateModels()
          breakEpilogue(cpu)
      override protected def onStop(cpu: M6800X0): Unit =
        if !tracingOnFile then
          log(s"Break on 68K STOP")
          stepOutPending = StepState.NoStep
          stepOverPending = StepState.NoStep
          checkTracingState(true)
          updateDisassembly(cpu)
          updateModels()
          breakEpilogue(cpu)
      override protected def onRw(cpu: M6800X0, address: Int, size: Size, read: Boolean, value: Int): Unit =
        if !tracingOnFile then
          log(s"Break on M68K ${if read then "READ" else "WRITE"} with size $size${if read then s"value=$value" else ""}")
          stepOutPending = StepState.NoStep
          stepOverPending = StepState.NoStep
          checkTracingState(true)
          updateDisassembly(cpu)
          updateModels()
          breakEpilogue(cpu)

      override protected def onFetch(cpu: M6800X0, address: Int, opcode: Int, i: Instruction, busNotAvailable: Boolean, wasBreak: Boolean): Unit =
        if tracingOnFile then
          tracingListener.onTrace(cpu.disassemble(address).toString,address)
          return

        if stepOverOutStopPending then
          stepOverOutStopPending = false
          stepOutPending = StepState.NoStep
          setStepAlways(false)

        stepInstruction = i
        checkStepOverOut(i, address)

        if wasBreak then
          val break = m68kAddressBreaks(address)
          log(break.toString)
          stepOutPending = StepState.NoStep
          stepOverPending = StepState.NoStep

        if !isStepAlways then
          checkTracingState(true)
          disassembledTableModel.clear()
          updateDisassembly(cpu,address)
          updateModels()
          breakEpilogue(cpu)
        end if
      end onFetch

      override def updateDisassembly(): Unit =
        //disassembledTableModel.clear()
        updateDisassembly(m68k)
        updateModels()

      private def updateDisassembly(cpu: M6800X0,address:Int = -1): Unit =
        import ucesoft.mac.cpu.m68k.RegisterType.PC
        if commentedROMEnabled then
          commentedROMPanel.gotoAddress(address)
        disassembledTableModel.clear()
        val startAddress = if address == -1 then cpu.getRegister(PC).get() else address
        swing {
          var adr = startAddress
          for a <- 1 to DIS_LINES do
            val dis = cpu.disassemble(adr)
            if a == 1 then
              stepDisassemble = dis
            disassembledTableModel.add(dis, false)
            adr += dis.size
          disassembledTableModel.update()
        }

      private def checkStepOverOut(instruction: Instruction, address: Int): Unit =
        import InstructionType.*
        import StepState.*

        stepOverPending match
          case WaitTarget =>
            if address == stepOverTargetAddress then
              stepOutPending = StepState.NoStep
              setStepAlways(false)
          case _ =>
        stepOutPending match
          case NoStep =>
          case WaitReturn =>
            instruction.instructionType match
              case RTR | RTE | RTS =>
                stepOverOutStopPending = true
              case _ =>
          case _ =>
    }

    override def updateDisassembly(): Unit = debugger.updateDisassembly()
    override def nextStep(): Unit = debugger.nextStep()

    override def enableTracing(enabled: Boolean): Unit =
      debugger.setStepByStep(enabled)

      if !enabled then
        debugger.setStepAlways(false)
        debugger.setStepByStep(false)

      stepOutPending = StepState.NoStep
      stepOverPending = StepState.NoStep

    override def stepIn(): Unit =
      debugger.nextStep()

    override def stepOver(): Unit =
      stepOverTargetAddress = stepDisassemble.address + stepDisassemble.size
      stepOverPending = StepState.WaitTarget
      debugger.setStepAlways(true)

      debugger.nextStep()

    override def stepOut(): Unit =
      import InstructionType.*
      import StepState.*
      stepInstruction.instructionType match
        case RTR | RTE | RTS =>
          stepOutPending = NoStep
        case _ =>
          stepOutPending = WaitReturn
          debugger.setStepAlways(true)
      debugger.nextStep()


    init()

    private def init(): Unit =
      setLayout(new BorderLayout())
      val northPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
      northPanel.add(cpuEnabled)
      northPanel.add(busAvailable)
      add("North", northPanel)
      val rightPanel = new JPanel(new BorderLayout())
      // registers
      val registerPanel = new JPanel(new GridLayout(0, 1))
      // sr
      val srtable = new JTable(statusRegisterTableModel)
      srtable.getTableHeader.setReorderingAllowed(false)
      srtable.setDefaultRenderer(classOf[java.lang.Boolean], new StatusRegisterRenderer)
      srtable.setDefaultRenderer(classOf[String], new RegisterRenderer("%01X"))
      var sp = new JScrollPane(srtable, ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)
      srtable.setPreferredScrollableViewportSize(srtable.getPreferredSize)
      sp.setBorder(BorderFactory.createTitledBorder("Status register"))
      registerPanel.add(sp)
      // data
      val datatable = new JTable(dataRegisterTableModel)
      datatable.getTableHeader.setReorderingAllowed(false)
      datatable.setDefaultRenderer(classOf[String], RegisterRenderer("%08X"))
      sp = new JScrollPane(datatable, ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)
      datatable.setPreferredScrollableViewportSize(datatable.getPreferredSize)
      sp.setBorder(BorderFactory.createTitledBorder("Data registers"))
      registerPanel.add(sp)
      // address
      val adrtable = new JTable(addressRegisterTableModel)
      adrtable.getTableHeader.setReorderingAllowed(false)
      adrtable.setDefaultRenderer(classOf[String], new RegisterRenderer("%08X"))
      sp = new JScrollPane(adrtable, ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)
      adrtable.setPreferredScrollableViewportSize(adrtable.getPreferredSize)
      sp.setBorder(BorderFactory.createTitledBorder("Address registers"))
      registerPanel.add(sp)
      // misc.
      val pctable = new JTable(pcRegisterTableModel)
      pctable.getTableHeader.setReorderingAllowed(false)
      pctable.setDefaultRenderer(classOf[String], new RegisterRenderer("%08X"))
      pctable.setDefaultRenderer(classOf[java.lang.Integer], new RegisterRenderer("%s"))
      sp = new JScrollPane(pctable, ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)
      pctable.setPreferredScrollableViewportSize(pctable.getPreferredSize)
      sp.setBorder(BorderFactory.createTitledBorder("Misc."))
      registerPanel.add(sp)
      // components
      for tm <- componentToObserveModels do
        val table = new JTable(tm)
        table.getTableHeader.setReorderingAllowed(false)
        table.setDefaultRenderer(classOf[Prop], new PropertiesCellRenderer(tm))
        sp = new JScrollPane(table)
        table.setPreferredScrollableViewportSize(new Dimension(0, 200))
        val compColModel = table.getColumnModel
        compColModel.getColumn(0).setMinWidth(80)
        compColModel.getColumn(0).setMaxWidth(150)
        compColModel.getColumn(1).setMinWidth(150)
        compColModel.getColumn(1).setMaxWidth(300)

        componentPanel.addTab(tm.comp.getComponentName, tm.comp.getIcon.orNull,sp)

      rightPanel.add("Center", componentPanel)
      rightPanel.add("North", registerPanel)

      // disassemble panel
      distable.getTableHeader.setReorderingAllowed(false)
      distable.setDefaultRenderer(classOf[String], new DisassembledCellRenderer)
      sp = new JScrollPane(distable)
      sp.setBorder(BorderFactory.createTitledBorder("Disassembler"))
      val colModel = distable.getColumnModel
      colModel.getColumn(0).setMinWidth(25)
      colModel.getColumn(0).setMaxWidth(30)
      colModel.getColumn(1).setMinWidth(70)
      colModel.getColumn(1).setMaxWidth(80)
      colModel.getColumn(2).setMinWidth(180)
      colModel.getColumn(2).setMaxWidth(200)
      colModel.getColumn(3).setMinWidth(220)
      colModel.getColumn(3).setMaxWidth(250)
      distable.addMouseListener(new MouseAdapter {
        override def mouseClicked(e: MouseEvent): Unit =
          if e.getClickCount == 2 then
            val row = distable.rowAtPoint(e.getPoint)
            val address = disassembledTableModel.getAddressAt(row)
            if hasBreakAt(address) then
              removeBreakAt(address)
            else
              addExecuteBreakAt(address)
            disassembledTableModel.update()
      })

      sp.setMinimumSize(new Dimension(500, 0))
      commentedROMSplitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, rightPanel, null)
      commentedROMSplitPane.setContinuousLayout(true)
      commentedROMSplitPane.setOneTouchExpandable(true)
      val splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, sp, commentedROMSplitPane)
      splitPane.setContinuousLayout(true)
      splitPane.setOneTouchExpandable(true)
      add("Center", splitPane)
    end init

    override def updateModels(): Unit = swing {
      dataRegisterTableModel.contentUpdated()
      addressRegisterTableModel.contentUpdated()
      statusRegisterTableModel.contentUpdated()
      pcRegisterTableModel.contentUpdated()
      disassembledTableModel.update()
      for tm <- componentToObserveModels do tm.update()
      distable.setRowSelectionInterval(0, 0)
      busAvailable.setSelected(m68k.isBUSAvailable)
    }

  end M68KDebugger

  // ==================================================================================================
  
  def setRAM(m68kRAM:Array[Int]): Unit =
    m68KramDialog = new MemoryDumper(m68kRAM, 0x000000, "68K RAM", frame, () => m68kramMemoryDumpItem.setSelected(false), setPreferredScrollableViewportSize = false, showASCII = true).dialog

  def setMessageBoard(mb:MessageBoardListener): Unit =
    messageBoard = mb

  swing {
    init()
  }

  private def swing(f: => Unit) : Unit =
    if !SwingUtilities.isEventDispatchThread then
      SwingUtilities.invokeLater(() => f)
    else f

  private def init(): Unit =
    frame.addWindowListener(new WindowAdapter:
      override def windowClosing(e: WindowEvent): Unit =
        windowCloseOperation()
    )
    frame.setIconImage(new ImageIcon(getClass.getResource("/resources/logo.gif")).getImage)
    val mainPanel = new JPanel(new BorderLayout())
    mainPanel.setPreferredSize(new Dimension(1000,400))
    val northPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    mainPanel.add("North",northPanel)

    val toolBar = new JToolBar("Tracer")
    toolBar.setRollover(true)
    northPanel.add(toolBar)

    // buttons
    onOffButton.setToolTipText("Enable tracing")
    onOffButton.addActionListener(_ => enableTracing(onOffButton.isSelected))

    val stepInButton = new JButton(new ImageIcon(getClass.getResource("/resources/trace/down.png")))
    stepInButton.setToolTipText("Step in")
    stepInButton.addActionListener(_ => stepIn() )
    val stepOverButton = new JButton(new ImageIcon(getClass.getResource("/resources/trace/down_left.png")))
    stepOverButton.addActionListener(_ => stepOver())
    stepOverButton.setToolTipText("Step over")
    val stepOutButton = new JButton(new ImageIcon(getClass.getResource("/resources/trace/up.png")))
    stepOutButton.addActionListener(_ => stepOut())
    stepOutButton.setToolTipText("Step out")

    val enableButtons = (enabled:Boolean) => {
      onOffButton.setEnabled(enabled)
      stepInButton.setEnabled(enabled)
      stepOverButton.setEnabled(enabled)
      stepOutButton.setEnabled(enabled)
    }

    val disaButton = new JButton(new ImageIcon(getClass.getResource("/resources/trace/bug.png")))
    disaButton.addActionListener(_ => disassembleGUI())
    disaButton.setToolTipText("Disassemble")

    val writeButton = new JButton(new ImageIcon(getClass.getResource("/resources/trace/write.png")))
    writeButton.addActionListener(_ => memoryGUI())
    writeButton.setToolTipText("Memory")

    val nextFrame = new JToggleButton(new ImageIcon(getClass.getResource("/resources/trace/nextFrame.png")))
    nextFrame.addActionListener(_ => advanceByOneFrame())
    nextFrame.setToolTipText("Advance by one frame")
    nextFrame.setEnabled(false)

    val frameByFrameMode = new JToggleButton(new ImageIcon(getClass.getResource("/resources/trace/frameByFrameMode.png")))
    frameByFrameMode.addActionListener(_ => {
      nextFrame.setEnabled(frameByFrameMode.isSelected)
      setFrameByFrameMode(frameByFrameMode.isSelected)
    })
    frameByFrameMode.setToolTipText("Frame by frame mode")

    val breakPoint = new JButton(new ImageIcon(getClass.getResource("/resources/trace/red_breakpoint.png")))
    breakPoint.addActionListener(_ => breakGUI())
    breakPoint.setToolTipText("Breakpoints")

    val saveTrace = new JButton(new ImageIcon(getClass.getResource("/resources/trace/save.png")))
    saveTrace.addActionListener(_ => saveTraceUI())
    saveTrace.setToolTipText("Save live disassembly on file")

    val commentedROMButton = new JToggleButton(new ImageIcon(getClass.getResource("/resources/trace/rom.png")))
    commentedROMButton.setToolTipText("Enable/disable commented rom")
    commentedROMButton.addActionListener(_ => if enableCommentedROMPanel(commentedROMButton.isSelected) then commentedROMButton.setSelected(false))

    val jumpButton = new JButton("Jump")
    jumpButton.addActionListener(_ => jumpToAddress())

    toolBar.add(onOffButton)
    toolBar.add(stepInButton)
    toolBar.add(stepOverButton)
    toolBar.add(stepOutButton)
    toolBar.add(disaButton)
    toolBar.add(writeButton)
    toolBar.add(frameByFrameMode)
    toolBar.add(nextFrame)
    toolBar.add(breakPoint)
    toolBar.add(saveTrace)
    toolBar.add(commentedROMButton)
    toolBar.add(jumpButton)

    // log panel
    logPanel.setEditable(false)
    logPanel.setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_NONE)
    logPanel.getCaret.asInstanceOf[DefaultCaret].setUpdatePolicy(DefaultCaret.ALWAYS_UPDATE)
    val lscroll = new RTextScrollPane(logPanel)
    lscroll.setMinimumSize(new Dimension(0, 70))
    lscroll.setPreferredSize(new Dimension(0, 150))
    lscroll.setBorder(BorderFactory.createTitledBorder("Log panel"))


    val logButtonPanel = new JPanel(new BorderLayout())
    logButtonPanel.add("Center", lscroll)
    val logToolBar = new JPanel(new FlowLayout(FlowLayout.LEFT))
    logButtonPanel.add("South", logToolBar)
    val clearLog = new JButton(new ImageIcon(getClass.getResource("/resources/trace/clear.png")))
    clearLog.setToolTipText("Clear log panel")
    logToolBar.add(clearLog)
    val logSeverityGroup = new ButtonGroup
    val logSeverityInfoButton = new JToggleButton(new ImageIcon(getClass.getResource("/resources/trace/log_info.png")))
    logSeverityInfoButton.setToolTipText("Set log level to INFO")
    val logSeverityWarningButton = new JToggleButton(new ImageIcon(getClass.getResource("/resources/trace/log_warning.png")))
    logSeverityWarningButton.setToolTipText("Set log level to WARNING")
    val logSeverityOffButton = new JToggleButton(new ImageIcon(getClass.getResource("/resources/trace/log_off.png")))
    logSeverityOffButton.setToolTipText("Set log level to OFF, log disabled")
    logSeverityGroup.add(logSeverityInfoButton)
    logSeverityGroup.add(logSeverityWarningButton)
    logSeverityGroup.add(logSeverityOffButton)
    val logSeverityPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    logSeverityPanel.add(logSeverityInfoButton)
    logSeverityPanel.add(logSeverityWarningButton)
    logSeverityPanel.add(logSeverityOffButton)
    logToolBar.add(logSeverityPanel)
    logSeverityOffButton.setSelected(true)
    logSeverityInfoButton.addActionListener(_ => {
      Logger.getLogger.setLevel(java.util.logging.Level.INFO)
    })
    logSeverityWarningButton.addActionListener(_ => {
      Logger.getLogger.setLevel(java.util.logging.Level.WARNING)
    })
    logSeverityOffButton.addActionListener(_ => {
      Logger.getLogger.setLevel(java.util.logging.Level.OFF)
    })
    clearLog.addActionListener(_ => {
      logLines = 0
      logPanel.setText("")
    })

    tabbedPane.addTab("M68K",new ImageIcon(getClass.getResource("/resources/trace/cpu.png")),m68kDebugger)
    tabbedPane.addChangeListener(e => {
      tabbedPane.getSelectedIndex match
        case 0 =>
          selectedDebugger = m68kDebugger
          enableButtons(true)
    })
    mainPanel.add("Center",tabbedPane)

    val splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, mainPanel, logButtonPanel)
    splitPane.setOneTouchExpandable(true)

    // menu
    val menu = new JMenuBar
    val memoryMenu = new JMenu("Memory")
    val disMenu = new JMenu("Disassembler")
    val breakMenu = new JMenu("Breaks")

    m68kramMemoryDumpItem.addActionListener(_ => m68KramDialog.setVisible(m68kramMemoryDumpItem.isSelected) )
    romDumpItem.addActionListener(_ => romDialog.setVisible(romDumpItem.isSelected) )
    memoryMenu.add(romDumpItem)
    memoryMenu.add(m68kramMemoryDumpItem)
    menu.add(memoryMenu)

    m68KDisassemblerItem.addActionListener(_ => m68kDisassemblerDialog.setVisible(m68KDisassemblerItem.isSelected))
    disMenu.add(m68KDisassemblerItem)

    menu.add(disMenu)

    m68kBreakItem.addActionListener(_ => m68kBreakDialog.setVisible(m68kBreakItem.isSelected))

    breakMenu.add(m68kBreakItem)

    menu.add(breakMenu)

    frame.setJMenuBar(menu)

    frame.getContentPane.add("Center",splitPane)
    frame.pack()
  end init

  def enableTracing(enabled: Boolean): Unit =
    selectedDebugger.enableTracing(enabled)
    checkTracingState(enabled)
    if !enabled then
      windowCloseOperation()

  protected def selectDebugger(index:Int): Unit =
    swing {
      tabbedPane.setSelectedIndex(index)
      selectedDebugger = index match
        case 0 => m68kDebugger
    }

  def showDebugger(show:Boolean): Unit =
    frame.setVisible(show)

  private def checkTracingState(enabled: Boolean): Unit =
    if enabled then
      onOffButton.setToolTipText("Disable tracing")
      if !frame.isVisible then
        frame.setVisible(true)
    else
      onOffButton.setToolTipText("Enable tracing")
      m68kDebugger.enableTracing(false)
      m68kDebugger.nextStep()
    onOffButton.setSelected(enabled)

  private def stepIn(): Unit =
    selectedDebugger.stepIn()

  private def stepOver(): Unit =
    selectedDebugger.stepOver()

  private def stepOut(): Unit =
    selectedDebugger.stepOut()

  private def disassembleGUI(): Unit =
    tabbedPane.getSelectedIndex match
      case 0 =>
        m68KDisassemblerItem.setSelected(true)
        m68kDisassemblerDialog.setVisible(true)

  private def memoryGUI(): Unit =
    m68kramMemoryDumpItem.setSelected(true)
    m68KramDialog.setVisible(true)

  def log(msg: String): Unit = swing {
    if logLines == MAX_LOG_LINES then
      logPanel.append("Hey, too many logs here, please clear this panel to keep reading new logs")
      logLines += 1
    else if logLines < MAX_LOG_LINES then
      logPanel.append(msg)
      logPanel.append("\n")
      logLines += 1
  }

  private def checkVBlankState(): Unit =
    if frameByFrameMode then
      video.addVideoSignalListener(this)
    else
      video.removeVideoSignalListener(this)
      advanceByOneFrame()

  override def onVBlank(on:Boolean): Unit =
    if frameByFrameMode && on then
      frameByFrameLock.synchronized {
        while frameByFrameCond do
          frameByFrameLock.wait(1000)
      }
      frameByFrameCond = true

  override def onHBlank(on: Boolean, vblank: Boolean, videoLine: Int): Unit = {}

  private def setFrameByFrameMode(on:Boolean): Unit =
    frameByFrameMode = on
    frameByFrameCond = on
    frameCount = 0
    checkVBlankState()
  private def advanceByOneFrame(): Unit =
    frameByFrameLock.synchronized {
      frameByFrameCond = false
      frameByFrameLock.notify()
    }
    frameCount += 1
    if messageBoard != null then
      messageBoard.addMessage(MessageBoard.builder.message(s"$frameCount  ").ytop().xright().delay(500).fadingMilliseconds(100).adminLevel().build())

  private def breakGUI(): Unit =
    tabbedPane.getSelectedIndex match
      case 0 =>
        m68kBreakItem.setSelected(true)
        m68kBreakDialog.setVisible(true)

  private def saveTraceUI(): Unit =
    val std = new SaveTraceDialog(frame,tl => selectedDebugger.startTracingOnFile(tl),() => selectedDebugger.stopTracingOnFile())
    std.setLocationRelativeTo(frame)
    std.setVisible(true)

  private def enableCommentedROMPanel(enabled:Boolean): Boolean =
    if enabled then
      val fc = new JFileChooser("Load commented rom")
      fc.showOpenDialog(frame) match
        case JFileChooser.CANCEL_OPTION =>
          true
        case JFileChooser.APPROVE_OPTION =>
          m68kDebugger.commentedROMLoad(fc.getSelectedFile.toString)
          m68kDebugger.enableCommentedROMPanel(enabled)
          false
    else
      m68kDebugger.enableCommentedROMPanel(enabled)
      false

  def setROM(rom:Array[Int]): Unit =
    romDialog = new MemoryDumper(rom,0,"ROM",frame,() => romDumpItem.setSelected(false),canUpdate = false,setPreferredScrollableViewportSize = false, showASCII = true).dialog

  private def jumpToAddress(): Unit =
    JOptionPane.showInputDialog(frame,"Insert hex address to jump to","") match
      case null =>
      case s =>
        try
          val address = Integer.parseInt(s,16)
          m68k.getRegister(RegisterType.PC).set(address,Size.Long)
          m68k.clearPrefetchQueue()
        catch
          case _:NumberFormatException =>
            JOptionPane.showMessageDialog(frame,"Invalid address","Error",JOptionPane.ERROR_MESSAGE)
