package ucesoft.mac.debugger

import ucesoft.mac.cpu.m68k.*

import java.awt.event.*
import java.awt.{BorderLayout, Color, Component, FlowLayout}
import java.io.{FileOutputStream, PrintWriter}
import java.util.zip.GZIPOutputStream
import javax.swing.*
import javax.swing.border.EmptyBorder
import javax.swing.table.{AbstractTableModel, DefaultTableCellRenderer, TableCellRenderer}
import scala.collection.mutable.ArrayBuffer
import Debugger.*
import ucesoft.mac.MACComponent

/**
 * @author Alessandro Abbruzzetti
 *         Created on 22/11/2023 19:46  
 */
object DebuggerUI {
  private case class Reg(value:Int,modified:Boolean,format:Option[String] = None,empty:Boolean = false)
  private final val MOD_COLOR = Color.YELLOW.darker()

  class RegisterRenderer(format: String) extends DefaultTableCellRenderer:
    private val defaultForegroundColor = getForeground

    override def setValue(value: Any): Unit =
      value match
        case Reg(value, modified,f,empty) =>
          setHorizontalAlignment(SwingConstants.CENTER)
          if empty then
            setText("")
          else
            setText(f.getOrElse(format).format(value))
          setForeground(if modified then MOD_COLOR else defaultForegroundColor)
  end RegisterRenderer

  class M68KRegisterTableModel(m68k: M6800X0, data: Boolean) extends AbstractTableModel:
    private val values = Array.ofDim[Int](8)
    private val modified = Array.ofDim[Boolean](8)

    override def getColumnName(column: Int): String = s"${if data then "D" else "A"}$column"
    override def isCellEditable(row: Int, col: Int): Boolean = false
    override def getColumnCount: Int = 8
    override def getRowCount: Int = 1
    override def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef = Reg(values(columnIndex), modified(columnIndex))
    override def getColumnClass(columnIndex: Int): Class[?] = classOf[String]
    def contentUpdated(): Unit =
      val rtype = if data then RegisterType.Data else RegisterType.Address
      for r <- 0 to 7 do
        val reg = m68k.getRegister(rtype, r).get()
        modified(r) = reg != values(r)
        values(r) = reg
      fireTableDataChanged()
  end M68KRegisterTableModel

  class StatusRegisterRenderer extends JCheckBox with TableCellRenderer:
    private val noFocusBorder = new EmptyBorder(1, 1, 1, 1)
    setHorizontalAlignment(SwingConstants.CENTER)
    setBorderPainted(true)

    override def getTableCellRendererComponent(table: JTable, _value: Any, isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int): Component =
      val (value, modified) = _value match
        case Reg(v, m,_,_) => (v, m)
        case _ => (0, false)

      if isSelected then
        setForeground(table.getSelectionForeground)
        super.setBackground(table.getSelectionBackground)
      else
        setForeground(table.getForeground)
        if modified then
          setBackground(MOD_COLOR)
        else
          setBackground(table.getBackground)

      setSelected(value == 1)

      if hasFocus then
        setBorder(UIManager.getBorder("Table.focusCellHighlightBorder"))
      else
        setBorder(noFocusBorder)

      this
  end StatusRegisterRenderer

  class M68KStatusRegisterTableModel(m68k: M6800X0) extends AbstractTableModel:
    private val columns = Array("T", "S", "Int", "X", "N", "Z", "V", "C")
    private val values = Array.ofDim[Boolean](8)
    private var intValue = 0
    private val modified = Array.ofDim[Boolean](8)

    override def getColumnName(column: Int): String = columns(column)
    override def getColumnCount: Int = columns.length
    override def getRowCount: Int = 1
    override def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef =
      columnIndex match
        case 2 =>
          Reg(intValue, modified(columnIndex))
        case _ =>
          Reg(if values(columnIndex) then 1 else 0, modified(columnIndex))
    override def getColumnClass(columnIndex: Int): Class[?] =
      columnIndex match
        case 2 =>
          classOf[String]
        case _ =>
          classOf[java.lang.Boolean]

    def contentUpdated(): Unit =
      import StatusRegister.StatusFlag.*
      val sr = m68k.getRegister(RegisterType.SR).asInstanceOf[StatusRegister]
      for c <- Seq(0, 1, 3, 4, 5, 6, 7) do
        val value = c match
          case 0 => java.lang.Boolean.valueOf(sr.isTrace)
          case 1 => java.lang.Boolean.valueOf(sr.isSupervisorMode)
          case 3 => java.lang.Boolean.valueOf(sr.isFlag(X))
          case 4 => java.lang.Boolean.valueOf(sr.isFlag(N))
          case 5 => java.lang.Boolean.valueOf(sr.isFlag(Z))
          case 6 => java.lang.Boolean.valueOf(sr.isFlag(V))
          case 7 => java.lang.Boolean.valueOf(sr.isFlag(C))
        modified(c) = value != this.values(c)
        this.values(c) = value
      val intV = sr.getInterruptMask
      modified(2) = intV != intValue
      intValue = intV

      fireTableDataChanged()
  end M68KStatusRegisterTableModel

  class M68KPCTableModel(m68k: M6800X0) extends AbstractTableModel:
    private val columns = Array("PC", "USP", "SSP", "Total cycles", "Last instr. cycles")
    private val values = Array.ofDim[Int](5)
    private val modified = Array.ofDim[Boolean](5)

    override def getColumnName(column: Int): String = columns(column)
    override def getColumnCount: Int = columns.length
    override def getRowCount: Int = 1
    override def getColumnClass(columnIndex: Int): Class[?] =
      columnIndex match
        case 3 | 4 => classOf[java.lang.Integer]
        case _ => classOf[String]
    override def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef = Reg(values(columnIndex), modified(columnIndex))

    def contentUpdated(): Unit =
      for c <- 0 to 4 do
        val value = c match
          case 0 => m68k.getLastInstructionPC
          case 1 => m68k.getRegister(RegisterType.USP).get()
          case 2 => m68k.getRegister(RegisterType.SSP).get()
          case 3 => m68k.getTotalElapsedCycles.toInt
          case 4 => m68k.getLastInstructionElapsedCycles
        modified(c) = value != values(c)
        values(c) = value

      fireTableDataChanged()
  end M68KPCTableModel

  class DisassembledCellRenderer extends DefaultTableCellRenderer:
    import java.awt.Font

    private final val BREAK_BG_COLOR = new Color(167,28,12)
    private val fontSize = getFont.getSize
    private val font = Font.decode(s"monospaced-italic-$fontSize")
    private val fontSel = Font.decode(s"monospaced-bold-$fontSize")
    private var breakRow = false
    setHorizontalAlignment(SwingConstants.LEFT)

    override def getTableCellRendererComponent(table: JTable, value: Any, isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int): Component =
      val c = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)
      if column == 0 then
        breakRow = value.toString.nonEmpty
      if column == 4 && value.toString.nonEmpty then
        setToolTipText(value.toString)
      else
        setToolTipText(null)

      if breakRow && !isSelected then
        c.setBackground(BREAK_BG_COLOR)
      else if !isSelected then
        c.setBackground(table.getBackground)
      else c.setBackground(table.getSelectionBackground)

      if isSelected then
        c.setFont(fontSel)
      else
        c.setFont(font)
      c
  end DisassembledCellRenderer

  class DisassembledTableModel(addressBreakHandler: Int => Option[String],
                               noteEditable: Boolean = false) extends AbstractTableModel:
    private case class DisInfo(numAddress: Int, address: String, opcodes: String, mnemonic: String, var notes: String, disString: String)

    private val columns = if noteEditable then Array("Brk", "Address", "Opcodes", "Mnemonic","Notes") else Array("Brk", "Address", "Opcodes", "Mnemonic")
    private val rows = new ArrayBuffer[DisInfo]

    override def getColumnName(column: Int): String = columns(column)
    override def getColumnCount: Int = columns.length
    override def getRowCount: Int = rows.size
    override def isCellEditable(rowIndex: Int, columnIndex: Int): Boolean = noteEditable && columnIndex == 4
    override def getColumnClass(columnIndex: Int): Class[?] = classOf[String]
    override def setValueAt(aValue: Any, rowIndex: Int, columnIndex: Int): Unit =
      rows(rowIndex).notes = aValue.toString
    override def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef =
      val dis = rows(rowIndex)
      columnIndex match
        case 0 => addressBreakHandler(dis.numAddress) match
          case Some(brk) => brk
          case None => ""
        case 1 => dis.address
        case 2 => dis.opcodes
        case 3 => dis.mnemonic
        case 4 => dis.notes

    def clear(): Unit =
      rows.clear()
      fireTableDataChanged()

    def add(d: DisassembledInstruction, busNotAvailable: Boolean): Unit =
      val dis = DisInfo(
        numAddress = d.address,
        address = "%08X".format(d.address),
        opcodes = (if d.isValidOpCode then d.opcode :: d.extendedWords else d.extendedWords).map(a => "%04X".format(a)).mkString(" "),
        mnemonic = if d.isValidOpCode then
          s"${d.mnemonic} ${
            d.op1 match
              case None => ""
              case Some(o1) =>
                d.op2 match
                  case None => o1
                  case Some(o2) => s"$o1,$o2"
          }"
        else "DW",
        notes = "",
        disString = d.toString
      )
      rows += dis

    def update(): Unit = fireTableDataChanged()

    def getAddressAt(row: Int): Int = Integer.parseInt(rows(row).address, 16)

    def copyToClipboard(): Unit =
      val sb = new StringBuilder()
      for r <- rows do
        val dis = r.disString.toUpperCase().replace(".SHORT","DW")
        sb.append(s"$dis\n")

      java.awt.Toolkit.getDefaultToolkit.getSystemClipboard.setContents(java.awt.datatransfer.StringSelection(sb.toString), null)

  end DisassembledTableModel

  case class Prop(name:String,value:String,changed:Boolean)
  
  class PropertiesCellRenderer(model: PropertiesTableModel) extends DefaultTableCellRenderer:
    private val defaultForegroundColor = getForeground
    setHorizontalAlignment(SwingConstants.LEFT)

    override def getTableCellRendererComponent(table: JTable, value: Any, isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int): Component =
      //super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)
      val prop = value.asInstanceOf[Prop]
      if column == 1 then
        val tooltip = prop.value
        if tooltip.isEmpty then setToolTipText(null) else setToolTipText(tooltip)
        super.getTableCellRendererComponent(table, prop.value, isSelected, hasFocus, row, column)
      else
        setToolTipText(null)
        super.getTableCellRendererComponent(table, prop.name, isSelected, hasFocus, row, column)
      setForeground(if prop.changed then MOD_COLOR else defaultForegroundColor)
      this
  end PropertiesCellRenderer

  class PropertiesTableModel(val comp:MACComponent, frame:JFrame) extends AbstractTableModel:
    private val columns = Array("Property", "Value")
    private var properties = comp.getProperties
    private var props : List[Prop] = setProps(properties)

    private def setProps(p:List[MACComponent.Property]): List[Prop] =
      p.zip(properties).map((newP,oldP) => Prop(newP.name,newP.value,newP.value != oldP.value))

    override def getColumnName(column: Int): String = columns(column)
    override def getColumnCount: Int = columns.length
    override def getRowCount: Int = properties.size

    override def getColumnClass(columnIndex: Int): Class[?] = classOf[Prop]
    override def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef = props(rowIndex)

    def update(): Unit =
      val newP = comp.getProperties
      props = setProps(newP)
      properties = newP
      fireTableDataChanged()
  end PropertiesTableModel
  

  class DisassemblerPanel(name:String,
                          disaHandler: (DisassembledTableModel,Int) => Int,
                          frame:JFrame,
                          disassemblerBreakHandler: DisassemblerBreakHandler,
                          override val windowCloseOperation: () => Unit) extends RefreshableDialog(frame, s"$name Disassembler", windowCloseOperation) with BreakListener:
    private val model = new DisassembledTableModel(disassemblerBreakHandler.getBreakStringAt, true)
    private var isAdjusting = false
    
    init()

    override def addBreak(break: AddressBreakType): Unit = if !isAdjusting then model.update()
    override def removeBreak(address: Int): Unit = if !isAdjusting then model.update()

    private def initTable(table: JTable, model: DisassembledTableModel): Unit =
      table.getTableHeader.setReorderingAllowed(false)
      table.setDefaultRenderer(classOf[String], new DisassembledCellRenderer)
      val colModel = table.getColumnModel
      colModel.getColumn(0).setMinWidth(45)
      colModel.getColumn(0).setMaxWidth(50)
      colModel.getColumn(1).setMinWidth(70)
      colModel.getColumn(1).setMaxWidth(80)
      colModel.getColumn(2).setMinWidth(130)
      colModel.getColumn(2).setMaxWidth(180)
      colModel.getColumn(3).setMinWidth(160)
      colModel.getColumn(3).setMaxWidth(200)
      table.addMouseListener(new MouseAdapter {
        override def mouseClicked(e: MouseEvent): Unit =
          if e.getClickCount == 2 then
            val row = table.rowAtPoint(e.getPoint)
            val address = model.getAddressAt(row)
            isAdjusting = true
            if disassemblerBreakHandler.hasBreakAt(address) then
              disassemblerBreakHandler.removeBreakAt(address)
            else
              disassemblerBreakHandler.addExecuteBreakAt(address)
            isAdjusting = false
            model.update()
      })

    private def disassemble(fromS: String, toS: String): Unit =
      try
        val from = java.lang.Integer.parseInt(fromS, 16) & 0xFF_FFFF
        val to = java.lang.Integer.parseInt(toS, 16) & 0xFF_FFFF
        if to < from then
          throw new IllegalArgumentException
        model.clear()
        new Thread(() => {
          var a = from
          while a <= to do
            a = disaHandler(model,a)
          model.update()
        }).start()
      catch
        case _: Exception =>
          JOptionPane.showMessageDialog(dialog, s"Invalid range of addresses", "Address error", JOptionPane.ERROR_MESSAGE)

    override protected def init(): Unit =
      super.init()
      disassemblerBreakHandler.addBreakListener(this)
      val mainPanel = new JPanel(new BorderLayout())
      dialog.getContentPane.add("Center", mainPanel)
      val buttonPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
      val copyClip = new JButton(new ImageIcon(getClass.getResource("/resources/trace/copy.png")))
      val disButton = new JButton(new ImageIcon(getClass.getResource("/resources/trace/bug.png")))
      val fromTF = new JTextField("000000", 10)
      val toTF = new JTextField("000000", 10)

      copyClip.addActionListener(_ => model.copyToClipboard())
      copyClip.setToolTipText("Copy to clipboard")
      buttonPanel.add(copyClip)
      buttonPanel.add(new JLabel("From:", SwingConstants.RIGHT))
      buttonPanel.add(fromTF)
      buttonPanel.add(new JLabel("To:", SwingConstants.RIGHT))
      buttonPanel.add(toTF)
      fromTF.addFocusListener(new FocusListener:
        override def focusGained(e: FocusEvent): Unit = {}

        override def focusLost(e: FocusEvent): Unit =
          try
            val from = java.lang.Integer.parseInt(fromTF.getText, 16)
            val to = (from + 1024) & 0xFF_FFFF
            toTF.setText(to.toHexString)
          catch
            case _: NumberFormatException =>
      )
      disButton.addActionListener(_ => disassemble(fromTF.getText, toTF.getText))
      buttonPanel.add(disButton)

      val disPanel = new JPanel(new BorderLayout())
      disPanel.add("North", buttonPanel)
      val disTable = new JTable(model)
      val sp = new JScrollPane(disTable)
      sp.setBorder(BorderFactory.createTitledBorder(s"$name Disassembler"))
      initTable(disTable, model)
      disPanel.add("Center", sp)

      mainPanel.add("Center", disPanel)

      dialog.pack()
  end DisassemblerPanel

  private class BreaksTableModel(addressPaddingLen:Int,breakHandler:DisassemblerBreakHandler) extends AbstractTableModel:
    private val breaks = new ArrayBuffer[AddressBreakType]
    private var adjusting = false

    def setAdjusting(adjusting:Boolean): Unit =
      this.adjusting = adjusting

    override def getColumnName(column: Int): String = column match
      case 0 => "Enabled"
      case 1 => "Address"
      case 2 => "Type"

    override def isCellEditable(row: Int, col: Int): Boolean = col == 0
    override def setValueAt(aValue: Any, rowIndex: Int, columnIndex: Int): Unit =
      adjusting = true
      breaks(rowIndex).enabled ^= true
      if breaks(rowIndex).enabled then
        breakHandler.addBreakAt(breaks(rowIndex).address,read = breaks(rowIndex).read, write = breaks(rowIndex).write, execute = breaks(rowIndex).execute)
      else
        breakHandler.removeBreakAt(breaks(rowIndex).address)
      adjusting = false
      fireTableCellUpdated(rowIndex,columnIndex)
    override def getRowCount: Int = breaks.size
    override def getColumnCount: Int = 3
    override def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef =
      columnIndex match
        case 0 =>
          java.lang.Boolean.valueOf(breaks(rowIndex).enabled)
        case 1 =>
          s"%0${addressPaddingLen}X".format(breaks(rowIndex).address)
        case 2 =>
          breaks(rowIndex).toString

    def removeBreakAtRow(rows: Array[Int]): Unit =
      if !adjusting then
        val orderedRows = rows.sortWith((r1, r2) => r1 > r2)
        for (r <- orderedRows) breaks.remove(r)
        fireTableDataChanged()

    def removeBreak(b:AddressBreakType): Unit =
      if !adjusting then
        breaks -= b
        fireTableDataChanged()

    def removeBreak(address:Int): Unit =
      if !adjusting then
        val index = breaks.indexWhere(_.address == address)
        if index != -1 then
          breaks.remove(index)
          fireTableDataChanged()
    def getBreakAtRow(row: Int): AddressBreakType = breaks(row)

    def setBreakAtRow(row: Int, b: AddressBreakType): Unit =
      if !adjusting then
        breaks(row) = b
        fireTableRowsUpdated(row, row)

    def getBreaks: List[AddressBreakType] = breaks.toList

    override def getColumnClass(columnIndex: Int): Class[?] =
      columnIndex match
        case 0 => classOf[java.lang.Boolean]
        case _ => classOf[String]
    def contentChanged(breaks: List[AddressBreakType]): Unit =
      this.breaks.clear()
      this.breaks.addAll(breaks)
      fireTableDataChanged()
    def contentUpdated(): Unit = fireTableDataChanged()
    def addBreak(b: AddressBreakType): Unit =
      if !adjusting then
        breaks += b
        fireTableDataChanged()

    def clear(): Unit =
      breaks.clear()
      fireTableDataChanged()
  end BreaksTableModel

  private class BreakpointPanel(addressPaddingLen:Int, breakHandler:DisassemblerBreakHandler) extends JPanel:
    val model = new BreaksTableModel(addressPaddingLen,breakHandler)
    private val table = new JTable(model)

    init()

    private def init(): Unit =
      setLayout(new BorderLayout())
      table.setAutoCreateRowSorter(true)
      table.setFillsViewportHeight(true)
      val sp = new JScrollPane(table)
      sp.setBorder(BorderFactory.createTitledBorder("Addresses"))
      add("Center",sp)
      val buttonPanel = new JPanel(new FlowLayout())
      val addBreakButton = new JButton(new ImageIcon(getClass.getResource("/resources/trace/plus.png")))
      val delBreakButton = new JButton(new ImageIcon(getClass.getResource("/resources/trace/minus.png")))
      addBreakButton.setToolTipText("Add a new breakpoint")
      delBreakButton.setToolTipText("Remove selected breakpoints")
      buttonPanel.add(addBreakButton)
      buttonPanel.add(delBreakButton)
      add("South",buttonPanel)

      addBreakButton.addActionListener(_ => editBreak(None) )
      delBreakButton.addActionListener(_ => removeSelectedBreaks() )

      table.addMouseListener(new MouseAdapter:
        override def mouseClicked(e: MouseEvent): Unit =
          if e.getClickCount == 2 then
            val break = model.getBreakAtRow(table.getSelectedRow)
            editBreak(Some(break))
      )

      table.getSelectionModel.addListSelectionListener(e => 
        if !e.getValueIsAdjusting then
          val selected = table.getSelectedRowCount > 0
          delBreakButton.setEnabled(selected)
      )

    private def editBreak(break:Option[AddressBreakType]): Unit =
      val panel = new JPanel(new BorderLayout())
      val readCheckBox = new JCheckBox("Read")
      val writeCheckBox = new JCheckBox("Write")
      val executeCheckBox = new JCheckBox("Execute")
      executeCheckBox.setSelected(true)
      val northPanel = new JPanel(new FlowLayout())
      northPanel.add(readCheckBox)
      northPanel.add(writeCheckBox)
      northPanel.add(executeCheckBox)
      panel.add("North",northPanel)
      val addressPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
      addressPanel.add(new JLabel("Address:",SwingConstants.RIGHT))
      val addressTF = new JTextField(addressPaddingLen)
      addressPanel.add(addressTF)
      panel.add("Center",addressPanel)
      val okPanel = new JPanel(new FlowLayout())
      val okButton = new JButton("Ok")
      val cancelButton = new JButton("Cancel")
      okPanel.add(okButton)
      okPanel.add(cancelButton)
      panel.add("South",okPanel)
      break match
        case Some(b) =>
          addressTF.setText(s"%0${addressPaddingLen}X".format(b.address))
          readCheckBox.setSelected(b.read)
          writeCheckBox.setSelected(b.write)
          executeCheckBox.setSelected(b.execute)
        case None =>
      val dialog = new JDialog(SwingUtilities.getWindowAncestor(this).asInstanceOf[JDialog],"Edit breakpoint",true)
      dialog.getContentPane.add("Center",panel)
      cancelButton.addActionListener(_ => dialog.dispose())
      val okAction : ActionEvent => Unit = _ => {
        try
          val newBreak = AddressBreakType(Integer.parseInt(addressTF.getText(),16),read = readCheckBox.isSelected,write = writeCheckBox.isSelected,execute = executeCheckBox.isSelected)
          if !newBreak.read && !newBreak.write && !newBreak.execute then
            throw new IllegalArgumentException()
          break match
            case Some(b) =>
              model.removeBreak(b)
              model.setAdjusting(true)
              breakHandler.removeBreakAt(b.address)
              model.setAdjusting(false)
            case None =>
          model.addBreak(newBreak)

          model.setAdjusting(true)
          breakHandler.addBreakAt(newBreak.address,read = newBreak.read, write = newBreak.write, execute = newBreak.execute)
          model.setAdjusting(false)
          dialog.dispose()
        catch
          case _:IllegalArgumentException =>
            JOptionPane.showMessageDialog(dialog,"Select one break mode","Invalid break mode",JOptionPane.ERROR_MESSAGE)
          case _:NumberFormatException =>
            JOptionPane.showMessageDialog(dialog, "Insert a valid hex address", "Invalid address format", JOptionPane.ERROR_MESSAGE)
      }
      okButton.addActionListener(e => okAction(e))
      addressTF.addActionListener(e => okAction(e))
      dialog.pack()
      addressTF.requestFocusInWindow()
      dialog.setLocationRelativeTo(SwingUtilities.getWindowAncestor(this))
      dialog.setVisible(true)
    private def removeSelectedBreaks(): Unit =
      val orderedRows = table.getSelectedRows.sortWith((r1, r2) => r1 > r2)
      for r <- orderedRows do
        breakHandler.removeBreakAt(model.getBreakAtRow(r).address)

      model.removeBreakAtRow(table.getSelectedRows)
  end BreakpointPanel

  class BreakMasterPanel(name:String,
                         frame: JFrame,
                         addressPaddingLen:Int,
                         removeBreakHandler: Debugger.AddressBreakType => Unit,
                         addBreakHandler: Debugger.AddressBreakType => Unit,
                         northPanel: JPanel,
                         breakHandler:DisassemblerBreakHandler,
                         override val windowCloseOperation: () => Unit) extends RefreshableDialog(frame, s"$name breakpoints", windowCloseOperation) with BreakListener:
    private val breakPanel = new BreakpointPanel(addressPaddingLen,breakHandler)
    init()

    override protected def init(): Unit =
      super.init()
      breakHandler.addBreakListener(this)
      dialog.getContentPane.add("North", northPanel)
      dialog.getContentPane.add("Center",breakPanel)
      dialog.pack()

    override def addBreak(break: AddressBreakType): Unit =
      breakPanel.model.addBreak(break)
    override def removeBreak(address: Int): Unit =
      breakPanel.model.removeBreak(address)
  end BreakMasterPanel
  
  class M68KBreakEventPanel(breakHandler:DisassemblerBreakHandler) extends JPanel:
    init()
    private def init(): Unit =
      setLayout(new FlowLayout())
      val resetCB = new JCheckBox("reset")
      val haltCB = new JCheckBox("halt")
      val stopCB = new JCheckBox("stop")
      val intCB = new JCheckBox("interrupt")
      val exCB = new JCheckBox("exception")
      val intTF = new JTextField(3)
      val exTF = new JTextField(3)
      
      intTF.setEnabled(false)
      exTF.setEnabled(false)
      
      resetCB.addActionListener(_ => if resetCB.isSelected then breakHandler.addBreakEvent("reset",null) else breakHandler.removeBreakEvent("reset"))
      haltCB.addActionListener(_ => if haltCB.isSelected then breakHandler.addBreakEvent("halt", null) else breakHandler.removeBreakEvent("halt"))
      stopCB.addActionListener(_ => if stopCB.isSelected then breakHandler.addBreakEvent("stop", null) else breakHandler.removeBreakEvent("stop"))
      intCB.addActionListener(_ => {
        intTF.setEnabled(intCB.isSelected)
        if !intCB.isSelected then breakHandler.removeBreakEvent("interrupt")
      })
      exCB.addActionListener(_ => {
        exTF.setEnabled(exCB.isSelected)
        if !exCB.isSelected then breakHandler.removeBreakEvent("exception")
      })
      intTF.setToolTipText("insert interrupt number and press ENTER")
      exTF.setToolTipText("insert exception number and press ENTER")

      intTF.addActionListener(_ => {
        try
          val i = intTF.getText.toInt
          if i < 0 || i > 7 then throw new IllegalArgumentException()
          breakHandler.addBreakEvent("interrupt",Integer.valueOf(i))
        catch
          case _:IllegalArgumentException =>
            JOptionPane.showMessageDialog(this,"Interrupt number must be >= 0 and < 8","Invalid interrupt",JOptionPane.ERROR_MESSAGE)
          case _:NumberFormatException =>
            JOptionPane.showMessageDialog(this, "Interrupt must be a decimal number", "Invalid interrupt", JOptionPane.ERROR_MESSAGE)
      })
      exTF.addActionListener(_ => {
        try
          val i = exTF.getText.toInt
          if i < 0 || i > 255 then throw new IllegalArgumentException()
          breakHandler.addBreakEvent("exception", Integer.valueOf(i))
        catch
          case _: IllegalArgumentException =>
            JOptionPane.showMessageDialog(this, "Interrupt number must be >= 0 and < 256", "Invalid interrupt", JOptionPane.ERROR_MESSAGE)
          case _: NumberFormatException =>
            JOptionPane.showMessageDialog(this, "Interrupt must be a decimal number", "Invalid interrupt", JOptionPane.ERROR_MESSAGE)
      })
      
      add(resetCB)
      add(haltCB)
      add(stopCB)
      add(intCB)
      add(intTF)
      add(exCB)
      add(exTF)
      setBorder(BorderFactory.createTitledBorder("Events"))
  end M68KBreakEventPanel

  trait TraceListener:
    def onTrace(disassembly:String,address:Int): Unit

  class SaveTraceDialog(frame:JFrame,startAction: TraceListener => Unit,stopAction: () => Unit) extends JDialog(frame,"Trace saving",true) with TraceListener:
    private val addressTF = new JTextField("000000")
    private val fileTF = new JTextField(20)
    private val fetchedLabel = new JLabel("0")
    private var fetchedCounter = 0
    private var stopIfAddress = false
    private var address = 0
    private var gzipped = false
    private var out : PrintWriter = scala.compiletime.uninitialized

    init()

    override final def onTrace(disassembly:String,address:Int): Unit =
      out.println(disassembly)
      fetchedCounter += 1
      fetchedLabel.setText(fetchedCounter.toString)

      if stopIfAddress && this.address == address then
        stopTracing(s"Tracing stopped: reached address ${addressTF.getText}")

    private def init(): Unit =
      val panel = new JPanel()
      val boxLayout = new BoxLayout(panel,BoxLayout.Y_AXIS)
      panel.setLayout(boxLayout)
      var dummyPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
      dummyPanel.add(new JLabel("Save to file:",SwingConstants.RIGHT))
      dummyPanel.add(fileTF)
      val browseButton = new JButton("Browse..")
      dummyPanel.add(browseButton)
      panel.add(dummyPanel)
      dummyPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
      val addressCB = new JCheckBox("Stop if address is reached")
      dummyPanel.add(addressCB)
      dummyPanel.add(addressTF)
      panel.add(dummyPanel)
      addressTF.setEnabled(false)
      addressCB.addActionListener(_ => {
        addressTF.setEnabled(addressCB.isSelected)
        stopIfAddress = addressCB.isSelected
      })

      browseButton.addActionListener(_ => {
        val fc = new JFileChooser()
        fc.setSelectedFile(new java.io.File(fileTF.getText()))
        fc.showSaveDialog(this) match
          case JFileChooser.APPROVE_OPTION =>
            fileTF.setText(fc.getSelectedFile.toString)
          case _ =>
      })

      dummyPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
      val gzipCB = new JCheckBox("gzip file")
      dummyPanel.add(gzipCB)
      gzipCB.addActionListener(_ => gzipped = gzipCB.isSelected)
      panel.add(dummyPanel)

      dummyPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
      dummyPanel.setBorder(BorderFactory.createLineBorder(Color.WHITE))
      dummyPanel.add(new JLabel("Processed instructions:",SwingConstants.RIGHT))
      dummyPanel.add(fetchedLabel)
      panel.add(dummyPanel)

      val buttonPanel = new JPanel(new FlowLayout())
      val startButton = new JButton("START")
      val cancelButton = new JButton("Cancel")

      cancelButton.addActionListener(_ => dispose())
      startButton.addActionListener(_ => startTracing(panel,startButton,cancelButton))

      buttonPanel.add(startButton)
      buttonPanel.add(cancelButton)

      val pane = getContentPane
      pane.add("Center",panel)
      pane.add("South",buttonPanel)

      pack()
      setResizable(false)

    private def stopTracing(msg:String): Unit =
      try
        out.close()
      catch
        case _ =>
      stopAction()
      JOptionPane.showMessageDialog(this, msg, "Tracing stopped", JOptionPane.INFORMATION_MESSAGE)
      dispose()

    private def startTracing(panel:JPanel,startButton:JButton,cancelButton:JButton): Unit =
      if startButton.getText != "START" then return

      if stopIfAddress then
        try
          address = Integer.parseInt(addressTF.getText,16)
        catch
          case _:NumberFormatException =>
            JOptionPane.showMessageDialog(this, "Invalid address, use hex format", "Address error", JOptionPane.ERROR_MESSAGE)
            return

      val file = fileTF.getText
      if file.isEmpty then
        JOptionPane.showMessageDialog(this,"Insert a valid file path","Path error",JOptionPane.ERROR_MESSAGE)
      else
        try
          val fileName = if gzipped then s"$file.gz" else file
          val fileOut = if gzipped then new GZIPOutputStream(new FileOutputStream(fileName)) else new FileOutputStream(fileName)
          out = new PrintWriter(fileOut)
          cancelButton.setEnabled(false)
          startButton.setText("STOP")
          startButton.addActionListener(_ => stopTracing("Tracing stopped"))
          startAction(this)
          for c <- panel.getComponents do
            c.setEnabled(false)
        catch
          case t:Throwable =>
            JOptionPane.showMessageDialog(this, s"Cannot open file: ${t.getMessage}", "File open error", JOptionPane.ERROR_MESSAGE)
}
