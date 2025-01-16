package ucesoft.mac.debugger

import java.awt.event.{WindowAdapter, WindowEvent}
import java.awt.{BorderLayout, Component, FlowLayout}
import javax.swing.table.{AbstractTableModel, DefaultTableCellRenderer}
import javax.swing.*

object MemoryDumper:
  def main(args:Array[String]): Unit =
    val f = new JFrame()
    val mem = Array.ofDim[Int](128)

    val dumper = new MemoryDumper(mem,0xC0_0000,"Memory dumper",null,() => println("Window closed"))
    dumper.dialog.setVisible(true)
/**
 * @author Alessandro Abbruzzetti
 *         Created on 22/10/2023 18:38  
 */
class MemoryDumper(mem:Array[Int],
                   startAddress:Int,
                   override val title:String,
                   override val frame:JFrame,
                   override val windowCloseOperation: () => Unit,
                   setPreferredScrollableViewportSize:Boolean = true,
                   showASCII:Boolean = false,
                   canUpdate:Boolean = true,
                   wordValues:Boolean = false) extends RefreshableDialog(frame, title, windowCloseOperation):

  private val model = new DumperTableModel

  init()

  private class DumperCellRenderer extends DefaultTableCellRenderer:
    import java.awt.Font
    private val fontSize = getFont.getSize
    private val font = Font.decode(s"monospaced-$fontSize")
    private val fontHighlighted = Font.decode(s"monospaced-bold-$fontSize")
    setHorizontalAlignment(SwingConstants.CENTER)

    override def getTableCellRendererComponent(table: JTable, value: Any, isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int): Component =
      val c = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)

      val str = value.toString
      if str.isBlank || str == "00" then
        c.setFont(font)
      else
        c.setFont(fontHighlighted)

      if column != 0 then
        val address = startAddress + (row << 4) + (column - 1)
        setToolTipText(address.toHexString.toUpperCase())
      else
        setToolTipText(null)
      c

  private class DumperTableModel extends AbstractTableModel:
    private val columns = Array("Address") ++ (0 to 0xF).map(_.toHexString.toUpperCase()).toArray ++ (if showASCII then Array("Ascii") else Array[String]())
    private val maxAddressDigits = (startAddress + mem.length).toHexString.length

    override def getColumnName(column: Int): String = columns(column)
    override def getColumnCount: Int = columns.length
    override def getRowCount: Int = mem.length / 16 + (if (mem.length % 16) != 0 then 1 else 0)

    override def isCellEditable(rowIndex: Int, columnIndex: Int): Boolean =
      val address = (rowIndex << 4) + (columnIndex - 1)
      columnIndex > 0 && (!showASCII || showASCII && columnIndex < columns.length - 1) && address < mem.length

    override def getColumnClass(columnIndex: Int): Class[?] = classOf[String]

    override def setValueAt(aValue: Any, rowIndex: Int, columnIndex: Int): Unit =
      try
        val maxValue = if wordValues then 0xFFFF else 0xFF
        val address = (rowIndex << 4) + (columnIndex - 1)
        val byte = Integer.parseInt(aValue.toString,16)
        if byte < 0 || byte > maxValue then
          throw new IllegalArgumentException()
        mem(address) = byte
        fireTableRowsUpdated(rowIndex,rowIndex)
      catch
        case _:NumberFormatException =>
          JOptionPane.showMessageDialog(frame,s"Invalid hex byte: $aValue","Type mismatch",JOptionPane.ERROR_MESSAGE)
        case _:IllegalArgumentException =>
          JOptionPane.showMessageDialog(frame,s"Invalid hex byte: $aValue. Must be between 00 and FF","Value too large",JOptionPane.ERROR_MESSAGE)

    override def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef =
      if columnIndex == 0 then
        s"%0${maxAddressDigits}X".format(startAddress + (rowIndex << 4))
      else if showASCII && columnIndex == columns.length - 1 then
        val sb = new StringBuilder()
        val address = rowIndex << 4
        for c <- address until address + 16 do
          val char = mem(c)
          val pchar = if char >= 32 && char <= 126 then char.toChar else '.'
          sb.append(pchar)
        sb.toString
      else
        val address = (rowIndex << 4) + (columnIndex - 1)
        if address >= mem.length then ""
        else
          val digits = if wordValues then 4 else 2
          s"%0${digits}X".format(mem(address))

    def update(): Unit =
      fireTableDataChanged()

  override def updateModel(): Unit = model.update()

  override protected def init(): Unit =
    super.init()
    val mainPanel = new JPanel(new BorderLayout())
    val buttonPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val table = new JTable(model)

    if canUpdate then
      val buttonPanel = makeRefreshButtonPanel()
      mainPanel.add("North",buttonPanel)

    table.setDefaultRenderer(classOf[String], new DumperCellRenderer)
    table.getTableHeader.setReorderingAllowed(false)

    val colModel = table.getColumnModel
    colModel.getColumn(0).setMinWidth(70)
    colModel.getColumn(0).setMaxWidth(80)
    for col <- 1 to 16 do
      colModel.getColumn(col).setMinWidth(40)
      colModel.getColumn(col).setMaxWidth(40)
    if showASCII then
      colModel.getColumn(17).setMinWidth(140)
      colModel.getColumn(17).setMaxWidth(150)

    val scrollPane = new JScrollPane(table)

    mainPanel.add("Center",scrollPane)

    val searchPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    searchPanel.add(new JLabel("Goto:"))
    val gotoTF = new JTextField(5)
    gotoTF.setToolTipText("Hex address to go to")
    val searchHexTF = new JTextField(10)
    searchHexTF.setToolTipText("Hex bytes to search (without spaces, 2 digits each) or a string to search between double quotes")
    searchPanel.add(gotoTF)
    searchPanel.add(new JLabel("Search Hex:"))
    searchPanel.add(searchHexTF)
    gotoTF.addActionListener(_ => {
      try
        val address = Integer.parseInt(gotoTF.getText,16)
        if address < startAddress then
          throw new IllegalArgumentException()
        val row = (address - startAddress) >> 4
        if row < table.getRowCount then
          val cellRect = table.getCellRect(row, 0, true)
          table.scrollRectToVisible(cellRect)
          table.getSelectionModel.setSelectionInterval(row,row)
      catch
        case _:NumberFormatException =>
          JOptionPane.showMessageDialog(frame,"Invalid hex address","Address mismatch",JOptionPane.ERROR_MESSAGE)
        case _: IllegalArgumentException =>
          JOptionPane.showMessageDialog(frame, s"Invalid address must be >= ${startAddress.toHexString}", "Address mismatch", JOptionPane.ERROR_MESSAGE)
    })
    searchHexTF.addActionListener(_ => {
      val text = searchHexTF.getText
      try
        val arrayToSearch = if text.startsWith("\"") && text.endsWith("\"") then
          text.substring(1,text.length - 1).getBytes().map(_.toInt & 0xFF)
        else
          text.sliding(2,2).map(h => Integer.parseInt(h,16)).toArray
        ByteArraySearch.findSubArrayIndex(mem, arrayToSearch) match
          case None =>
          case Some(address) =>
            val row = (address - startAddress) >> 4
            if row < table.getRowCount then
              val cellRect = table.getCellRect(row, 0, true)
              table.scrollRectToVisible(cellRect)
              table.getSelectionModel.setSelectionInterval(row,row)
      catch
        case e:NumberFormatException =>
          JOptionPane.showMessageDialog(frame, s"Invalid hex array: $text", "Hex array mismatch", JOptionPane.ERROR_MESSAGE)
    })

    mainPanel.add("South",searchPanel)

    if setPreferredScrollableViewportSize then
      table.setPreferredScrollableViewportSize(table.getPreferredSize)
    else
      val pref = scrollPane.getPreferredSize
      pref.width = table.getPreferredSize.width
      scrollPane.setPreferredSize(pref)

    dialog.getContentPane.add("Center",mainPanel)
    dialog.pack()
