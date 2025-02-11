package ucesoft.mac.ui

import com.formdev.flatlaf.FlatLightLaf
import ucesoft.mac.misc.DNDHandler
import ucesoft.mac.{MacModel, ROM}

import java.awt.{BorderLayout, FlowLayout, GridLayout}
import java.io.File
import javax.swing.*
import javax.swing.border.BevelBorder
import javax.swing.event.{ListSelectionEvent, ListSelectionListener}
import javax.swing.table.DefaultTableModel
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.compiletime.uninitialized

object ConfigPanel:
  def main(args:Array[String]): Unit =
    FlatLightLaf.setup()
    JFrame.setDefaultLookAndFeelDecorated(false)
    JDialog.setDefaultLookAndFeelDecorated(false)
    UIManager.setLookAndFeel("com.formdev.flatlaf.themes.FlatMacLightLaf")
    val f = new JFrame()
    f.getContentPane.add("Center",new ConfigPanel(_ => f.dispose()))
    f.pack()
    f.setVisible(true)
/**
 * @author Alessandro Abbruzzetti
 *         Created on 14/01/2025 10:09  
 */
class ConfigPanel(action: Boolean => Unit) extends JPanel:
  private class ListModel(indexMap: Int => Int,update:List[String] => Unit) extends AbstractListModel[String]:
    private val data = new ArrayBuffer[String]()

    def addItem(s:String): Unit =
      data += s
      fireContentsChanged(this,0,data.length)
      update(data.toList)
    def removeItem(index:Int): Unit =
      data.remove(index)
      fireContentsChanged(this, 0, data.length)
      update(data.toList)
    def up(index:Int): Unit =
      val above = data(index - 1)
      data(index - 1) = data(index)
      data(index) = above
      fireContentsChanged(this, 0, data.length)
    def down(index: Int): Unit =
      val below = data(index + 1)
      data(index + 1) = data(index)
      data(index) = below
      fireContentsChanged(this, 0, data.length)

    override def getElementAt(index: Int): String = "%02d - %s".format(indexMap(index),data(index))
    override def getSize: Int = data.length
  end ListModel

  private class ListPanel(maxItems:Int,indexMap: Int => Int,update:List[String] => Unit) extends JPanel with ListSelectionListener:
    private val model = new ListModel(indexMap,update)
    private val list = new JList[String](model)
    private val up = new JButton(new ImageIcon(getClass.getResource("/resources/trace/up.png")))
    private val down = new JButton(new ImageIcon(getClass.getResource("/resources/trace/down.png")))
    private val minus = new JButton(new ImageIcon(getClass.getResource("/resources/trace/minus.png")))

    init()

    def addItem(item:String): Unit =
      if model.getSize() < maxItems then
        model.addItem(item)

    override def valueChanged(e: ListSelectionEvent): Unit =
      if !e.getValueIsAdjusting then
        val si = list.getSelectedIndex
        up.setEnabled(si != -1 && si != 0)
        down.setEnabled(si != -1 && si != model.getSize - 1)
        minus.setEnabled(si != -1)

    private def init(): Unit =
      setLayout(new BorderLayout())
      list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
      list.addListSelectionListener(this)
      list.setToolTipText("Drag & Drop")
      add("Center",list)

      up.setEnabled(false)
      down.setEnabled(false)
      minus.setEnabled(false)

      up.addActionListener(_ => model.up(list.getSelectedIndex))
      down.addActionListener(_ => model.down(list.getSelectedIndex))
      minus.addActionListener(_ => model.removeItem(list.getSelectedIndex))

      val buttonPanel = new JPanel(new GridLayout(3,1))
      buttonPanel.add(up)
      buttonPanel.add(down)
      buttonPanel.add(minus)
      add("East",buttonPanel)
      setTransferHandler(new DNDHandler((file, _) => addItem(file.toString)))
  end ListPanel

  private class RAMPanel extends JPanel with ListSelectionListener:
    private val list = new JList[String](new DefaultListModel[String])

    init()

    override def valueChanged(e: ListSelectionEvent): Unit =
      if !e.getValueIsAdjusting then
        ramIndex = list.getSelectedIndex

    private def init(): Unit =
      setLayout(new BorderLayout())
      setBorder(BorderFactory.createTitledBorder("RAM"))
      add("North",new JLabel("RAM available",new ImageIcon(getClass.getResource("/resources/ram.png")),SwingConstants.LEFT))
      add("Center",list)
      list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
      list.addListSelectionListener(this)
    def setModel(macModel:MacModel): Unit =
      val labels = macModel.ramSizesInK.map(m => {
        val k = m
        val M = m / 1024
        if M != 0 then s"${M}M" else s"${k}K"
      })
      val model = list.getModel.asInstanceOf[DefaultListModel[String]]
      model.clear()
      for l <- labels do
        model.addElement(l)
      list.setSelectedIndex(labels.length - 1)
  end RAMPanel

  private var rom : ROM = uninitialized
  private val scsiPaths = new ListBuffer[String]
  private val floppyPaths = new ListBuffer[String]
  private var ramIndex = 0
  private var lastFCPath : File = uninitialized
  private val okButton = new JButton("Start", new ImageIcon(getClass.getResource("/resources/logo.gif")))
  private val ramPanel = new RAMPanel
  private val iconLabel = new JLabel(new ImageIcon(getClass.getResource("/resources/bigLogo.png")))
  private var canceled = false
  private val centerPanel = new JTabbedPane()

  init()

  def getROM: ROM = rom
  def getSCSIPaths: List[String] = if centerPanel.isEnabledAt(0) then scsiPaths.toList else Nil
  def getFloppyPaths: List[String] = floppyPaths.toList
  def getRAMIndex: Int = ramIndex
  def isCanceled : Boolean = canceled

  private def setROM(path:String): Option[MacModel] =
    ROM.loadROM(path) match
      case Right(r@ROM(_, _, hash, model)) =>
        this.rom = r
        okButton.setEnabled(true)
        iconLabel.setIcon(new ImageIcon(getClass.getResource("/resources/bigLogoReady.png")))
        ramPanel.setModel(model)
        if model.ordinal < MacModel.PLUS.ordinal then
          centerPanel.setEnabledAt(0,false)
          centerPanel.setSelectedIndex(1)
        else
          centerPanel.setEnabledAt(0,true)
          centerPanel.setSelectedIndex(0)
        Some(model)
      case _ =>
        val info = MacModel.values.map(m => (m.toString,m.md5)).flatMap(kv => kv._2.map(md5 => Array(kv._1.asInstanceOf[AnyRef],md5.asInstanceOf[AnyRef])))
        val model = new DefaultTableModel():
          override def getColumnCount: Int = 2
          override def getColumnName(column: Int): String =
            column match
              case 0 => "Model"
              case 1 => "MD5"
        for i <- info do
          model.addRow(i)
        val table = new JTable(model)
        val sp = new JScrollPane(table)
        sp.setBorder(BorderFactory.createTitledBorder("Valid ROM MD5 hashes"))
        JOptionPane.showMessageDialog(this, sp, "Invalid ROM", JOptionPane.ERROR_MESSAGE)
        None
  private def init(): Unit =
    setLayout(new BorderLayout())
    val northPanel = new JPanel(new BorderLayout())
    northPanel.add("West",iconLabel)
    northPanel.add("Center",initROMPanel())
    add("North", northPanel)

    centerPanel.add("SCSI",initSCSIPanel())
    centerPanel.add("Floppy",initFloppyPanel())
    centerPanel.add("RAM",ramPanel)
    add("Center",centerPanel)
    val buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER))
    okButton.setEnabled(false)
    val cancelButton = new JButton("Cancel")
    buttonPanel.add(okButton)
    buttonPanel.add(cancelButton)
    add("South",buttonPanel)
    okButton.addActionListener(_ => action(true))
    cancelButton.addActionListener(_ => {
      canceled = true
      action(false)
    })
  end init

  private def initROMPanel(): JPanel =
    val romPanel = new JPanel(new FlowLayout(FlowLayout.CENTER))

    romPanel.setBorder(BorderFactory.createTitledBorder("ROM - select a valid rom"))
    val setRomButton = new JButton("Set", new ImageIcon(getClass.getResource("/resources/trace/rom.png")))
    setRomButton.setToolTipText("Set ROM")
    val pathField = new JTextField(30)
    val fc = new JFileChooser()
    val browseButton = new JButton("Browse")
    val ddLabel = new JLabel("Drag & Drop here")
    val ddPanel = new JPanel
    ddPanel.add(ddLabel)
    ddPanel.setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED))
    romPanel.add(setRomButton)
    romPanel.add(pathField)
    romPanel.add(browseButton)
    romPanel.add(ddPanel)
    browseButton.addActionListener(_ => {
      if lastFCPath != null then fc.setCurrentDirectory(lastFCPath)
      fc.showOpenDialog(this) match
        case JFileChooser.APPROVE_OPTION =>
          lastFCPath = fc.getSelectedFile.getParentFile
          pathField.setText(fc.getSelectedFile.toString)
        case _ =>
    })
    setRomButton.addActionListener(_ => {
      if pathField.getText.isEmpty then
        JOptionPane.showMessageDialog(this, "Select a valid ROM path", "ROM path error", JOptionPane.ERROR_MESSAGE)
      else
        setROM(pathField.getText) match
          case Some(model) =>
            romPanel.setBorder(BorderFactory.createTitledBorder(s"ROM - $model"))
          case None =>
    })
    ddPanel.setTransferHandler(new DNDHandler((file, _) => {
      setROM(file.toString) match
        case Some(model) =>
          pathField.setText(file.toString)
          romPanel.setBorder(BorderFactory.createTitledBorder(s"ROM - $model"))
        case None =>
    }))
    romPanel
  end initROMPanel

  private def initSCSIPanel(): JPanel =
    val scsiPanel = new JPanel(new BorderLayout())
    scsiPanel.setBorder(BorderFactory.createTitledBorder("SCSI"))
    val scsiList = new ListPanel(maxItems = 7,6 - _,list => {
      scsiPaths.clear()
      scsiPaths.addAll(list)
    })
    val buttonPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val setButton = new JButton("Set", new ImageIcon(getClass.getResource("/resources/scsi_off.png")))
    setButton.setToolTipText("Add SCSI image")
    val pathField = new JTextField(30)
    val fc = new JFileChooser()
    val browseButton = new JButton("Browse")
    buttonPanel.add(setButton)
    buttonPanel.add(pathField)
    buttonPanel.add(browseButton)
    browseButton.addActionListener(_ => {
      if lastFCPath != null then fc.setCurrentDirectory(lastFCPath)
      fc.showOpenDialog(this) match
        case JFileChooser.APPROVE_OPTION =>
          lastFCPath = fc.getSelectedFile.getParentFile
          pathField.setText(fc.getSelectedFile.toString)
        case _ =>
    })
    setButton.addActionListener(_ => {
      if pathField.getText.isEmpty then
        JOptionPane.showMessageDialog(this, "Select a valid SCSI path", "SCSI path error", JOptionPane.ERROR_MESSAGE)
      scsiList.addItem(pathField.getText)
    })
    buttonPanel.setTransferHandler(new DNDHandler((file, _) => {
      pathField.setText(file.toString)
      scsiList.addItem(file.toString)
    }))
    scsiPanel.add("North",buttonPanel)
    scsiPanel.add("Center",scsiList)
    scsiPanel
  end initSCSIPanel

  private def initFloppyPanel(): JPanel =
    val scsiPanel = new JPanel(new BorderLayout())
    scsiPanel.setBorder(BorderFactory.createTitledBorder("Floppy"))
    val floppyList = new ListPanel(maxItems = 3,i => i, list => {
      floppyPaths.clear()
      floppyPaths.addAll(list)
    })
    val buttonPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val setButton = new JButton("Set", new ImageIcon(getClass.getResource("/resources/trace/save.png")))
    setButton.setToolTipText("Add Floppy image")
    val pathField = new JTextField(30)
    val fc = new JFileChooser()
    val browseButton = new JButton("Browse")
    buttonPanel.add(setButton)
    buttonPanel.add(pathField)
    buttonPanel.add(browseButton)
    browseButton.addActionListener(_ => {
      if lastFCPath != null then fc.setCurrentDirectory(lastFCPath)
      fc.showOpenDialog(this) match
        case JFileChooser.APPROVE_OPTION =>
          lastFCPath = fc.getSelectedFile.getParentFile
          pathField.setText(fc.getSelectedFile.toString)
        case _ =>
    })
    setButton.addActionListener(_ => {
      if pathField.getText.isEmpty then
        JOptionPane.showMessageDialog(this, "Select a valid Floppy path", "Floppy path error", JOptionPane.ERROR_MESSAGE)
      floppyList.addItem(pathField.getText)
    })
    buttonPanel.setTransferHandler(new DNDHandler((file, _) => {
      pathField.setText(file.toString)
      floppyList.addItem(file.toString)
    }))
    scsiPanel.add("North", buttonPanel)
    scsiPanel.add("Center", floppyList)
    scsiPanel
  end initFloppyPanel
