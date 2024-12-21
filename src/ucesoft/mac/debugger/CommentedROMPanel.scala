package ucesoft.mac.debugger

import org.fife.ui.rsyntaxtextarea.{RSyntaxTextArea, SyntaxConstants}
import org.fife.ui.rtextarea.RTextScrollPane

import java.awt.{BorderLayout, Dimension, FlowLayout}
import javax.swing.{JLabel, JPanel, JTextField, SwingUtilities}

object CommentedROMPanel:
  def main(args:Array[String]): Unit =
    val f = new javax.swing.JFrame()
    val crp = new CommentedROMPanel
    f.getContentPane.add("Center",crp)

    f.pack()
    f.setVisible(true)
    SwingUtilities.invokeLater(() => crp.load("""G:\My Drive\Emulatori\Macintosh\bigmessowires.com_rom-adapter_plus-rom-listing.asm.asm"""))
    SwingUtilities.invokeLater(() => crp.gotoAddress(0x1BA))
/**
 * @author Alessandro Abbruzzetti
 *         Created on 22/11/2024 16:20  
 */
class CommentedROMPanel extends JPanel:
  private val map = new collection.mutable.HashMap[Int,Int]()
  private val textArea = new RSyntaxTextArea(20,100)
  private val rsp = new RTextScrollPane(textArea)
  private val offset = new JTextField(10)
  init()

  private def init(): Unit =
    setLayout(new BorderLayout())
    textArea.setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_ASSEMBLER_X86)
    textArea.setEditable(false)
    offset.setText("400000")
    val offsetPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    offsetPanel.add(new JLabel("ROM offset:"))
    offsetPanel.add(offset)
    add("Center",rsp)
    add("North",offsetPanel)
    //setMinimumSize(new Dimension(100,0))

  def clear(): Unit =
    textArea.setText("")

  def load(file:String,initialAddress:Int = -1): Unit =
    val src = io.Source.fromFile(file)
    try
      map.clear()
      val lines = src.getLines()
      val addressRE = """([0-9a-fA-F]+)[\s\t].*""".r
      var number = 1
      val sb = new StringBuilder()
      while lines.hasNext do
        val line = lines.next()
        sb.append(line + "\n")
        line match
          case addressRE(a) =>
            try
              map += Integer.parseInt(a,16) -> number
            catch
              case _:NumberFormatException =>
          case _ =>
        number += 1
      textArea.setText(sb.toString)
      if initialAddress == -1 then
        textArea.setCaretPosition(textArea.getLineStartOffset(0))
      else
        gotoAddress(initialAddress)
    finally
      src.close()

  def gotoAddress(address:Int): Unit =
    val romOffset = try
      Integer.parseInt(offset.getText,16)
    catch
      case _:NumberFormatException =>
        0x40_0000
    map.get(address - romOffset) match
      case Some(line) =>
        textArea.setCaretPosition(textArea.getLineStartOffset(line - 1))
      case None =>
