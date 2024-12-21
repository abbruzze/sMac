package ucesoft.mac.debugger

import java.awt.FlowLayout
import java.awt.event.{WindowAdapter, WindowEvent}
import javax.swing.*
import scala.compiletime.uninitialized

/**
 * @author Alessandro Abbruzzetti
 *         Created on 06/11/2023 11:53  
 */
class RefreshableDialog(val frame:JFrame, val title:String, val windowCloseOperation: () => Unit):
  protected var timer: Timer = uninitialized
  protected val periodTF = new JTextField("1000", 5)

  final val dialog = new JDialog(frame, title, false):
    override def setVisible(b: Boolean): Unit =
      super.setVisible(b)
      windowActive(b)
      if b then
        setLocationRelativeTo(frame)

  protected def windowActive(on: Boolean): Unit =
    if !on then
      if timer != null then
        timer.stop()
        timer = null
    else if periodTF.isEnabled then
      setTimer(true)
  
  protected def setTimer(on: Boolean): Unit =
    if timer != null then
      timer.stop()
      timer = null

    if on then
      timer = new Timer(periodTF.getText.toInt, _ => updateModel())
      timer.setRepeats(true)
      timer.start()

  protected def init(): Unit =
    dialog.addWindowListener(new WindowAdapter:
      override def windowClosing(e: WindowEvent): Unit =
        windowActive(false)
        windowCloseOperation()
      override def windowIconified(e: WindowEvent): Unit = windowActive(false)
      override def windowDeiconified(e: WindowEvent): Unit = windowActive(true)
    )
    
  protected def makeRefreshButtonPanel(firstComponents:JComponent*): JPanel =
    val buttonPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    for c <- firstComponents do
      buttonPanel.add(c)
    val autoRefresh = new JCheckBox("Auto refresh")
    val refreshButton = new JButton(new ImageIcon(getClass.getResource("/resources/trace/refresh.png")))

    periodTF.setEnabled(false)
    refreshButton.addActionListener(_ => updateModel())
    autoRefresh.addActionListener(_ => {
      refreshButton.setEnabled(!autoRefresh.isSelected)
      periodTF.setEnabled(autoRefresh.isSelected)
      setTimer(autoRefresh.isSelected)
    })
    periodTF.addActionListener(_ => {
      try
        periodTF.getText.toInt
        setTimer(true)
      catch
        case _: NumberFormatException =>
          periodTF.setText("100")
    })
    buttonPanel.add(autoRefresh)
    buttonPanel.add(refreshButton)
    buttonPanel.add(new JLabel("Auto refresh millis period:", SwingConstants.RIGHT))
    buttonPanel.add(periodTF)
    buttonPanel
    
  protected def updateModel(): Unit = {}