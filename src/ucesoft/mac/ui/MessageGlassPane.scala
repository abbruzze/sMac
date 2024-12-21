package ucesoft.mac.ui

import java.awt.event.{ComponentAdapter, ComponentEvent, ComponentListener}
import java.awt.{Color, Font, Graphics}
import java.util.concurrent
import java.util.concurrent.{CountDownLatch, Executors, LinkedBlockingDeque}
import javax.swing.*
import scala.compiletime.uninitialized

/**
 * @author Alessandro Abbruzzetti
 *         Created on 13/12/2023 15:59  
 */
class MessageGlassPane(private var frame:JFrame) extends ComponentListener with Runnable with MessageBoard.MessageBoardListener:
  import MessageBoard.*
  private inline val FONT_WINDOW_WIDTH_RATIO = 30.0f
  private var xoff, yoff = 0
  private var msg : Message = uninitialized
  private var enabled = true
  private val queue = new LinkedBlockingDeque[Message]
  private val thread = new Thread(this,"MessageGlassThread")
  private val panelReadyWait = new CountDownLatch(1)
  private val messageTimerExecutor = Executors.newFixedThreadPool(1)
  private val font = new JLabel().getFont
  private var level = MessageLevel.ADMIN
  private var interrupted = false
  private val interruptedLock = new Object
  private val glassPane = new JPanel:
    setOpaque(false)
      
  def start(): Unit =
    changeFrame(frame)
    thread.start()

  def changeFrame(f:JFrame): Unit =
    frame = f
    frame.addComponentListener(this)
    frame.getRootPane.addComponentListener(new ComponentAdapter:
      override def componentResized(e: ComponentEvent): Unit =
        if enabled then
          renderMessage()
    )

  def run(): Unit =
    panelReadyWait.await()
    while true do
      try
        msg = queue.take()
        if !Thread.currentThread().isInterrupted then renderMessage(startTimer = true)
      catch
        case _:InterruptedException =>
      
  override def interrupt(): Unit =
    queue.clear()
    interruptedLock.synchronized {
      interrupted = true
      interruptedLock.notify()
    }

  override def setLevel(level: MessageLevel): Unit =
    this.level = level

  override def addMessage(msg:Message): Unit =
    if level.accept(msg.messageLevel) then
      queue.offer(msg)
      
  def getReady(): Unit =
    panelReadyWait.await()

  private def initPane(): Unit =
    val insets = frame.getContentPane.getBounds
    frame.setGlassPane(glassPane)
    glassPane.setLayout(null)
    glassPane.setVisible(true)
    xoff = insets.x
    yoff = insets.y
    panelReadyWait.countDown()

  private def renderMessage(startTimer:Boolean = false): Unit =
    glassPane.removeAll()

    if msg != null then
      val fsize = frame.getSize
      val fontSize = fsize.width / FONT_WINDOW_WIDTH_RATIO
      val label = new JLabel(msg.text)
      label.setForeground(msg.color.getOrElse(Color.WHITE))
      var labelFont = msg.font match
        case Some(f) =>
          Font.decode(f)
        case None =>
          font
      labelFont = labelFont.deriveFont(fontSize)
      if msg.bold then
        labelFont = labelFont.deriveFont(Font.BOLD)
      if msg.italic then
        labelFont = labelFont.deriveFont(Font.ITALIC)
      label.setFont(labelFont)
      val fm = label.getFontMetrics(label.getFont)
      glassPane.add(label)

      var y = msg.ypos match
        case YPOS.TOP =>
          yoff
        case YPOS.BOTTOM =>
          fsize.height - fm.getHeight - fm.getDescent
        case YPOS.CENTER =>
          (fsize.height - fm.getHeight) / 2
      y += msg.yoffset * fm.getHeight
      val x = msg.xpos match
        case XPOS.LEFT =>
          0
        case XPOS.RIGHT =>
          fsize.width - (fm.stringWidth(msg.text) * 1.3).toInt
        case XPOS.CENTER =>
          (fsize.width - fm.stringWidth(msg.text)) / 2

      val msgSize = label.getPreferredSize
      label.setBounds(xoff + x,y - yoff,msgSize.width,msgSize.height)
      glassPane.repaint()

      if startTimer then
        try
          val sleep = if msg.millis == -1 then
            Int.MaxValue
          else
            msg.millis
          interruptedLock.synchronized {
            if !interrupted then
              interruptedLock.wait(sleep)
          }
        catch
          case _:InterruptedException =>
        val fadingMillis = msg.fadingMillis.getOrElse(0)
        if fadingMillis > 0 then
          val sleep = fadingMillis / 256
          var color = label.getForeground
          var c = 255
          while c >= 0 do
            color = new Color(color.getRed, color.getGreen, color.getBlue, c)
            label.setForeground(color)
            interruptedLock.synchronized {
              if !interrupted then
                interruptedLock.wait(sleep)
            }
            c -= 1
        panelReadyWait.countDown()
        msg = null
        SwingUtilities.invokeAndWait(() => {
          glassPane.removeAll()
          glassPane.repaint()
        })
      interrupted = false
  end renderMessage

  override def enableMessages(enabled:Boolean): Unit =
    this.enabled = enabled
    frame.getGlassPane.asInstanceOf[JPanel].setVisible(enabled)

  override def componentResized(e: ComponentEvent): Unit = {}
  override def componentMoved(e: ComponentEvent): Unit = {}
  override def componentShown(e: ComponentEvent): Unit =
    initPane()
    frame.removeComponentListener(this)
  override def componentHidden(e: ComponentEvent): Unit = {}

