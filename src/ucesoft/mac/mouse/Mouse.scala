package ucesoft.mac.mouse

import ucesoft.mac.{MACComponent, MacModel, MessageBus}
import ucesoft.mac.adb.ADBMouse

import java.awt.{Point, Robot, Toolkit}
import java.awt.event.{MouseEvent, MouseListener, MouseMotionListener, WindowAdapter, WindowEvent}
import java.util
import javax.swing.{JComponent, SwingUtilities}
import scala.compiletime.uninitialized

/**
 * @author Alessandro Abbruzzetti
 *         Created on 18/11/2024 18:52  
 */
trait Mouse extends MACComponent:
  override protected val componentName = "QuadMouse"

  def isReady: Boolean
  def isButtonPressed: Boolean
  def setCapture(on:Boolean,component:JComponent): Unit

class QuadMouse(private var zoomFactorX:Int,private var zoomFactorY:Int) extends Mouse with MouseListener with MouseMotionListener:
  private inline val X = 0
  private inline val Y = 1

  private val adbMouse = new ADBMouse
  private var pressed = false
  private val primaries,secondaries,axes = Array(0,0)
  private var lastX, lastY = -1
  private var captureOn = false
  private val robot = new Robot()
  private var component : JComponent = uninitialized
  private var componentActive = true
  private val emptyCursor = {
    val cursor = new java.awt.image.BufferedImage(16, 16, java.awt.image.BufferedImage.TYPE_INT_ARGB)
    Toolkit.getDefaultToolkit.createCustomCursor(cursor, new Point(0, 0), "null")
  }
  private val windowAdapter = new WindowAdapter:
    override def windowActivated(e: WindowEvent): Unit =
      componentActive = true
      component.setCursor(emptyCursor)
    override def windowDeactivated(e: WindowEvent): Unit =
      componentActive = false
      component.setCursor(new java.awt.Cursor(java.awt.Cursor.DEFAULT_CURSOR))

  override def onMessage(msg: MessageBus.Message): Unit =
    msg match
      case MessageBus.ZoomFactorChanged(source, zx, zy) =>
        zoomFactorX = zx
        zoomFactorY = zy
      case _ =>

  def X1: Int = primaries(X)
  def Y1: Int = primaries(Y)
  def X2: Int = secondaries(X)
  def Y2: Int = secondaries(Y)

  def getADBMouse: ADBMouse = adbMouse

  override protected def reset(): Unit = 
    super.reset()
    lastX = -1
    lastY = -1
    util.Arrays.fill(primaries,0)
    util.Arrays.fill(secondaries, 0)
    util.Arrays.fill(axes, 0)
    pressed = false

  override def setCapture(on:Boolean,comp:JComponent = null): Unit =
    captureOn = on
    component = comp
    if on then
      comp.setCursor(emptyCursor)
      SwingUtilities.getWindowAncestor(comp).addWindowListener(windowAdapter)
    else
      comp.setCursor(new java.awt.Cursor(java.awt.Cursor.DEFAULT_CURSOR))
      SwingUtilities.getWindowAncestor(comp).removeWindowListener(windowAdapter)

  inline private def move(x:Int,y:Int): Unit =
    if macModel.ordinal < MacModel.SE.ordinal then
      axes(X) += x
      axes(Y) += y
    else
      adbMouse.moveDelta(x,y)

  def quad(): Unit =
    var axis = X
    while axis <= Y do
      val axisValue = axes(axis)
      if axisValue != 0 then
        primaries(axis) ^= 1
        secondaries(axis) = primaries(axis) ^ axis
        if axisValue > 0 then
          axes(axis) -= 1
          secondaries(axis) ^= 1
        else
          axes(axis) += 1
      axis += 1

  override def isReady: Boolean = axes(X) != 0 || axes(Y) != 0

  override def isButtonPressed: Boolean = pressed

  override def mousePressed(e: MouseEvent): Unit =
    if !captureOn || !componentActive then return

    pressed = true
    adbMouse.setButtonPressed(true)
  override def mouseReleased(e: MouseEvent): Unit =
    pressed = false
    adbMouse.setButtonPressed(false)

  override def mouseEntered(e: MouseEvent): Unit =
    lastX = e.getX
    lastY = e.getY
  override def mouseExited(e: MouseEvent): Unit =
    if !captureOn || !componentActive then return

    val center = component.getLocationOnScreen
    val dim = component.getSize
    center.x += dim.width / 2
    center.y += dim.height / 2
    robot.mouseMove(center.x,center.y)
    lastX = center.x
    lastY = center.y
  override def mouseClicked(e: MouseEvent): Unit = {}

  override def mouseDragged(e: MouseEvent): Unit = mouseMoved(e)
  override def mouseMoved(e: MouseEvent): Unit =
    if !captureOn || !componentActive then return

    if lastX != -1 then
      val dx = e.getX - lastX
      val dy = e.getY - lastY
      var mx = dx / zoomFactorX
      if dx != 0 && mx == 0 then
        if dx < 0 then mx = -1 else mx = 1
      var my = dy / zoomFactorY
      if dy != 0 && my == 0 then
        if dy < 0 then my = -1 else my = 1
      move(mx,my)

    lastX = e.getX
    lastY = e.getY
