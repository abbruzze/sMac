package ucesoft.mac.misc

import java.awt.{Color, Dimension, GraphicsEnvironment}
import javax.imageio.ImageIO
import javax.swing.{JComponent, JFrame, JOptionPane}

object FullScreenMode:
  case class FullscreenWindow(window:JFrame,component:JComponent,frame:JFrame,originalSize:Dimension,newSize:Dimension)
  def getScreenDeviceIDs: Array[String] =
    val env = GraphicsEnvironment.getLocalGraphicsEnvironment
    env.getScreenDevices.filter(_.isFullScreenSupported).map(_.getIDstring)
  def goFullScreen(screenDeviceIndex:Int,
                   frame:JFrame,
                   component:JComponent,
                   width:Int,
                   height:Int) : Option[FullscreenWindow] =
    val env = GraphicsEnvironment.getLocalGraphicsEnvironment
    val device = env.getScreenDevices()(screenDeviceIndex)
    val conf = device.getDefaultConfiguration
    if device.isFullScreenSupported then
      val window = new JFrame(conf)
      window.getContentPane.setLayout(null)
      window.getContentPane.add(component)
      window.getContentPane.setBackground(Color.BLACK)
      frame.setVisible(false)
      window.setUndecorated(true)
      window.setIconImage(ImageIO.read(getClass.getResourceAsStream("/resources/logo.gif")))
      device.setFullScreenWindow(window)
      val size = conf.getBounds
      val windowWidthFactor = size.width / width.toDouble
      val windowHeightFactor = size.height / height.toDouble
      val factor = math.min(windowWidthFactor,windowHeightFactor)
      val originalSize = component.getSize()
      val newSize = new Dimension((width * factor).toInt,(height * factor).toInt)
      component.setSize(newSize)
      val vicSize = component.getSize()
      val winSize = window.getSize()
      component.setLocation((winSize.width - vicSize.width) / 2,(winSize.height - vicSize.height) / 2)
      component.invalidate()
      window.validate()
      window.setVisible(true)
      window.toFront()
      
      Some(FullscreenWindow(window,component,frame,originalSize,newSize))
    else 
      JOptionPane.showMessageDialog(frame,"Your display device does not support full screen mode","Full Screen Mode",JOptionPane.ERROR_MESSAGE)
      None
      
  def restore(w:FullscreenWindow): Unit =
    w.window.dispose()
    w.frame.setVisible(true)
    w.component.setSize(w.originalSize)
    w.frame.getContentPane.add("Center", w.component)
    w.frame.pack()