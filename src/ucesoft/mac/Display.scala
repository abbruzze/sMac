package ucesoft.mac

import java.awt.*
import java.awt.event.{MouseEvent, MouseListener, MouseMotionListener}
import java.awt.image.{BufferedImage, MemoryImageSource}
import java.io.File
import javax.imageio.ImageIO
import javax.swing.*

class Display(width: Int, height: Int, title: String, frame: JFrame, clk:Clock) extends JComponent with MouseMotionListener with MouseListener with MACComponent {
  private val dimension = new Dimension(0, 0)
  private var dashIndex = 0
  private val mouseZoomStartPoint,mouseZoomEndPoint = new Point
  private val mouseZoomLineColors = Array(Color.WHITE,Color.YELLOW, Color.RED)
  private var mouseZoomColorIndex = 0
  private var mouseZoomEnabled = false
  private var clipArea,zoomArea: (Point, Point) = scala.compiletime.uninitialized
  private var totalFrameCounter,frameCounter = 0L
  private var framePerSecond = 0.0
  private var ts = 0L
  private var normalDisplayMem = Array.fill(width * height)(0xFF000000)
  private var interlacedDisplayMem = Array.fill(width * height * 2)(0xFF000000)
  private var ptrDisplayMem = normalDisplayMem
  private var normalDisplayImage = new MemoryImageSource(width, height, normalDisplayMem, 0, width)
  private var interlacedDisplayImage = new MemoryImageSource(width, height * 2, interlacedDisplayMem, 0, width)
  private var displayImage = normalDisplayImage
  private var normalScreen = {
    normalDisplayImage.setAnimated(true)
    normalDisplayImage.setFullBufferUpdates(false)
    createImage(normalDisplayImage)
  }
  private var interlacedScreen = {
    interlacedDisplayImage.setAnimated(true)
    interlacedDisplayImage.setFullBufferUpdates(false)
    createImage(interlacedDisplayImage)
  }
  private var screen = normalScreen
  private var drawRasterLine = false
  private var rasterLine = 0
  private var lpX, lpY = 0
  private var zoomFactorX,zoomFactorY = 0.0
  private var interlaced = false
  private var renderingHints = RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR

  private var rotationAngleRad = 0.0
  private var flipX,flipY = false

  private var singleFrameMode = false
  private var singleFrameCounter = 0
  private val singleFrameModeMonitor = new Object

  private var waitFrameAndSaveSnapshotFile : File = scala.compiletime.uninitialized
  private var waitFrameAndSaveSnapshotCounter = 0
  private var waitFrameAndSaveSnapshotCallback : () => Unit = scala.compiletime.uninitialized

  addMouseMotionListener(this)
  addMouseListener(this)

  def setSingleFrameMode(sfm:Boolean) : Unit = {
    singleFrameMode = sfm
    singleFrameCounter = 0
    if (!singleFrameMode) advanceOneFrame()
  }

  def advanceOneFrame() : Unit = singleFrameModeMonitor.synchronized {
    singleFrameModeMonitor.notifyAll()
  }

  def setRotationAngle(angleInDeg:Double) : Unit = {
    rotationAngleRad = math.toRadians(angleInDeg)
    repaint()
  }

  def setFlipXY(flipX:Boolean,flipY:Boolean) : Unit = {
    this.flipX = flipX
    this.flipY = flipY
    repaint()
  }

  def setNewResolution(height:Int,width:Int) : Unit = {
    log.info("New resolution: %d x %d",width,height)
    normalDisplayMem = Array.fill(width * height)(0xFF000000)
    interlacedDisplayMem = Array.fill(width * height * 2)(0xFF000000)
    normalDisplayImage = new MemoryImageSource(width, height, normalDisplayMem, 0, width)
    interlacedDisplayImage = new MemoryImageSource(width, height * 2, interlacedDisplayMem, 0, width)
    normalDisplayImage.setAnimated(true)
    normalDisplayImage.setFullBufferUpdates(false)
    normalScreen = createImage(normalDisplayImage)
    interlacedDisplayImage.setAnimated(true)
    interlacedDisplayImage.setFullBufferUpdates(false)
    interlacedScreen = createImage(interlacedDisplayImage)
    setInterlaceMode(interlaced)
  }
  
  def setRenderingHints(hints:java.lang.Object) : Unit = {
    renderingHints = hints
  }
  
  def displayMem : Array[Int] = ptrDisplayMem
  def setInterlaceMode(enabled:Boolean) : Unit = {
    interlaced = enabled
    if (enabled) {
      ptrDisplayMem = interlacedDisplayMem
      displayImage = interlacedDisplayImage
      screen = interlacedScreen
    }
    else {
      ptrDisplayMem = normalDisplayMem
      displayImage = normalDisplayImage
      screen = normalScreen
    }
  }

  inline private def isZoomEvent(e:MouseEvent) : Boolean = {
    import java.awt.event.InputEvent.*
    val zoomMod = SHIFT_DOWN_MASK | CTRL_DOWN_MASK
    (e.getModifiersEx & zoomMod) == zoomMod
  }

  def mouseClicked(e:MouseEvent) : Unit = {}
  def mousePressed(e:MouseEvent) : Unit = {
    if (isZoomEvent(e)) {
      if (SwingUtilities.isRightMouseButton(e)) {
        mouseZoomEnabled = false
        zoomArea = null
        repaint()
      }
      else {
        setCursor(new java.awt.Cursor(java.awt.Cursor.CROSSHAIR_CURSOR))
        mouseZoomEnabled = true
        mouseZoomStartPoint.x = e.getX
        mouseZoomStartPoint.y = e.getY
        mouseZoomEndPoint.x = e.getX
        mouseZoomEndPoint.y = e.getY
      }
    }
  }
  def mouseReleased(e:MouseEvent) : Unit = {
    if (mouseZoomEnabled) {
      setCursor(new java.awt.Cursor(java.awt.Cursor.DEFAULT_CURSOR))
      mouseZoomEnabled = false
      val zoomR = zoomRec
      val interlacedFactor = if (interlaced) 2.0 else 1.0
      zoomArea = (new Point((zoomR.x / zoomFactorX).toInt, (zoomR.y / zoomFactorY * interlacedFactor).toInt), new Point(((zoomR.x + zoomR.width) / zoomFactorX).toInt,((zoomR.y + zoomR.height) / zoomFactorY * interlacedFactor).toInt))
      repaint()
    }
  }
  def mouseEntered(e:MouseEvent) : Unit = {}
  def mouseExited(e:MouseEvent) : Unit = {}
  
  // light pen events
  def mouseDragged(e:MouseEvent) : Unit = { mouseMoved(e) }
  def mouseMoved(e:MouseEvent) : Unit = {
    lpX = (e.getX / zoomFactorX).toInt
    lpY = (e.getY / zoomFactorY).toInt

    if (mouseZoomEnabled) {
      mouseZoomEndPoint.x = e.getX
      mouseZoomEndPoint.y = e.getY
    }
  }
  
  def getLightPenX: Int = lpX
  def getLightPenY: Int = lpY
  
  def getFrameCounter: Long = totalFrameCounter
  
  def getClipArea: (Point, Point) = clipArea

  def setClipArea(clip:(Int,Int,Int,Int)): Unit =
    setClipArea(clip._1,clip._2,clip._3,clip._4)
  def setClipArea(x1: Int, y1: Int, x2: Int, y2: Int) : Unit = {
    clipArea = (new Point(x1, y1), new Point(x2, y2))
    recalcZoomFactors()
  }
  private def recalcZoomFactors(): Unit = {
    zoomFactorX = dimension.width.toDouble / (if (clipArea != null) clipArea._2.x - clipArea._1.x else screen.getWidth(null))
    zoomFactorY = dimension.height.toDouble / (if (clipArea != null) clipArea._2.y - clipArea._1.y else screen.getHeight(null))
  }
  def removeClipArea(): Option[(Point,Point)] = {
    val clip = clipArea
    clipArea = null
    Option(clip)
  }

  final override def update(g: Graphics) : Unit = {
    paint(g)
  }

  def setDrawRasterLine(drawRasterLine: Boolean) : Unit = this.drawRasterLine = drawRasterLine
  def setRasterLineAt(rasterLine: Int) : Unit ={
    this.rasterLine = rasterLine
    repaint()
  }
  def getRasterLine: Int = rasterLine

  override final def paint(g: Graphics) : Unit = {
    if (dimension.width != getWidth || dimension.height != getHeight) {
      dimension.width = getWidth
      dimension.height = getHeight
      log.info("New screen dimension %d x %d",dimension.width,dimension.height)
      recalcZoomFactors()
      //println(s"New screen dimension ${dimension.width} x ${dimension.height} width/height=${dimension.width.toDouble/dimension.height} zoomFactorX=$zoomFactorX zoomFactorY=$zoomFactorY")
    }
    // interpolation
    g.asInstanceOf[Graphics2D].setRenderingHint(RenderingHints.KEY_INTERPOLATION,renderingHints)
    // rotation
    if (rotationAngleRad != 0) g.asInstanceOf[Graphics2D].rotate(rotationAngleRad,dimension.width >> 1,dimension.height >> 1)
    // clipping
    var clip : Tuple2[Point,Point] = null
    if (clipArea != null) clip = clipArea
    if (zoomArea != null) {
      if (clip == null) clip = zoomArea
      else 
      clip = (new Point(clip._1.x + zoomArea._1.x,clip._1.y + zoomArea._1.y),new Point(clip._1.x + zoomArea._2.x,clip._1.y + zoomArea._2.y)) 
    }
    // flipping
    if (flipX) clip = (new Point(clip._2.x,clip._1.y),new Point(clip._1.x,clip._2.y))
    if (flipY) clip = (new Point(clip._1.x,clip._2.y),new Point(clip._2.x,clip._1.y))

    if (clip == null) g.drawImage(screen, 0, 0, dimension.width, dimension.height, null)
    else g.drawImage(screen, 0, 0, dimension.width, dimension.height, clip._1.x, clip._1.y, clip._2.x, clip._2.y, null)

    if (drawRasterLine) {
      g.setColor(Color.RED)
      if (clip == null) g.drawLine(0, rasterLine, dimension.width, rasterLine)
      else //g.drawLine(0, ((rasterLine - clip._1.y) * zoomFactorY).toInt, dimension.width, ((rasterLine - clip._1.y) * zoomFactorY).toInt)
      g.fillRect(0, ((rasterLine - clip._1.y) * zoomFactorY).toInt, dimension.width,zoomFactorY.toInt)
    }
    if (mouseZoomEnabled) {
      if ((totalFrameCounter % 10) == 0) mouseZoomColorIndex = (mouseZoomColorIndex + 1) % mouseZoomLineColors.length
      g.setColor(mouseZoomLineColors(mouseZoomColorIndex))
      val zoomR = zoomRec   
      dashIndex = (dashIndex + 1) % 10
      val dash = new BasicStroke(2, BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL,10, Array(10.0f),dashIndex.toFloat)
      g.asInstanceOf[Graphics2D].setStroke(dash)
      g.drawRect(zoomR.x,zoomR.y,zoomR.width,zoomR.height)
    }
  }
  
  @inline private def zoomRec : Rectangle = {
    val zx = if (mouseZoomStartPoint.x < mouseZoomEndPoint.x) mouseZoomStartPoint.x else mouseZoomEndPoint.x
    val zy = if (mouseZoomStartPoint.y < mouseZoomEndPoint.y) mouseZoomStartPoint.y else mouseZoomEndPoint.y
    val dx = math.abs(mouseZoomStartPoint.x - mouseZoomEndPoint.x)
    val dy = math.abs(mouseZoomStartPoint.y - mouseZoomEndPoint.y)
    new Rectangle(zx,zy,dx,dy)
  }
  
  final def showFrame(): Unit = showFrame(0,0,0,0,newPixels = true)
  final def showFrame(x1: Int, y1: Int, x2: Int, y2: Int,newPixels:Boolean = false,updateFrameRateOnly:Boolean = false) : Unit = {
    if newPixels then
      displayImage.newPixels()
      repaint()
    else if !updateFrameRateOnly then
      displayImage.newPixels(x1, y1, x2, y2)      
      repaint()

    if (waitFrameAndSaveSnapshotFile != null) {
      if (waitFrameAndSaveSnapshotCounter < 2) waitFrameAndSaveSnapshotCounter += 1
      else {
        saveSnapshot(waitFrameAndSaveSnapshotFile)
        waitFrameAndSaveSnapshotCallback()
        waitFrameAndSaveSnapshotCounter = 0
        waitFrameAndSaveSnapshotFile = null
      }
    }

    frameCounter += 1
    totalFrameCounter += 1
    if (singleFrameMode) {
      singleFrameCounter += 1
      frame.setTitle(s"$title - single frame mode ($singleFrameCounter)")
      singleFrameModeMonitor.synchronized {
        singleFrameModeMonitor.wait()
      }
    }
    else {
      val now = System.currentTimeMillis
      if (ts == 0 || now - ts > 1000) {
        framePerSecond = frameCounter / ((now - ts) / 1000.0)
        ts = now
        frameCounter = 0
        frame.setTitle("%s - %.2ffps - %d%%".format(title,framePerSecond,clk.getLastPerformance))
      }
    }
  }

  def setPaused() : Unit = {
    frame.setTitle(s"$title - paused")
  }

  def lastFramePerSecondCounter: Int = framePerSecond.toInt

  def saveSnapshot(file: File) : Unit = {
    val snap = createImage(getSize().width, getSize().height).asInstanceOf[BufferedImage]
    paint(snap.getGraphics)
    ImageIO.write(snap, "png", file)
  }

  def getSnapshot(_targetWidth:Int,_targetHeight:Int): BufferedImage =
    val size = getSize
    val ratio = size.width.toDouble / size.height
    var targetWidth = size.width
    var targetHeight = size.height
    if _targetWidth == -1 && _targetHeight != -1 then
      targetWidth = (ratio * _targetHeight).toInt
      targetHeight = _targetHeight
    else if _targetWidth != -1 && _targetHeight == -1 then
      targetHeight = (_targetWidth / ratio).toInt
      targetWidth = _targetWidth
    val resizedImage = new BufferedImage(targetWidth, targetHeight, BufferedImage.TYPE_INT_RGB)
    val graphics2D = resizedImage.createGraphics()
    graphics2D.drawImage(screen, 0, 0, targetWidth, targetHeight, null)
    graphics2D.dispose()
    resizedImage

  def blankVideo(): Unit =
    java.util.Arrays.fill(ptrDisplayMem,0xFF000000)
    showFrame()
    repaint()

  def waitFrameSaveSnapshot(file:File, callback:() => Unit) : Unit = {
    waitFrameAndSaveSnapshotFile = file
    waitFrameAndSaveSnapshotCallback = callback
  }
}