package ucesoft.mac.video

import ucesoft.mac.{Display, MACComponent, MacModel}

import javax.swing.ImageIcon
import scala.compiletime.uninitialized

object MacVideo:
  trait VideoSignalListener:
    def onVBlank(on:Boolean): Unit
    def onHBlank(on:Boolean,vblank:Boolean,videoLine:Int): Unit
  trait BusAcquiring:
    def acquire(acquire:Boolean): Unit

/**
 * @author Alessandro Abbruzzetti
 *         Created on 13/11/2024 14:53  
 */
class MacVideo extends MACComponent:
  import MacVideo.*
  override protected val componentName = "Video"
  override protected val icon = new ImageIcon(getClass.getResource("/resources/trace/monitor.png"))

  inline private val BLACK = 0xFF_000000
  inline private val WHITE = 0xFF_FFFFFF
  private final val PALETTE = Array(WHITE,BLACK)

  private var ram : Array[Byte] = Array()
  private var videoBufferOffset = macModel.videoSettings.videoBufferAddress
  private var videoBufferByteCounter = 0
  private var videoPixels : Array[Int] = Array()
  private var display: Display = uninitialized

  private var videoListeners : List[VideoSignalListener] = Nil
  private var busAcquiring: BusAcquiring = uninitialized

  private var vblank = true
  private var hblank = false

  private var rasterLine = 0
  private var videoLine = -1
  private var linePixels = 0

  private var readCycle = 0
  private var readLong = false
  private var readCycles = 0
  private var shiftMask = 0L
  private var pixelShiftRegister = 0L // 16/32 pixel shift register (1 word/long)

  override def getProperties: List[MACComponent.Property] =
    import MACComponent.Property
    Property("Raster line",rasterLine.toString) ::
    Property("Video line",videoLine.toString) ::
    Property("Line pixels",linePixels.toString) ::
    Property("Read cycles",readCycles.toString) ::
    Property("VBlank",vblank.toString) ::
    Property("HBlank",hblank.toString) ::
    Property("Video buffer address","%06X".format(videoBufferOffset)) ::
    Property("Video buffer counter",videoBufferByteCounter.toString) ::
    Nil

  def setBusAcquiring(ba:BusAcquiring): Unit =
    busAcquiring = ba
  override protected def setModel(model:MacModel): Unit =
    super.setModel(model)
    videoBufferOffset = macModel.videoSettings.videoBufferAddress
    if model.ordinal < MacModel.SE.ordinal then
      readCycle = 4
      readLong = false
      shiftMask = 0x10000
    else
      readCycle = 8
      readLong = true
      shiftMask = 0x10000_0000L
  def setRAM(ram:Array[Byte]): Unit =
    this.ram = ram
  def selectVideoBuffer(alternate:Boolean): Unit =
    if alternate then
      videoBufferOffset = macModel.videoSettings.alternateVideoBufferAddress
    else
      videoBufferOffset = macModel.videoSettings.videoBufferAddress
  def addVideoSignalListener(l:VideoSignalListener): Unit =
    videoListeners ::= l
  def removeVideoSignalListener(l:VideoSignalListener): Unit =
    videoListeners = videoListeners.filterNot(_ == l)
  def setDisplay(d:Display): Unit =
    display = d
    videoPixels = d.displayMem

  private def notifyVBlank(on:Boolean): Unit =
    var ptr = videoListeners
    while ptr.nonEmpty do
      ptr.head.onVBlank(on)
      ptr = ptr.tail
  private def notifyHBlank(on: Boolean): Unit =
    var ptr = videoListeners
    while ptr.nonEmpty do
      ptr.head.onHBlank(on,vblank = vblank,videoLine = rasterLine)
      ptr = ptr.tail

  inline private def shiftPixel(): 0|1 =
    pixelShiftRegister <<= 1
    if (pixelShiftRegister & shiftMask) != 0 then 1 else 0

  inline private def draw2Pixels(): Unit =
    val pixelPos = videoLine * macModel.videoSettings.horizontalPixels + linePixels
    videoPixels(pixelPos) = PALETTE(shiftPixel())
    videoPixels(pixelPos + 1) = PALETTE(shiftPixel())

  private def readData(): Unit =
    val ramOffset = (videoBufferOffset + videoBufferByteCounter) & (ram.length - 1)
    if readLong then
      pixelShiftRegister = (ram(ramOffset) & 0xFF) << 24 | (ram(ramOffset + 1) & 0xFF) << 16 | (ram(ramOffset + 2) & 0xFF) << 8 | (ram(ramOffset + 3) & 0xFF)
      videoBufferByteCounter += 4
    else
      pixelShiftRegister = (ram(ramOffset) & 0xFF) << 8 | (ram(ramOffset + 1) & 0xFF)
      videoBufferByteCounter += 2

  inline private def acquireBUS(acquire:Boolean): Unit =
    if busAcquiring != null then busAcquiring.acquire(acquire)

  private def goNextLine(): Unit =
    hblank = false
    linePixels = 0
    rasterLine += 1
    if !vblank then
      videoLine += 1
    notifyHBlank(on = false)

    if rasterLine == macModel.videoSettings.totalLines then
      videoBufferByteCounter = 0
      rasterLine = 0
      videoLine = -1
      vblank = true
      display.showFrame()
      notifyVBlank(on = true)
    else if rasterLine == macModel.videoSettings.vblankLines then
      readData() // read the first word of the frame
      videoLine = 0
      vblank = false
      notifyVBlank(on = false)

  def cycle(): Unit =
    if !vblank && !hblank then // active area
      draw2Pixels()
      readCycles += 1
      if readCycles == readCycle then acquireBUS(acquire = true)
      else if readCycles == (readCycle << 1) then
        readData()
        acquireBUS(acquire = false)
        readCycles = 0

    linePixels += 2
    if hblank then
      if linePixels == macModel.videoSettings.totalHorizontalPixels then
        goNextLine()
    else
      if linePixels == macModel.videoSettings.horizontalPixels then // entering in hblank
        hblank = true
        notifyHBlank(on = true)
  end cycle





