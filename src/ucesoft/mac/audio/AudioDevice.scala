package ucesoft.mac.audio

import ucesoft.mac.{MACComponent, MacModel}

import java.util.concurrent.LinkedBlockingDeque
import javax.sound.sampled.*
import javax.swing.ImageIcon

class AudioDevice(sampleRate:Int) extends MACComponent with Runnable with Audio:
  override protected val componentName = "Audio"
  override protected val icon = new ImageIcon(getClass.getResource("/resources/trace/audio.png"))

  private val queue = new LinkedBlockingDeque[Array[Byte]]
  private var bufferSize = 0

  private var buffer: Array[Byte] = Array()
  private var bufferPendingSize = -1

  private var bufferId = 0
  private var bufferPos = 0
  private var bufferInMillis = 10
  private val thread = new Thread(this,s"AudioDevice")
  private var muted = false
  private var sourceLine : SourceDataLine = scala.compiletime.uninitialized
  private var volumeLine : FloatControl = scala.compiletime.uninitialized
  private var masterVolume = 0
  private var volumeLevel = 0
  private var stopped = false
  private var lastPerformance = 0
  private var alternateAudioBuffer = false

  setBufferMillisNow(bufferInMillis)
  thread.setPriority(Thread.MAX_PRIORITY)

  override def getProperties: List[MACComponent.Property] =
    import MACComponent.Property
    List(
      Property("Muted",muted.toString),
      Property("Buffer millis",bufferInMillis.toString),
      Property("Master volume",masterVolume.toString),
      Property("Volume level",volumeLevel.toString),
      Property("Alternate buffer",alternateAudioBuffer.toString),
      Property("Performance",lastPerformance.toString)
    )

  private def setBufferMillisNow(bim:Int): Unit =
    setBufferInMillis(bim)
    bufferSize = bufferPendingSize
    buffer = Array.ofDim[Byte](bufferSize)
    bufferPendingSize = -1

  def setBufferInMillis(bim:Int) : Unit =
    bufferInMillis = bim
    val scale = 1 // mono
    bufferPendingSize = scale * (sampleRate * bim / 1000.0).toInt

  override protected def reset(): Unit =
    queue.clear()

  inline private def dequeue() : Array[Byte] = queue.take()

  def start(): Unit =
    if !thread.isAlive then thread.start()

  def isMuted : Boolean = muted

  def mute(muted:Boolean) : Unit =
    this.muted = muted

  def stop(): Unit =
    stopped = true

  inline private def getSourceLine: Option[SourceDataLine] =
    try
      val format = new AudioFormat(sampleRate.toFloat, 8 ,1, false, false)

      val info = new DataLine.Info(classOf[SourceDataLine], format)
      val sourceLine = AudioSystem.getLine(info).asInstanceOf[SourceDataLine]
      try
        val name = sourceLine.getClass.getSuperclass.getCanonicalName
        if name == "com.sun.media.sound.DirectAudioDevice.DirectDL" then
          val f = sourceLine.getClass.getSuperclass.getDeclaredField("waitTime")
          f.setAccessible(true)
          f.set(sourceLine,1)
      catch
        case e:Exception =>
          println(s"Cannot initialize audio: $e")

      sourceLine.open(format)

      volumeLine = sourceLine.getControl(FloatControl.Type.MASTER_GAIN).asInstanceOf[FloatControl]
      setMasterVolume(0)

      sourceLine.start()
      Some(sourceLine)
    catch
      case t: Throwable =>
        t.printStackTrace()
        None

  def setMasterVolume(v: Int): Unit =
    if volumeLine != null then
      val max = volumeLine.getMaximum
      val min = volumeLine.getMinimum / 2f
      volumeLine.setValue((v / 100.0f) * (max - min) + min)
      masterVolume = v

  def getMasterVolume: Int = masterVolume
  def available(): Int = if sourceLine == null then 0 else sourceLine.available()
  def getLastPerformance: Int = lastPerformance

  override def run(): Unit =
    getSourceLine match
      case Some(sl) =>
        sourceLine = sl
        log.info("Audio System started")
        while !stopped do
          val samples = queue.take()
          val available = sourceLine.available()
          if available >= samples.length then
            sourceLine.write(samples, 0, samples.length)
            lastPerformance = 100
          else
            sourceLine.write(samples, 0, available)
            lastPerformance = (available / samples.length * 100.0).toInt
            //println(s"Perf $lastPerformance samples=${samples.length} available=$available")

        sourceLine.drain()
        sourceLine.close()
        sourceLine = null
        log.info("Audio System stopped")
      case None =>
        log.error("Cannot initialize audio system")

  override def turn(on: Boolean): Unit =
    log.info("Audio is %s",if on then "on" else "off")
    muted = !on
  override def setVolumeLevel(level: Int): Unit =
    volumeLevel = level
    val volume = (level / 7.0 * 100.0).toInt
    log.info("Audio: setting volume level %d: volume %% is %d",level,volume)
    setMasterVolume(volume)
  override def getVolumeLevel: Int = volumeLevel
  override def setAlternateAudioBuffer(alternateOn: Boolean): Unit =
    log.info("Audio alternate audio buffer %b",alternateOn)
    alternateAudioBuffer = alternateOn
  override def getAudioBufferAddress: Int =
    if alternateAudioBuffer then
      macModel.audioSettings.alternateAudioBufferAddress
    else
      macModel.audioSettings.audioBufferAddress

  override def newSample(sample: Byte): Unit =
    buffer(bufferId) = if muted || volumeLevel == 0 then 0 else sample ; bufferId += 1
    if bufferId == bufferSize then
      queue.put(buffer)
      if bufferPendingSize != -1 then
        bufferSize = bufferPendingSize
        bufferPendingSize = -1
      buffer = Array.ofDim[Byte](bufferSize)
      bufferId = 0