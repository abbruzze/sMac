package ucesoft.mac.storage

import ucesoft.mac.MACComponent

import scala.compiletime.uninitialized

/**
 * @author Alessandro Abbruzzetti
 *         Created on 07/11/2024 18:15  
 */
abstract class Drive(val doubleSide: Boolean,val present: Boolean) extends MACComponent:
  override protected val componentName = "Drive"
  protected enum HeadStepDirection:
    case Up, Down

  protected var floppy : DiskImage = uninitialized
  protected var motorOn = false
  protected var headStepDirection : HeadStepDirection = HeadStepDirection.Up
  protected var trackNumber, steppedTrackNumber = 0
  protected var flushFloppyOnEject = true

  override protected def reset(): Unit =
    super.reset()
    motorOn = false
    headStepDirection = HeadStepDirection.Up

  override protected def hardReset(): Unit =
    super.hardReset()
    if floppy != null then
      for t <- 0 until floppy.getTrackCount do
        for side <- 0 to 1 do
          floppy.getTrack(side,t).resetPositionTo()

  def setFlushFloppyOnEject(flush:Boolean): Unit = flushFloppyOnEject = flush
  def isFloppyIn: Boolean = floppy != null
  def getFloppy: Option[DiskImage] = Option(floppy)
  def getFloppySides: Int = if floppy != null then floppy.getHeadCount else 1 
  def insertFloppy(newFloppy:DiskImage): Unit =
    if floppy != null then
      floppy.eject(flush = flushFloppyOnEject)
    floppy = newFloppy
    trackNumber = 0
    headStepDirection = HeadStepDirection.Up
  def ejectFloppy(): Unit =
    if floppy != null then
      log.info("Ejecting floppy: motor=%s",isMotorOn)
      floppy.eject(flush = flushFloppyOnEject)
      floppy = null
  def isMotorOn: Boolean = motorOn
  def setMotorOn(on:Boolean): Unit = motorOn = on && present && floppy != null

  def stepHead(): Unit
  def isSteppingUp: Boolean = headStepDirection == HeadStepDirection.Up
  def setStepDirection(stepUP:Boolean): Unit =
    headStepDirection = if stepUP then HeadStepDirection.Up else HeadStepDirection.Down
  def isStepping: Boolean
  def getTrack: TrackPos = trackNumber
  def getBitOnHead(head:Int,moveAhead:Boolean): Boolean
  def setBitOnHeadAndMoveAhead(head:Int,bit:Boolean): Unit

  def cycle(): Unit

class MacDrive(val driveIndex:Int,val clockSpeed:Int,override val doubleSide: Boolean,override val present:Boolean,trackChangeListener:IWM.DiskControllerListener) extends Drive(doubleSide,present):
  override protected val componentName = s"Drive #$driveIndex"
  private final val PWM_VALUE_TO_LEN = Array(
    0,  1, 59,  2, 60, 40, 54,  3,
    61, 32, 49, 41, 55, 19, 35,  4,
    62, 52, 30, 33, 50, 12, 14, 42,
    56, 16, 27, 20, 36, 23, 44,  5,
    63, 58, 39, 53, 31, 48, 18, 34,
    51, 29, 11, 13, 15, 26, 22, 43,
    57, 38, 47, 17, 28, 10, 25, 21,
    37, 46,  9, 24, 45,  8,  7,  6
  )
  private final val STEPPING_CYCLES = (clockSpeed / 1000.0 * 12).toInt // 12ms
  private var pwm_avg_sum, pwm_avg_count = 0
  private var pwm_dutycycle = 0.0f
  private var stepping = 0
  private var cycles = 0L

  override protected def reset(): Unit =
    super.reset()
    pwm_avg_sum = 0
    pwm_avg_count = 0
    pwm_dutycycle = 0
    stepping = 0
    cycles = 0

  override def insertFloppy(newFloppy:DiskImage): Unit =
    super.insertFloppy(newFloppy)
    trackChangeListener.onTrackChanged(driveIndex,trackNumber)

  private def getTrackRPM: Int =
    if doubleSide then
      trackNumber >> 4 match
        case 0 => 393//402
        case 1 => 429//438
        case 2 => 472//482
        case 3 => 524//536
        case 4 => 590//603
    else
      ((pwm_dutycycle - 0.094f) * (702.5f - 342.5f) / (0.91f - 0.094f) + 342.5f).toInt

  def getTACH: Boolean =
    val rpm = getTrackRPM
    if !isMotorOn || rpm == 0 then return false

    // TACH: 60 pulses per minute
    val pulsesPerMinute = rpm * 60
    val edgesPerMinute = pulsesPerMinute << 1
    val clocksPerMinute = clockSpeed * 60
    val clocksPerEdge = clocksPerMinute / edgesPerMinute
    ((cycles / clocksPerEdge) & 1) != 0

  def updatePWMDutyCycle(_pwm:Int): Unit =
    if doubleSide then
      return

    val pwm = _pwm & 0x3F
    pwm_avg_sum += PWM_VALUE_TO_LEN(pwm)
    pwm_avg_count += 1
    if pwm_avg_count == 100 then
      var index = pwm_avg_sum / (pwm_avg_count / 10.0f) - 11
      if index < 0 then index = 0
      if index > 399 then index = 399

      pwm_dutycycle = index / 419
      pwm_avg_sum = 0
      pwm_avg_count = 0

  private def stepHeadFinal(): Unit =
    stepTrack(head = 0)
    if floppy.getHeadCount == 2 then stepTrack(head = 1)

    trackNumber = steppedTrackNumber
    trackChangeListener.onTrackChanged(driveIndex,trackNumber)

  private def stepTrack(head:Int): Unit =
    val targetTrack = floppy.getTrack(head, steppedTrackNumber)
    val sourceTrack = floppy.getTrack(head, trackNumber)
    targetTrack.resetPositionTo((sourceTrack.getPos * targetTrack.getBitSize.toFloat / sourceTrack.getBitSize).toInt)
    log.info("Drive stepped: %d => %d / %d => %d",trackNumber,steppedTrackNumber,sourceTrack.getPos,targetTrack.getPos)

  override def setMotorOn(on:Boolean): Unit =
    val oldMotorOn = motorOn
    super.setMotorOn(on)
    if oldMotorOn ^ motorOn then trackChangeListener.onMotorChanged(driveIndex, on)

  override def isStepping: Boolean = stepping > 0

  override def stepHead(): Unit =
    if isMotorOn then
      if stepping > 0 then
        log.warning("Drive is already stepping(%d): requested a new step head",stepping)
      headStepDirection match
        case HeadStepDirection.Up =>
          if trackNumber + 1 < 80 then
            steppedTrackNumber = trackNumber + 1
            stepping = STEPPING_CYCLES
        case HeadStepDirection.Down =>
          if trackNumber - 1 >= 0 then
            steppedTrackNumber = trackNumber - 1
            stepping = STEPPING_CYCLES
            
  override def getBitOnHead(head:Int,moveAhead:Boolean): Boolean =
    if floppy == null then return false

    if floppy.getHeadCount == 2 then
      floppy.getTrack(head ^ 1, trackNumber).getAndMoveOn

    if moveAhead then
      floppy.getTrack(head,trackNumber).getAndMoveOn == 1
    else
      floppy.getTrack(head,trackNumber).get == 1

  override def setBitOnHeadAndMoveAhead(head:Int,bit:Boolean): Unit =
    if floppy == null then return

    if floppy.getHeadCount == 2 then
      floppy.getTrack(head ^ 1, trackNumber).getAndMoveOn

    if bit then
      floppy.getTrack(head, trackNumber).setAndMoveOn()
    else
      floppy.getTrack(head, trackNumber).clearAndMoveOn()
  
  override def cycle(): Unit =
    cycles += 1

    if stepping > 0 then
      stepping -= 1
      if stepping == 0 then stepHeadFinal()







