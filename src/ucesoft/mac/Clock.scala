package ucesoft.mac

import java.util.concurrent.CountDownLatch
import scala.compiletime.uninitialized

object Clock:
  trait EventID:
    def cancel(): Unit
    def isCanceled: Boolean

  trait Clockable:
    def clock(cycles:Long): Unit

class Clock (val name: String,private var clocksPerSecond: Int,val autoClockIncrement:Boolean = false) extends MACComponent with Runnable:
  override protected val componentName: String = "masterClock"
  import Clock.*

  private class ClockEvent(val when:Long,val action:Clockable,val period:Int = 0):
    var canceled : Boolean = false
  private class EventList(val e:ClockEvent,var next:EventList = null)

  private val thread : Thread = {
    val t = new Thread(this, s"MasterClock-$name")
    t.setPriority(Thread.MAX_PRIORITY)
    t
  }
  private var running = false
  private var clockCycles = 0L

  private var suspended = false
  private val suspendedLock = new Object
  private var suspendedNotifier = new CountDownLatch(1)
  private val shutdownNotifier = new CountDownLatch(1)

  private var warpMode = false
  private var lastCorrectionTime = 0L
  private var lastCorrectionCycles = 0L
  private var nextPerformanceMeasurementTime = 0L
  private var lastPerformance = 0
  private var throttleStartedAt = 0L
  private var skipThrottle = false
  private var throttleCycleCount = 0
  private inline val THROTTLE_CYCLE_TARGET = 500
  private inline val PERFORMANCE_MEASUREMENT_INTERVAL_SECONDS = 1 * 1000
  private var freqDiv1000, freqInvBy1000 = 0.0

  private var events : EventList = uninitialized

  private var errorHandler : Throwable => Unit = uninitialized

  private var clockable : Clockable = uninitialized

  setFrequency(clocksPerSecond)

  override def reset(): Unit =
    clockCycles = 0
    events = null
    
  final def addCycles(cycles:Int): Unit =
    clockCycles += cycles

  final def setClockable(clockable:Clockable): Unit =
    this.clockable = clockable

  final def setErrorHandler(eh:Throwable => Unit): Unit =
    this.errorHandler = eh

  final def setFrequency(clocksPerSecond:Int): Unit =
    this.clocksPerSecond = clocksPerSecond
    freqDiv1000 = clocksPerSecond / 1000.0
    freqInvBy1000 = 1000.0 / clocksPerSecond

  final def setWarpMode(enabled:Boolean): Unit =
    warpMode = enabled
    if !enabled then
      setupNextMeasurement()

  final def cycles: Long = clockCycles

  final def shutdown(waitForShutdown:Boolean = false): Unit =
    running = false
    if waitForShutdown then
      shutdownNotifier.await()

  final def start(): Unit =
    if !running then
      thread.start()

  final def cyclesForMillis(millis:Float): Int =
    math.round(clocksPerSecond * millis / 1000f)

  final def scheduleMillis(millis:Float,action:Clockable,isPeriodic: Boolean = false): EventID =
    schedule(cyclesForMillis(millis),action,isPeriodic)
  final def schedule(cyclesFromNow:Int,action:Clockable,isPeriodic: Boolean = false): EventID =
    val event = new ClockEvent(clockCycles + cyclesFromNow,action,if isPeriodic then cyclesFromNow else 0)
    schedule(event)
  final def scheduleAbsolute(cycles:Long,action:Clockable): EventID =
    val event = new ClockEvent(cycles, action, 0)
    schedule(event) 
  
  private def schedule(event:ClockEvent): EventID =
    val id = new EventID:
      override def cancel(): Unit = event.canceled = true
      override def isCanceled: Boolean = event.canceled

    if events == null then
      events = new EventList(event)
    else if event.when <= events.e.when then
      events = new EventList(event, events)
    else
      var ptr = events
      var ptrNext = events.next
      val when = event.when
      while ptrNext != null && when > ptrNext.e.when do
        ptr = ptrNext
        ptrNext = ptrNext.next

      ptr.next = new EventList(event, ptrNext)

    id
  end schedule

  override final def run(): Unit =
    running = true
    while running do
        if suspended then
          while suspended do
            suspendedLock.synchronized {
              suspendedNotifier.countDown()
              suspendedLock.wait()
            }
        try
          checkEvents()
          doAction()
          throttleCycleCount += 1
          if throttleCycleCount == THROTTLE_CYCLE_TARGET then
            throttleCycleCount = 0
            throttle()
        catch
          case t: Throwable =>
            if errorHandler != null then
              errorHandler(t)
            else
              t.printStackTrace()
    end while

    shutdownNotifier.countDown()
    log.info("Clock %s shutdown",name)

  private inline def doAction(): Unit =
    if !autoClockIncrement then
      clockCycles += 1
    clockable.clock(clockCycles)

  private inline def checkEvents(): Unit =
    while events != null && clockCycles >= events.e.when do
      if !events.e.canceled then
        events.e.action.clock(clockCycles)
        if events.e.period > 0 then
          schedule(events.e.period,events.e.action,isPeriodic = true)
      val next = events.next
      events.next = null // cut from list
      events = next

  private inline def setupNextMeasurement(): Unit =
    lastCorrectionTime = System.currentTimeMillis
    lastCorrectionCycles = clockCycles
    throttleStartedAt = clockCycles
    nextPerformanceMeasurementTime = System.currentTimeMillis + PERFORMANCE_MEASUREMENT_INTERVAL_SECONDS

  private inline def throttle(): Unit =
    if !warpMode && !skipThrottle then
      val timeDiff = System.currentTimeMillis - lastCorrectionTime
      val cyclesDiff = cycles - lastCorrectionCycles
      val expectedCycles = timeDiff * freqDiv1000
      if cyclesDiff > expectedCycles then
        val waitTime = freqInvBy1000 * (cyclesDiff - expectedCycles)
        val millis = math.floor(waitTime).asInstanceOf[Int]
        val nanos = ((waitTime - millis) * 1000).toInt
        Thread.sleep(millis,nanos)

    if skipThrottle || System.currentTimeMillis > nextPerformanceMeasurementTime then
      skipThrottle = false
      val executed = cycles - throttleStartedAt
      lastPerformance = math.round(100.0 * executed / clocksPerSecond / (PERFORMANCE_MEASUREMENT_INTERVAL_SECONDS / 1000)).toInt
      setupNextMeasurement()

  final def getLastPerformance: Int =
    lastPerformance

  final def isPaused: Boolean = suspended || !running
  final def isRunning : Boolean = running

  final def pause(): Unit =
    if Thread.currentThread() != thread && running then
      if !suspended then
        suspended = true
        suspendedNotifier.await()
        suspendedNotifier = new CountDownLatch(1)
    else
      suspended = true

  final def play(): Unit =
    if suspended then
      suspendedLock.synchronized {
        suspended = false
        suspendedLock.notify()
      }

  // ===================== State =============================
  override protected def createState(sb: StateBuilder): Unit =
    sb.
    w("cycles",clockCycles)

  override protected def restoreState(sb: StateBuilder): Unit =
    import sb.*
    clockCycles = r[Long]("cycles")
    setupNextMeasurement()
