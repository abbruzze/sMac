package ucesoft.mac.ui

import org.jfree.chart.axis.AxisSpace
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.{ChartFactory, ChartPanel, StandardChartTheme}
import org.jfree.data.time.{DynamicTimeSeriesCollection, Second}
import ucesoft.mac.cpu.m68k.M6800X0
import ucesoft.mac.{ByteTransferProbe, Clock}

import java.awt.event.{ActionEvent, ActionListener, WindowAdapter, WindowEvent}
import java.awt.{Color, Paint}
import java.lang.management.ManagementFactory
import javax.swing.*
import scala.compiletime.uninitialized

/**
 * @author Alessandro Abbruzzetti
 *         Created on 19/01/2024 13:01  
 */
class PerformanceMonitor(frame:JFrame, m68k:M6800X0, clock:Clock,probes:Array[ByteTransferProbe], closeAction: () => Unit) extends JPanel with ActionListener:
  private enum PerfState:
    case LOW_RES,NORMAL_RES

  import PerfState.*

  private final val CHART_COLOR = Color.YELLOW
  private final val LOW_RES_CHART_COLOR = Color.RED

  private val overallPerfDataset = new DynamicTimeSeriesCollection(1, 2000, new Second())
  private val m68kPerfDataset = new DynamicTimeSeriesCollection(1, 2000, new Second())
  private val sysPerfDataset = new DynamicTimeSeriesCollection(1, 2000, new Second())
  private val memDataset = new DynamicTimeSeriesCollection(1, 2000, new Second())
  private val probesDataset = Array.fill(probes.length)(new DynamicTimeSeriesCollection(1, 2000, new Second()))
  private var lastM68kCycles = 0L
  final val dialog = new JDialog(frame,"Performance monitor")
  private val timer = new Timer(1000,this)
  private val darkTheme = StandardChartTheme.createDarknessTheme()
  private var emuPlot : XYPlot = uninitialized
  private var emuPlotPaint : Paint = uninitialized
  private var state = NORMAL_RES
  private var lowResThreshold = 80
  private var lowResObservationPeriodInSec = 10
  private var resCounter = 0
  private var firstSample = true

  init()
  
  def shutdown(): Unit =
    timer.stop()

  def setLowResThreshold(t:Int): Unit =
    lowResThreshold = t
  def setLowResObservationPeriodInSec(p:Int): Unit =
    lowResObservationPeriodInSec = p

  private def init(): Unit =
    dialog.addWindowListener(new WindowAdapter:
      override def windowClosing(e: WindowEvent): Unit =
        shutdown()
        closeAction()
    )
    setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS))
    overallPerfDataset.setTimeBase(new Second(new java.util.Date()))
    overallPerfDataset.addSeries(Array(0f),0,"Overall performance")
    m68kPerfDataset.setTimeBase(new Second(new java.util.Date()))
    m68kPerfDataset.addSeries(Array(0f), 0, "M68k million cycles/sec")
    sysPerfDataset.setTimeBase(new Second(new java.util.Date()))
    sysPerfDataset.addSeries(Array(0f), 0, "System load")
    memDataset.setTimeBase(new Second(new java.util.Date()))
    memDataset.addSeries(Array(0f), 0, "Memory Mb")
    emuPlot = addChart("Emulator","Performance",overallPerfDataset)
    addChart("M68000", "cycles/sec", m68kPerfDataset)
    addChart("Host system load", "load", sysPerfDataset)
    addChart("Used memory", "memory", memDataset)
    for (p,i) <- probes.zipWithIndex do
      probesDataset(i).setTimeBase(new Second(new java.util.Date()))
      probesDataset(i).addSeries(Array(0f),0,s"${p.probeName} - transfer rate")
      addChart(p.probeName,"Kb/sec",probesDataset(i))

    dialog.getContentPane.add("Center",this)
    dialog.setSize(500,800)
    timer.setRepeats(true)
    timer.start()

  private def addChart(title:String,yLabel:String,series:DynamicTimeSeriesCollection): XYPlot =
    val chart = ChartFactory.createTimeSeriesChart(title, "Time", yLabel, series, true, true, false)
    darkTheme.apply(chart)
    val plot = chart.getXYPlot
    val space = new AxisSpace()
    space.setLeft(50)
    plot.setFixedRangeAxisSpace(space)
    plot.getRenderer.setSeriesPaint(0, CHART_COLOR)
    val axis = plot.getDomainAxis
    axis.setAutoRange(true)
    axis.setFixedAutoRange(200000)
    val chartPanel = new ChartPanel(chart)
    add(chartPanel)
    plot

  override def actionPerformed(e:ActionEvent): Unit =
    if e.getSource == timer then
      overallPerfDataset.advanceTime()
      val perf = clock.getLastPerformance
      overallPerfDataset.appendData(Array(perf.toFloat))
      m68kPerfDataset.advanceTime()
      val m68kCycles = m68k.getTotalElapsedCycles
      val elapsed = if firstSample then 0 else ((m68kCycles - lastM68kCycles) / 1_000_000.0).toFloat
      m68kPerfDataset.appendData(Array(elapsed))
      lastM68kCycles = m68kCycles

      sysPerfDataset.advanceTime()
      val load = ManagementFactory.getPlatformMXBean(classOf[com.sun.management.OperatingSystemMXBean]).getProcessCpuLoad * 100
      sysPerfDataset.appendData(Array(load.toFloat))
      memDataset.appendData(Array((Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()) / (1024f * 1024f)))
      memDataset.advanceTime()

      for (p, i) <- probes.zipWithIndex do
        probesDataset(i).advanceTime()
        val kbytes = p.getAndResetByteAccessed / 1024.0f
        probesDataset(i).appendData(Array(kbytes))

      state match
        case NORMAL_RES =>
          if perf < lowResThreshold then
            resCounter += 1
            if resCounter == lowResObservationPeriodInSec then
              state = LOW_RES
              resCounter = 0
              emuPlot.getRenderer.setSeriesPaint(0,LOW_RES_CHART_COLOR)
          else
            resCounter = 0
        case LOW_RES =>
          if perf > lowResThreshold then
            resCounter += 1
            if resCounter == lowResObservationPeriodInSec then
              state = NORMAL_RES
              resCounter = 0
              emuPlot.getRenderer.setSeriesPaint(0,CHART_COLOR)
          else
            resCounter = 0

      firstSample = false


