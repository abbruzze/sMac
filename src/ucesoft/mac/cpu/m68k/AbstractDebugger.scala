package ucesoft.mac.cpu.m68k

import ucesoft.mac.cpu.m68k.M6800X0.Snapshot

import scala.compiletime.uninitialized

abstract class AbstractDebugger extends M6800X0.EventListener:
  case class BreakType(read:Boolean = false,write:Boolean = false,execute:Boolean = false):
    override def toString: String =
      val sb = new StringBuilder()
      if read then sb += 'R'
      if write then sb += 'W'
      if execute then sb += 'E'
      sb.toString()
  protected case class AddressBreak(breakType:BreakType,address:Int):
    override def toString: String = s"$breakType break on address ${address.toHexString}"

  private var breakOnInterruptLevel = -1
  private var breakOnReset = false
  private var breakOnHalt = false
  private var breakOnStop = false
  private var breakOnExceptionNumber = -1

  protected val m68kAddressBreaks = new collection.mutable.HashMap[Int,AddressBreak]

  private var stepByStep = false
  private var stepAlways = false

  private var lastSnap : Snapshot = uninitialized

  def addAddressBreak(address:Int,breakType:BreakType): Unit =
    m68kAddressBreaks += address -> AddressBreak(breakType, address)

  def clearAllBreaks(): Unit =
    m68kAddressBreaks.clear()
    breakOnInterruptLevel = -1
    breakOnReset = false
    breakOnHalt = false
    breakOnStop = false
    breakOnExceptionNumber = -1

  def stepByStepEnabled(enabled:Boolean): Unit =
    stepByStep = enabled
    
  def setBreakOnReset(on:Boolean): Unit = breakOnReset = on
  def isBreakOnReset: Boolean = breakOnReset
  def setBreakOnHalt(on: Boolean): Unit = breakOnHalt = on
  def isBreakOnHalt: Boolean = breakOnHalt
  def setBreakOnStop(on: Boolean): Unit = breakOnStop = on
  def isBreakOnStop: Boolean = breakOnStop
  def setBreakOnInterrupt(level:Int): Unit = breakOnInterruptLevel = level
  def getBreakOnInterruptLevel: Int = breakOnInterruptLevel
  def setBreakOnExceptionNumber(number: Int): Unit = breakOnExceptionNumber = number
  def getBreakOnExceptionNumber: Int = breakOnExceptionNumber

  def reset(): Unit =
    lastSnap = null
    stepByStep = false
    stepAlways = false
    clearAllBreaks()

  def existsBreakPending : Boolean =
    m68kAddressBreaks.nonEmpty ||
      breakOnHalt ||
      breakOnStop ||
      breakOnReset ||
      breakOnExceptionNumber != -1 ||
      breakOnInterruptLevel != -1
    
  protected def isStepAlways: Boolean = stepAlways
  protected def isStepByStep: Boolean = stepByStep
    
  def setStepAlways(enabled:Boolean): Unit =
    stepAlways = enabled

  def setStepByStep(enabled:Boolean): Unit =
    if stepByStep != enabled then
      stepByStep = enabled
      onStepByStepChange(stepByStep)

  protected def onStepByStepChange(stepByStepEnabled:Boolean): Unit = {}

  override def rw(cpu: M6800X0, address: Int, size: Size, read: Boolean, value: Int): Unit =
    m68kAddressBreaks.get(address) match
      case None =>
      case Some(break) =>
        if (read && break.breakType.read) || (!read && break.breakType.write) then
          setStepByStep(true)
          onRw(cpu,address,size,read,value)

  override def fetch(cpu: M6800X0, address: Int, opcode: Int, i: Instruction, busNotAvailable: Boolean): Unit =
    val wasBreak = m68kAddressBreaks.get(address) match
      case None =>
        false
      case Some(break) =>
        if break.breakType.execute then
          setStepByStep(true)
          true
        else
          false

    if stepByStep || stepAlways then
      onFetch(cpu,address,opcode,i,busNotAvailable,wasBreak)

  override def interrupted(cpu: M6800X0, level: Int): Unit =
    if breakOnInterruptLevel == 0 || (breakOnInterruptLevel != -1 && breakOnInterruptLevel == level) then
      setStepByStep(true)
      onInterrupted(cpu, level)

  override def reset(cpu: M6800X0): Unit =
    if breakOnReset then
      setStepByStep(true)
      onReset(cpu)

  override def halt(cpu: M6800X0, halted: Boolean): Unit =
    if breakOnHalt && halted then
      setStepByStep(true)
      onHalt(cpu)

  override def stop(cpu: M6800X0, stopped: Boolean): Unit =
    if breakOnStop && stopped then
      setStepByStep(true)
      onStop(cpu)

  override def exception(cpu: M6800X0, number: Int): Unit =
    if breakOnExceptionNumber == 0 || (breakOnExceptionNumber != -1 && breakOnExceptionNumber == number) then
      setStepByStep(true)
      onException(cpu,number)

  protected def onRw(cpu: M6800X0, address: Int, size: Size, read: Boolean, value: Int): Unit =
    val break = m68kAddressBreaks(address)
    output(break.toString)
    breakEpilogue(cpu)
  protected def onFetch(cpu: M6800X0, address: Int, opcode: Int, i: Instruction, busNotAvailable: Boolean, wasBreak:Boolean): Unit =
    if wasBreak then
      val break = m68kAddressBreaks(address)
      output(break.toString)

    checkSnapshot(cpu.getSnapshot)
    output(formatDisassembly(cpu.disassemble(address)))
    breakEpilogue(cpu)
  protected def onInterrupted(cpu: M6800X0, level: Int): Unit =
    output(s"Break on interrupt level $level")
    output(formatDisassembly(cpu.disassemble(cpu.getLastInstructionPC)))
    breakEpilogue(cpu)
  protected def onReset(cpu: M6800X0): Unit =
    output(s"Break on reset signal")
    output(formatDisassembly(cpu.disassemble(cpu.getLastInstructionPC)))
    breakEpilogue(cpu)
  protected def onHalt(cpu: M6800X0): Unit =
    output(s"Break on halt signal")
    output(formatDisassembly(cpu.disassemble(cpu.getLastInstructionPC)))
    breakEpilogue(cpu)
  protected def onStop(cpu: M6800X0): Unit =
    output(s"Break on stop condition")
    output(formatDisassembly(cpu.disassemble(cpu.getLastInstructionPC)))
    breakEpilogue(cpu)
  protected def onException(cpu: M6800X0,level:Int): Unit =
    output(s"Break on exception $level condition")
    output(formatDisassembly(cpu.disassemble(cpu.getLastInstructionPC)))
    breakEpilogue(cpu)
  protected def formatDisassembly(dis:DisassembledInstruction): String = dis.toString

  protected def checkSnapshot(snap:Snapshot): Unit =
    if lastSnap != null then
      val keys = lastSnap.regs.keySet.toList.sorted
      for k <- keys do
        val v1 = lastSnap.regs(k)
        val v2 = snap.regs(k)
        if k.toUpperCase() != "PC" && v1 != v2 then onRegisterModified(k,v1,v2)
    lastSnap = snap

  protected def onRegisterModified(reg:String,beforeValue:Int,afterValue:Int) = {}

  protected def output(text:String): Unit = {}

  protected def breakEpilogue(cpu: M6800X0): Unit = {}

