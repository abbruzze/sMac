package ucesoft.mac.cpu.m68k

import ucesoft.mac.{MACComponent, StateBuilder}
import scala.compiletime.uninitialized

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

abstract class M68KCore(val mem:Memory) extends MACComponent with M6800X0:
  import M6800X0.*

  protected enum CPUState:
    case EXECUTING, STOPPED, HALTED

  // internal exception
  protected class CPUException extends Exception
  protected class AddressBusException(val address: Int, val busAccess: BusAccessMode, val instructionInProgress: Boolean, val codeAccess: Boolean) extends CPUException
  protected class IllegalInstruction extends CPUException
  protected class UnimplementedInstruction(val pattern:Int) extends CPUException
  protected class PrivilegeViolation extends CPUException
  protected class ResetException extends CPUException
  protected class TraceException extends CPUException
  protected class BUSNotAvailable extends CPUException

  // CPU state
  protected var state : CPUState = CPUState.EXECUTING
  // Data registers
  protected final val dataRegs : Array[Register] = (0 to 7).toArray.map(i => new Register(RegisterType.Data,model,i))
  // Address registers
  protected final val addressRegs : Array[Register] = (0 to 6).toArray.map(i => new Register(RegisterType.Address,model,i))
  // Status register (SR+CCR)
  protected final val statusReg = new StatusRegister(model)
  // User Stack pointer (USP)
  protected final val userSPReg = new Register(RegisterType.SP,model)
  // System Stack pointer (SSP)
  protected final val systemSPReg = new Register(RegisterType.SP, model)
  // Program counter
  protected final val pcReg = new Register(RegisterType.PC, model)
  // Last instruction PC
  protected var pc0 = 0
  // Prefetch queue
  protected case class PrefetchQueue(var memptr:Int,var irc:Int,var ird:Int)
  protected final val prefetchQueue = PrefetchQueue(0,0,0)
  private var cyclesAdjustment = 0
  // Bus Access Listener
  private var busAccessListener : BusAccessListener = uninitialized
  // Pending reset request
  protected var pendingResetRequest = false
  // Reset devices listener
  private var resetDeviceListener : ResetDeviceListener = uninitialized
  // Last instruction
  protected var lastInstruction : Instruction = uninitialized
  // Interrupt mask pending
  protected var pendingInterruptMask = 0
  protected var nmiInterruptPending = false
  protected var pendingInterruptDelayCount = 0
  // event listeners
  protected final val eventListeners = new ListBuffer[EventListener]
  protected var notifyEventListeners = false
  // bus
  protected var busAvailable = true
  // dtack
  protected var dtackEnabled = true
  // Interrupt ack listener
  protected var intAckListener : InterruptAckListener = uninitialized
  // additional wait cycles
  protected var waitCycles = 0

  // Context
  protected final val ctx = new M6800X0.Context {
    override final def model : Model =  M68KCore.this.model
    override final def adjustCycles(adjustment:Int): Unit = cyclesAdjustment = adjustment

    override final def busIdle(cycles: Int): Unit =
      busAccess(BusAccessMode.Idle,Size.Word,codeAccess = false,0,0,cycles)

    override final def fetchWord(incPC:Boolean,clearPrefetchQueue:Boolean): Int =
      pendingPrefetchQueueClear |= clearPrefetchQueue
      val p = prefetch()
      if incPC then
        pcReg.increment(Size.Word)
      p

    override final def fetchWordForDisassembling(address: Int): Int =
      mem.read(address & model.addressBUSMask,Size.Word,DEBUG_READ_OPTION) // doesn't fire read event

    override final def getEA(mode: Int, reg: Int, size: Size, disassemblingAddress: Option[Int],includeIdleBusCycles:Boolean): Operand =
      val index = if mode < 7 then mode else mode + reg
      val op = if disassemblingAddress.isEmpty then addressingModes(index) else disassemblingAddressingModes(index)
      /*if !busAvailable && disassemblingAddress.isEmpty && op.requiresBUS then
        throw new BUSNotAvailable*/
      op.init(reg, size, disassemblingAddress,includeIdleBusCycles)
      op

    override final def getEA(addressingMode: AddressingMode, reg: Int, size: Size, disassemblingAddress: Option[Int]): Operand =
      val op = if disassemblingAddress.isEmpty then addressingModes(addressingMode.ordinal) else disassemblingAddressingModes(addressingMode.ordinal)
      /*if !busAvailable && disassemblingAddress.isEmpty && op.requiresBUS then
        throw new BUSNotAvailable*/
      op.init(reg, size, disassemblingAddress)
      op

    override final def readMemory(address: Int, size: Size,busAccess:BusAccessMode = BusAccessMode.Read): Int =
      checkMemoryAddress(address,BusAccessMode.Read,size,isCodeAccess = false)
      if notifyEventListeners then fireRWEvent(address,size,isRead = true)
      val read = mem.read(address & model.addressBUSMask, size)
      checkMemoryAccess(address, busAccess, read, size, codeAccess = false,checkAddress = false)

      read

    override final def writeMemory(address: Int, value: Int, size: Size,busAccess:BusAccessMode = BusAccessMode.Write): Unit =
      checkMemoryAccess(address, busAccess, value, size, codeAccess = false)
      if notifyEventListeners then fireRWEvent(address,size,isRead = false,value)
      mem.write(address & model.addressBUSMask, value, size)

    override final def raiseException(number:Int): Unit =
      M68KCore.this.raiseException(number)

    override final def branch(pc:Int): Unit =
      pcReg.set(pc,Size.Long)
      checkMemoryAddress(pc,BusAccessMode.Read,Size.Long,isCodeAccess = true,instructionInProgress = false)
      pendingPrefetchQueueClear = true

    override final def getRegister(rtype: RegisterType, index: Int): Register =
      import RegisterType.*
      rtype match
        case Data => dataRegs(index)
        case Address =>
          if index < 7 then
            addressRegs(index)
          else // A7
            if statusReg.isSupervisorMode then systemSPReg else userSPReg
        case PC => throw new IllegalArgumentException("Invalid use of getRegister. Use getPC or branch method")
        case SR => statusReg
        case SP => // A7
          if statusReg.isSupervisorMode then systemSPReg else userSPReg
        case USP =>
          userSPReg

    override final def getPC: Int = pcReg.get(Size.Long)

    override final def resetDevices(): Unit =
      if resetDeviceListener != null then
        resetDeviceListener.resetDevice()

    override final def stopCPU(): Unit =
      changeState(CPUState.STOPPED)
      if notifyEventListeners then fireStopEvent(stopped = true)
  }
  // Instruction Set
  protected final val instructionSet: Array[Instruction] = buildInstructionSet()
  // Addressing modes for execution
  protected final val addressingModes: Array[Operand] = buildAddressingModes()
  // Addressing modes for disassembling
  protected final val disassemblingAddressingModes: Array[Operand] = buildAddressingModes()
  // pending clear of prefetch queue
  protected var pendingPrefetchQueueClear = false
  // bus tracing
  protected var busTracing = false
  protected var lastBusTraces : List[BusTrace] = Nil
  // elapsed cycles in the instruction
  protected var instructionElapsedCycles = 0
  protected var lastInstructionElapsedCycles = 0
  // total elapsed cycles
  protected var totalElapsedCycles = 0L
  // instruction exception number handled
  protected var instructionExceptionNumber = -1 // -1 means no exception handled

  // ======================= Constructor ===========================================

  // ===============================================================================

  // =========================== Memory ============================================
  protected def push(value:Int,size:Size): Unit =
    val stackReg = getRegister(RegisterType.SP)
    stackReg.decrement(size)
    val address = stackReg.get(Size.Long)
    checkMemoryAccess(address, BusAccessMode.Push, value, size,codeAccess = false)
    if notifyEventListeners then fireRWEvent(address,size,isRead = false,value)
    mem.write(address & model.addressBUSMask, value, size)

  protected def pop(size:Size): Int =
    val stackReg = getRegister(RegisterType.SP)
    val address = stackReg.get(Size.Long)
    if notifyEventListeners then fireRWEvent(address,size,isRead = true)
    val popped = mem.read(address & model.addressBUSMask,size)
    checkMemoryAccess(address, BusAccessMode.Pop, popped, size,codeAccess = false)

    popped

  // Must check the address for address error exception
  protected def checkMemoryAddress(address:Int,accessMode:BusAccessMode,size:Size,isCodeAccess:Boolean,instructionInProgress:Boolean = true): Unit = {}

  protected def checkMemoryAccess(address:Int, accessMode:BusAccessMode, value:Int, size:Size, codeAccess:Boolean,checkAddress:Boolean = true): Unit =
    if checkAddress then
      checkMemoryAddress(address,accessMode,size,codeAccess)

    busAccess(accessMode,size, codeAccess,address,value)
    //println(s"Bus ${if isRead then "Read" else "Write"} ${if size == Size.Long then 8 else 4}")

  protected def busAccess(accessMode:BusAccessMode,size:Size, codeAccess:Boolean,address:Int = 0,value:Int = 0,cycles:Int = 0): Unit =
    import FunctionCode.*

    val elapsedCycles = accessMode match
      case BusAccessMode.Idle =>
        instructionElapsedCycles += cycles
        cycles
      case _ =>
        var elapsed = if size == Size.Long then accessMode.cycles << 1 else accessMode.cycles
        if cyclesAdjustment != 0 then
          elapsed += cyclesAdjustment
          cyclesAdjustment = 0

        instructionElapsedCycles += elapsed
        elapsed

    if busAccessListener != null && elapsedCycles > 0 then
      busAccessListener.busAccess(address,accessMode,elapsedCycles)

    if busTracing && elapsedCycles > 0 then
      val fcode = if !codeAccess then
        if statusReg.isSupervisorMode then SuperData else UserData
      else if statusReg.isSupervisorMode then SuperProgram else UserProgram

      accessMode match
        case BusAccessMode.Idle =>
          val idleCycles4 = cycles >> 2
          val idleCycles2 = (cycles - (idleCycles4 << 2)) >> 1
          for(_ <- 1 to idleCycles4)
            lastBusTraces = BusTrace(address,value,BusAccessMode.Idle,Size.Long, fcode) :: lastBusTraces
          for (_ <- 1 to idleCycles2)
            lastBusTraces = BusTrace(address, value, BusAccessMode.Idle, Size.Word, fcode) :: lastBusTraces
        case _ =>
          lastBusTraces = BusTrace(address,value,accessMode,size, fcode) :: lastBusTraces
  // ===============================================================================

  // =========================== Disassembling =====================================
  override def disassemble(address: Int): DisassembledInstruction =
    val opcode = mem.read(address & model.addressBUSMask,Size.Word,DEBUG_READ_OPTION) // doesn't fire read event
    val i = instructionSet(opcode)
    if i == null then
      DisassembledInstruction(address,-1,".short",List(opcode),Some("%04x".format(opcode)))
    else
      i.disassemble(address)
  // ===============================================================================

  // =========================== Snapshot ==========================================
  override def getSnapshot: Snapshot =
    val regMap = new mutable.HashMap[String,Int]()
    for (dx <- dataRegs) regMap += dx.mnemonic -> dx.get(Size.Long)
    for (ax <- addressRegs) regMap += ax.mnemonic -> ax.get(Size.Long)
    regMap += "usp" -> userSPReg.get(Size.Long)
    regMap += "ssp" -> systemSPReg.get(Size.Long)
    regMap += "sr"  -> statusReg.get(Size.Long)
    regMap += "pc"  -> pcReg.get(Size.Long,signExtended = true)
    Snapshot(regMap.toMap,List(prefetchQueue.ird,prefetchQueue.irc))

  override def setSnapshot(snap: Snapshot): Unit =
    for(dx <- 0 to 7) {
      val d = snap.regs(s"d$dx")
      dataRegs(dx).set(d,Size.Long)
    }
    for (ax <- 0 to 6) {
      val a = snap.regs(s"a$ax")
      addressRegs(ax).set(a, Size.Long)
    }
    userSPReg.set(snap.regs("usp"),Size.Long)
    systemSPReg.set(snap.regs("ssp"),Size.Long)
    statusReg.set(snap.regs("sr"),Size.Long)
    pcReg.set(snap.regs("pc"),Size.Long)
    prefetchQueue.irc = snap.prefetch.tail.head
    prefetchQueue.ird = snap.prefetch.head
    prefetchQueue.memptr = pcReg.get(Size.Long) + 4
    pendingPrefetchQueueClear = false

    //pcReg.increment(Size.Word)
  // ===============================================================================

  /**
   * Fill instructionSet
    */
  protected def buildInstructionSet(): Array[Instruction]

  /**
   * Fill addressingModes
   * A new fresh array of operands must be created for each invocation
   */
  protected def buildAddressingModes(): Array[Operand]

  /*
    0 0000 RESET: initial supervisor stack pointer (SSP)
    1 0004 RESET: initial program counter (PC)
    2 0008 bus error
    3 000C address error
    4 0010 illegal instruction
    5 0014 zero divide
    6 0018 CHK instruction
    7 001C TRAPV instruction
    8 0020 privilege violation
    9 0024 trace
    10 0028 1010 instruction trap
    11 002C 1111 instruction trap
    12* 0030 not assigned, reserved by Motorola
    13* 0034 not assigned, reserved by Motorola
    14* 0038 not assigned, reserved by Motorola
    15 003C uninitialized interrupt vector
    16-23* 0040-005F not assigned, reserved by Motorola
    24 0060 spurious interrupt
    25 0064 Level 1 interrupt autovector
    26 0068 Level 2 interrupt autovector
    27 006C Level 3 interrupt autovector
    28 0070 Level 4 interrupt autovector
    29 0074 Level 5 interrupt autovector
    30 0078 Level 6 interrupt autovector
    31 007C Level 7 interrupt autovector
    32-47 0080-00BF TRAP instruction vectors**
    48-63 00C0-00FF not assigned, reserved by Motorola
    64-255 0100-03FF user interrupt vectors

    * No peripheral devices should be assigned these numbers
    ** TRAP #N uses vector number 32+N
  */

  /*
    When RESET and HALT are driven by an external device, the entire system, including the processor, is reset.
    Resetting the processor initializes the internal state.
    The processor reads the reset vector table entry (address $00000) and loads the contents into the supervisor stack pointer (SSP).
    Next, the processor loads the contents of address $00004(vector table entry 1) into the program counter.
    Then the processor initializes the interrupt level in the status register to a value of seven.
    In the MC68010, the processor also clears the vector base register to $00000. No other register is affected by the reset sequence.
  */
  protected def resetCPU(): Unit =
    dtackEnabled = true
    busAvailable = true
    totalElapsedCycles = 0
    pendingInterruptMask = 0
    nmiInterruptPending = false
    pendingInterruptDelayCount = 0
    waitCycles = 0
    pendingPrefetchQueueClear = false
    totalElapsedCycles = 0L
    instructionExceptionNumber = -1

    busAccess(BusAccessMode.Idle,Size.Word,false,0,0,14)
    if notifyEventListeners then fireRWEvent(0,Size.Long,isRead = true)
    val vec0Address = mem.read(0,Size.Long)
    checkMemoryAccess(0,BusAccessMode.Reset,vec0Address,Size.Long,false)
    systemSPReg.set(vec0Address,Size.Long)
    val vec4Address = mem.read(4, Size.Long)
    if notifyEventListeners then fireRWEvent(4,Size.Long,isRead = true)
    checkMemoryAccess(4, BusAccessMode.FetchVector, vec4Address, Size.Long, false)
    pcReg.set(vec4Address,Size.Long)
    statusReg.setCCR(0)
    statusReg.setSupervisorMode(true)
    statusReg.setInterruptMask(7)
    pendingPrefetchQueueClear = true
    busAccess(BusAccessMode.Idle,Size.Word,false,0,0,2)
    changeState(CPUState.EXECUTING)
    prefetch()
    fireResetEvent()

  protected def changeState(newState:CPUState): Unit =
    state = newState

  override protected def reset(): Unit = reset(now = false)

  override def reset(now:Boolean): Unit = 
    if now then 
      resetCPU() 
    else
      pendingResetRequest = true
  
  override def halt(asserted:Boolean): Unit =
    if asserted then
      changeState(CPUState.HALTED)
      if notifyEventListeners then fireHaltEvent(halted = true)
    else
      changeState(CPUState.EXECUTING)
      if notifyEventListeners then fireHaltEvent(halted = false)

  // =============== PREFETCH HANDLING ===========================================
  protected def fillPrefetchQueue(clear:Boolean): Unit =
    // 1)  Loads IRC from external memory
    if clear then
      prefetchQueue.memptr = pcReg.get(Size.Long)
      if notifyEventListeners then fireRWEvent(prefetchQueue.memptr,Size.Word,isRead = true)
      prefetchQueue.irc = mem.read(prefetchQueue.memptr & model.addressBUSMask,Size.Word)
      checkMemoryAccess(prefetchQueue.memptr,BusAccessMode.Fetch,prefetchQueue.irc,Size.Word,codeAccess = true)
      prefetchQueue.memptr += Size.Word.bytes
    // 2) IR <- IRC
    val ir = prefetchQueue.irc
    // 3) Reload IRC from external memory
    if notifyEventListeners then fireRWEvent(prefetchQueue.memptr,Size.Word,isRead = true)
    prefetchQueue.irc = mem.read(prefetchQueue.memptr & model.addressBUSMask,Size.Word)
    checkMemoryAccess(prefetchQueue.memptr,BusAccessMode.Fetch,prefetchQueue.irc,Size.Word,codeAccess = true)
    //println(s"Prefected from ${prefetchQueue.memptr} = ${prefetchQueue.irc}")
    prefetchQueue.memptr += Size.Word.bytes
    // 4) Transfer IR to IRD
    prefetchQueue.ird = ir


  protected def prefetch(): Int =
    fillPrefetchQueue(clear = pendingPrefetchQueueClear)
    pendingPrefetchQueueClear = false
    prefetchQueue.ird

  /*
  protected def prefetch(): Int =
    fillPrefetchQueue(clear = pendingPrefetchQueueClear)
    if !pendingPrefetchQueueClear && prefetchQueue.fetched > 0 then
      fillPrefetchQueue(false)

    pendingPrefetchQueueClear = false

    prefetchQueue.ird
  */
  // ====================== Exception handling ===================================
  protected def raiseException(n:Int,pushStackFrame:Boolean = true): Unit =
    val oldSR = statusReg.get(Size.Word)
    statusReg.setSupervisorMode(true)
    statusReg.setTrace(false)

    if pushStackFrame then
      push(pcReg.get(Size.Long),Size.Long)
      push(oldSR,Size.Word)

    val vectorAddress = n << 2
    if notifyEventListeners then fireRWEvent(vectorAddress,Size.Long,isRead = true)
    val newPC = mem.read(vectorAddress & model.addressBUSMask,Size.Long)
    checkMemoryAccess(vectorAddress, BusAccessMode.FetchVector, newPC, Size.Long,codeAccess = true)

    busAccess(BusAccessMode.Idle,Size.Byte,false,0,0,2)

    pcReg.set(newPC,Size.Long)
    pendingPrefetchQueueClear = true
    instructionExceptionNumber = n

    if state == CPUState.STOPPED then
      changeState(CPUState.EXECUTING)
      if notifyEventListeners then fireStopEvent(stopped = false)
    if notifyEventListeners then fireExceptionEvent(n)
  // =============================================================================
  override def getLastInstructionPC : Int = pc0
  
  override def getRegister(rtype:RegisterType,index:Int): Register =
    import RegisterType.*
    rtype match
      case Data =>      dataRegs(index)
      case Address =>
        if index < 7 then
          addressRegs(index)
        else // A7
          if statusReg.isSupervisorMode then systemSPReg else userSPReg
      case PC =>        pcReg
      case SR =>        statusReg
      case SP => // A7
        if statusReg.isSupervisorMode then systemSPReg else userSPReg
      case USP =>
        userSPReg
      case SSP =>
        systemSPReg

  override final def addWaitCycles(waitCycles: Int): Unit =
    this.waitCycles = waitCycles

  override def setBUSAvailable(available: Boolean): Unit = busAvailable = available

  override def isBUSAvailable: Boolean = busAvailable

  override def setDTACK(enabled: Boolean): Unit = dtackEnabled = enabled

  override def setBusTracing(enabled: Boolean): Unit = busTracing = enabled

  override def getBusTracing: List[BusTrace] = lastBusTraces.reverse

  override def getExceptionNumber: Option[Int] = 
    if instructionExceptionNumber == -1 then
      None
    else Some(instructionExceptionNumber)

  override def setBusAccessListener(listener:BusAccessListener): Unit = busAccessListener = listener
  
  override def setInterruptAckListener(listener:InterruptAckListener): Unit = intAckListener = listener
  override def setResetDeviceListener(resetDeviceListener: ResetDeviceListener): Unit = this.resetDeviceListener = resetDeviceListener
  
  override def getTotalElapsedCycles : Long = totalElapsedCycles

  override def getLastInstructionElapsedCycles: Int = lastInstructionElapsedCycles

  override def addEventListener(el:EventListener): Unit =
    if !eventListeners.contains(el) then
      eventListeners += el
      notifyEventListeners = eventListeners.nonEmpty

  override def removeEventListener(el:EventListener): Unit =
    eventListeners -= el
    notifyEventListeners = eventListeners.nonEmpty

  protected def handleAddressBusException(ex: CPUException, opcode: Int): Unit =
    import ucesoft.mac.cpu.m68k.M6800X0.FunctionCode.*
    ex match
      case e: AddressBusException =>
        busAccess(BusAccessMode.Idle, Size.Word, codeAccess = false, 0, 0, 6)
        val delta = if lastInstruction.isBranch then 4 else 2
        push(pcReg.get() - delta, Size.Long)
        //println(s"PC => ${pcReg.get(Size.Long)} / ${pcReg.get(Size.Long) - 2}")
        push(statusReg.get(Size.Word), Size.Word)
        push(opcode, Size.Word)
        push(e.address, Size.Long)
        var flags = opcode & 0xFFE0
        if e.busAccess.isRead then flags |= 0x10
        if !e.instructionInProgress then flags |= 0x8
        val fcode = if !e.codeAccess then
          if statusReg.isSupervisorMode then SuperData else UserData
        else if statusReg.isSupervisorMode then SuperProgram else UserProgram
        //println(s"flags=$flags FCODE=$fcode codeAccess=${e.instructionInProgress}")
        flags |= fcode.code
        push(flags, Size.Word)

  // ========================= Interrupt handling ================================
  override def interrupt(ipl012:Int): Unit =
    val _int = ipl012 & 7
    if _int == 7 && pendingInterruptMask != 7 then // NMI raising edge
      nmiInterruptPending = true
    else
      pendingInterruptMask = _int
    if pendingInterruptDelayCount == 0 then
      pendingInterruptDelayCount = 1

  protected def serveInterrupt(): Unit =
    busAccess(BusAccessMode.Idle, Size.Word, codeAccess = false, 0, 0, 6)
    busAccess(BusAccessMode.VectorNumberAcquisition, Size.Word, codeAccess = false, 0, 0, 2)
    busAccess(BusAccessMode.Idle, Size.Word, codeAccess = false, 0, 0, 2)
    nmiInterruptPending = false
    // The interrupt acknowledge cycle places the level of the interrupt being acknowledged on address bits A3–A1 and drives all other address lines high
    if notifyEventListeners then fireRWEvent(0xFFFFF0 | pendingInterruptMask << 1,Size.Byte,isRead = true)
    mem.read(0xFFFFF0 | pendingInterruptMask << 1,Size.Byte) // fake read
    raiseException(24 + pendingInterruptMask)
    statusReg.setInterruptMask(pendingInterruptMask)
    if state == CPUState.STOPPED then
      changeState(CPUState.EXECUTING)
      if notifyEventListeners then fireStopEvent(stopped = false)
    fillPrefetchQueue(true)
    if notifyEventListeners then fireInterruptedEvent(pendingInterruptMask)
  // ======================= Event Listeners ====================================
  protected def fireRWEvent(address:Int,size:Size,isRead:Boolean,value:Int = 0): Unit =
    val listeners = eventListeners.toList.iterator
    while listeners.hasNext do
      listeners.next().rw(this,address,size,isRead,value)
  protected def fireFetchEvent(address:Int,opcode:Int,i:Instruction): Unit =
    val listeners = eventListeners.toList.iterator
    while listeners.hasNext do
      listeners.next().fetch(this,address,opcode,i,!busAvailable)
  protected def fireInterruptedEvent(level: Int): Unit =
    val listeners = eventListeners.toList.iterator
    while listeners.hasNext do
      listeners.next().interrupted(this, level)
  protected def fireResetEvent(): Unit =
    val listeners = eventListeners.toList.iterator
    while listeners.hasNext do
      listeners.next().reset(this)
  protected def fireHaltEvent(halted:Boolean): Unit =
    val listeners = eventListeners.toList.iterator
    while listeners.hasNext do
      listeners.next().halt(this,halted)
  protected def fireStopEvent(stopped: Boolean): Unit =
    val listeners = eventListeners.toList.iterator
    while listeners.hasNext do
      listeners.next().stop(this, stopped)
  protected def fireExceptionEvent(number: Int): Unit =
    val listeners = eventListeners.toList.iterator
    while listeners.hasNext do
      listeners.next().exception(this, number)

  // ===================== State ========================
  override protected def restoreState(sb: StateBuilder): Unit =
    import sb.*
    state = scala.util.Try(CPUState.valueOf(r[String]("state"))).getOrElse(CPUState.EXECUTING)
    for dr <- dataRegs do
      dr.set(r[Int](s"D${dr.index}"),Size.Long)
    for ar <- addressRegs do
      ar.set(r[Int](s"A${ar.index}"), Size.Long)
    statusReg.set(r[Int]("statusReg"),Size.Long)
    userSPReg.set(r[Int]("userSPReg"), Size.Long)
    systemSPReg.set(r[Int]("systemSPReg"), Size.Long)
    pcReg.set(r[Int]("pcReg"), Size.Long)
    prefetchQueue.memptr = r[Int]("prefetch.memptr")
    prefetchQueue.irc = r[Int]("prefetch.irc")
    prefetchQueue.ird = r[Int]("prefetch.ird")
    pendingInterruptMask = r[Int]("pendingInterruptMask")
    nmiInterruptPending = r[Boolean]("nmiInterruptPending")
    pendingInterruptDelayCount = r[Int]("pendingInterruptDelayCount")
    busAvailable = r[Boolean]("busAvailable")
    dtackEnabled = r[Boolean]("dtackEnabled")
    totalElapsedCycles = r[String]("totalElapsedCycles").toLong

  override protected def createState(sb: StateBuilder): Unit =
    sb.w("state",state.toString)
    for dr <- dataRegs do
      sb.w(s"D${dr.index}",dr.get())
    for ar <- addressRegs do
      sb.w(s"A${ar.index}", ar.get())
    sb.
    w("statusReg",statusReg.get()).
    w("userSPReg",userSPReg.get()).
    w("systemSPReg",systemSPReg.get()).
    w("pcReg",pcReg.get()).
    w("prefetch.memptr",prefetchQueue.memptr).
    w("prefetch.irc",prefetchQueue.irc).
    w("prefetch.ird",prefetchQueue.ird).
    w("pendingInterruptMask",pendingInterruptMask).
    w("nmiInterruptPending",nmiInterruptPending).
    w("pendingInterruptDelayCount",pendingInterruptDelayCount).
    w("busAvailable",busAvailable).
    w("dtackEnabled",dtackEnabled).
    w("totalElapsedCycles",totalElapsedCycles)