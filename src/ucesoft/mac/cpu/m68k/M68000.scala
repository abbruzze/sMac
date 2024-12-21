package ucesoft.mac.cpu.m68k

import ucesoft.mac.cpu.m68k.M6800X0.{BusAccessMode, EventListener, Snapshot}
import ucesoft.mac.cpu.m68k.Memory.{ByteBufferMemory, MapMemory}

import java.io.FileInputStream

class M68000(override val mem:Memory) extends M68KCore(mem):
  override protected val componentName = "M68K"
  import ucesoft.mac.cpu.m68k.instructions.*
  override lazy val model: Model = Model._68K

  protected lazy val INSTRUCTION_SET : Array[InstructionGenerator] = Array(
    new ABCD(ctx), new ADD(ctx), new ADDA(ctx), new ADDI(ctx), new ADDQ(ctx), new ADDX(ctx), new AND(ctx), new ANDI(ctx),
    new ANDI2CCR(ctx), new ANDI2SR(ctx), new ASL_ASR(ctx), new Bcc(ctx), new BCHG(ctx), new BCLR(ctx), new BSET(ctx), new BTST(ctx),
    new CHK(ctx), new CLR(ctx), new CMP(ctx), new CMPA(ctx), new CMPI(ctx), new CMPM(ctx), new DBcc(ctx),new DIVS(ctx),
    new DIVU(ctx), new EOR(ctx), new EORI(ctx), new EORI2CCR(ctx), new EORI2SR(ctx), new EXG(ctx), new EXT(ctx),  new ILLEGAL(ctx),
    new JMP(ctx), new JSR(ctx), new LEA(ctx), new LINK(ctx), new LSL_LSR(ctx), new MOVE(ctx), new MOVEA(ctx), new MOVEFromSR(ctx),
    new MOVEToCCR(ctx), new MOVEToSR(ctx), new MOVE_USP(ctx), new MOVEM(ctx), new MOVEP(ctx), new MOVEQ(ctx), new MULS(ctx), new MULU(ctx),
    new NBCD(ctx), new NEG(ctx), new NEGX(ctx), new NOP(ctx), new NOT(ctx), new OR(ctx), new ORI(ctx), new ORI2CCR(ctx), new ORI2SR(ctx),
    new PEA(ctx), new RESET(ctx), new ROL_ROR(ctx), new ROXL_ROXR(ctx), new RTE(ctx), new RTR(ctx), new RTS(ctx), new SBCD(ctx),
    new Scc(ctx), new STOP(ctx), new SUB(ctx), new SUBA(ctx), new SUBI(ctx), new SUBQ(ctx), new SUBX(ctx), new SWAP(ctx), new TAS(ctx),
    new TRAP(ctx), new TRAPV(ctx), new TST(ctx), new UNLK(ctx)
  )

  override protected def buildInstructionSet(): Array[Instruction] =
    val iset = Array.ofDim[Instruction](0x10000)

    for h <- INSTRUCTION_SET do
      h.generate((opcode,i) => {
        if iset(opcode) != null then throw new IllegalArgumentException(s"Opcode ${opcode.toHexString} with instruction ${i.instructionType} already set with ${iset(opcode).instructionType}")
        iset(opcode) = i
      })

    iset

  override protected def buildAddressingModes(): Array[Operand] =
    import ucesoft.mac.cpu.m68k.addressingModes.*
    Array(
      new DataRegisterDirectMode(ctx),
      new AddressRegisterDirectMode(ctx),
      new AddressRegisterIndirectMode(ctx),
      new AddressRegisterIndirectPostIncrementMode(ctx),
      new AddressRegisterIndirectPreDecrementMode(ctx),
      new AddressRegisterIndirectWithDisplacementMode(ctx),
      new AddressRegisterIndirectWithIndexMode(ctx),
      new AbsoluteShortMode(ctx),
      new AbsoluteLongMode(ctx),
      new ProgramCounterWithDisplacementMode(ctx),
      new ProgramCounterWithIndexMode(ctx),
      new ImmediateMode(ctx)
    )

  override final def checkMemoryAddress(address: Int, busAccessMode: BusAccessMode, size:Size, isCodeAccess: Boolean, instructionInProgress:Boolean = true): Unit =
    if (size == Size.Word || size == Size.Long) && (address & 1) == 1 then
      throw new AddressBusException(address,busAccessMode,instructionInProgress = instructionInProgress,isCodeAccess)

  override final def execute(): Int =
    if !dtackEnabled || !busAvailable then // on last read/write dtack was not asserted, must wait
      return 1

    lastInstructionElapsedCycles = instructionElapsedCycles
    instructionElapsedCycles = 0
    instructionExceptionNumber = -1
    waitCycles = 0

    var opcode = 0
    try
      // check reset condition
      if pendingResetRequest then
        pendingResetRequest = false
        throw new ResetException

      // check interrupt
      if (nmiInterruptPending || pendingInterruptMask > statusReg.getInterruptMask) && pendingInterruptDelayCount == 0 then
        serveInterrupt()
        if intAckListener != null then
          intAckListener.intAcknowledged(pendingInterruptMask)
      else {
        if pendingInterruptDelayCount > 0 then
          pendingInterruptDelayCount -= 1

        if state == CPUState.STOPPED || state == CPUState.HALTED then
          busAccess(BusAccessMode.Idle, Size.Word, codeAccess = false, 0, 0, 2)
          return 2

        pc0 = pcReg.get()

        if busTracing then
          lastBusTraces = Nil

        opcode = prefetchQueue.ird
        lastInstruction = instructionSet(opcode)
        if notifyEventListeners then fireFetchEvent(pc0,opcode,lastInstruction)
        if lastInstruction == null then
          val bit15_12 = opcode >> 12
          if bit15_12 == 10 || bit15_12 == 15 then
            throw new UnimplementedInstruction(bit15_12)
          else throw new IllegalInstruction

        pcReg.increment(Size.Word)

        if lastInstruction.instructionType.needsSupervisorMode && !statusReg.isSupervisorMode then
          throw new PrivilegeViolation

        val trace = statusReg.isTrace
        lastInstruction.execute()

        // check Tracing
        if trace then
          throw new TraceException
      }
    catch
      case _:BUSNotAvailable =>
        // restore PC
        pcReg.set(pc0,Size.Long)
        if notifyEventListeners then fireFetchEvent(pc0,opcode,lastInstruction)
        return 1
      case _:ResetException =>
        resetCPU()
      case ui: UnimplementedInstruction =>
        busAccess(BusAccessMode.Idle, Size.Word, codeAccess = false, 0, 0, 4)
        if ui.pattern == 10 then
          raiseException(10)
        else
          raiseException(11)
        fillPrefetchQueue(true)
      case _: IllegalInstruction =>
        busAccess(BusAccessMode.Idle, Size.Word, codeAccess = false, 0, 0, 4)
        raiseException(4)
        fillPrefetchQueue(true)
      case _: PrivilegeViolation =>
        busAccess(BusAccessMode.Idle, Size.Word, codeAccess = false, 0, 0, 4)
        raiseException(8)
        fillPrefetchQueue(true)
      case e:AddressBusException =>
        handleAddressBusException(e,opcode)
        raiseException(3,pushStackFrame = false)
        fillPrefetchQueue(true)
      case _:TraceException =>
        busAccess(BusAccessMode.Idle, Size.Word, codeAccess = false, 0, 0, 4)
        raiseException(9)
        fillPrefetchQueue(true)

    instructionElapsedCycles += waitCycles
    totalElapsedCycles += instructionElapsedCycles
    instructionElapsedCycles
  end execute
