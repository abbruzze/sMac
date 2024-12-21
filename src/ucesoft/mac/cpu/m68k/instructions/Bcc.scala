package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*
import ucesoft.mac.cpu.m68k.M6800X0.BusAccessMode

object Bcc:
  class Bcc(override protected val ctx: M6800X0.Context,
            override protected val opcode:Int) extends Instruction(ctx,opcode):
    import RegisterType.*
    import InstructionType.*

    override val isBranch = true
    protected final val conditionCode = (opcode >> 8) & 0xF
    private val instructions = Array(
      BRA, BSR, BHI, BLS, BCC, BCS, BNE, BEQ,
      BVC, BVS, BPL, BMI, BGE, BLT, BGT, BLE
    )
    override val instructionType : InstructionType = instructions(conditionCode)
    private final val displacement = opcode & 0xFF
    private final val displacementIsWord = displacement == 0x00


    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
            BRA       |      INSTR      |                   INSTR
    ------------------+-----------------+------------------------------------------
    <label> :         |                 |
      .B, .S or .W :  | 10(2/0)         |                 n          np np

    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
            BSR       |      INSTR      |                   INSTR
    ------------------+-----------------+------------------------------------------
    <label> :         |                 |
      .B .S or .W :   | 18(2/2)         |                 n    nS ns np np

    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
            Bcc       |      INSTR      |                   INSTR
    ------------------+-----------------+------------------------------------------
    <label> :         |                 |
     .B or .S :       |                 |
      branch taken    | 10(2/0)         |                 n          np np
      branch not taken|  8(1/0)         |                nn             np
     .W :             |                 |
      branch taken    | 10(2/0)         |                 n          np np
      branch not taken| 12(2/0)         |                nn          np np
    */
    override def execute(): Unit = {
      // read PC value before reading displacement (PC will be incremented)
      val pcValue = ctx.getPC

      if conditionCode == 0x00 || conditionCode == 0x01 then
        ctx.busIdle(2)

      val disp = if displacementIsWord  then
        extendSign(Size.Word,ctx.fetchWord())
      else
        extendSign(Size.Byte,displacement)

      conditionCode match
        case 0x00 => // BRA
          // PC = PC + disp
          ctx.branch(pcValue + disp)
        case 0x01 => // BSR
          // PC = PC + disp
          ctx.branch(pcValue + disp) // check address bus error before decrementing SP
          // push PC
          val sp = ctx.getRegister(RegisterType.SP)
          sp.decrement(Size.Long)
          val pcToPush = pcValue + (if displacementIsWord then 2 else 0)
          ctx.writeMemory(sp.get(Size.Long),pcToPush,Size.Long,BusAccessMode.Push)
          if displacementIsWord then ctx.adjustCycles(-4)
        case _ =>
          if testCC then
            ctx.busIdle(2)
            if displacementIsWord then ctx.adjustCycles(-4)
            // PC = PC + disp
            ctx.branch(pcValue + disp)
          else // branch not taken
            ctx.busIdle(4)
            //ctx.adjustCycles(-4)
      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================
    }

    protected def testCC: Boolean =
      import StatusRegister.StatusFlag.*
      val ccr = flags.getCCR
      conditionCode match
        case 0x00 /* T */     => true
        case 0x01 /* F */     => false
        case 0x02 /* HI */    => (ccr & (C.flag | Z.flag)) == 0
        case 0x03 /* LS */    => (ccr & (C.flag | Z.flag)) != 0
        case 0x04 /* CC */    => (ccr & C.flag) == 0
        case 0x05 /* CS */    => (ccr & C.flag) != 0
        case 0x06 /* NE */    => (ccr & Z.flag) == 0
        case 0x07 /* EQ */    => (ccr & Z.flag) != 0
        case 0x08 /* VC */    => (ccr & V.flag) == 0
        case 0x09 /* VS */    => (ccr & V.flag) != 0
        case 0x0A /* PL */    => (ccr & N.flag) == 0
        case 0x0B /* MI */    => (ccr & N.flag) != 0
        case 0x0C /* GE */    => val nv = N.flag | V.flag ; (ccr & nv) == 0 || (ccr & nv) == nv
        case 0x0D /* LT */    => val nv = N.flag | V.flag ; (ccr & nv) == N.flag || (ccr & nv) == V.flag
        case 0x0E /* GT */    => val nvz = N.flag | V.flag | Z.flag; (ccr & nvz) == 0 || (ccr & nvz) == (N.flag | V.flag)
        case 0x0F /* LE */    => val nv = N.flag | V.flag; (ccr & Z.flag) == Z.flag || (ccr & nv) == N.flag || (ccr & nv) == V.flag


    override def disassemble(address: Int): DisassembledInstruction = {
      val disp = if displacementIsWord then
        val immOp = ctx.getEA(AddressingMode.IM, 0, Size.Word, Some(address))
        immOp.get(Size.Word,signExtended = true)
      else
        extendSign(Size.Byte,displacement)

      val targetAddress = address + 2 + disp
      DisassembledInstruction(address, opcode, s"${instructionType.mnemonic}", if displacementIsWord then List(disp) else Nil, Some(targetAddress.toHexString))
    }

class Bcc(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     1) bhi, bls, bcc, bcs, bne, beq, bvc, bvs, bpl, bmi, bge, blt, bgt, ble
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 1 | 1 | 0 |   Condition   |       Displacement            |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     |          16bit displacement if Displacement = 0x00            |
     +---------------------------------------------------------------+
     |          32bit displacement if Displacement = 0xFF            |                                       |
     +---------------------------------------------------------------+
    2) bra Condition = 000
    3) brs Condition = 001

     Size = (Byte, Word, Long*)
     * MC68020, MC68030, and MC68040 only

    Mnemonic Condition          Encoding   Test
    =================================================================
    T*          True              0000    1
    F*          False             0001    0
    HI          High              0010    !(C or Z)
    LS          Low or Same       0011    C or Z
    CC(HI)      Carry Clear       0100    !C
    CS(LO)      Carry Set         0101    C
    NE          Not Equal         0110    !Z
    EQ          Equal             0111    Z
    VC          Overflow Clear    1000    !V
    VS          Overflow Set      1001    V
    PL          Plus              1010    !N
    MI          Minus             1011    N
    GE          Greater or Equal  1100    N and V or !N and !V
    LT          Less Than         1101    N and !V or !N and V
    GT          Greater Than      1110    N and V and !Z or !N and !V and !Z
    LE          Less or Equal     1111    Z or N and !V or !N and V

  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    val code = genOpcode("0110____________")
    for(condition <- 0 to 15)
      for(displacement <- 0 to 255)
        val opcode = code | condition << 8 | displacement
        instructionSetHandler.registerInstruction(opcode,new Bcc.Bcc(ctx,opcode))
