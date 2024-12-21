package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object NEG:
  class NEG(override protected val ctx: M6800X0.Context,
            override protected val opcode:Int,
            protected val size:Size) extends Instruction(ctx,opcode):
    import RegisterType.*

    override val instructionType : InstructionType = InstructionType.NEG
    private final lazy val negx = instructionType == InstructionType.NEGX

    /*
    -------------------------------------------------------------------------------
            CLR,      |    Exec Time    |               Data Bus Usage
      NEGX, NEG, NOT  |  INSTR     EA   |  1st OP (ea)  |          INSTR
    ------------------+-----------------+---------------+--------------------------
    <ea> :            |                 |               |
      .B or .W :      |                 |               |
        Dn            |  4(1/0)  0(0/0) |               |               np
        (An)          |  8(1/1)  4(1/0) |            nr |               np nw
        (An)+         |  8(1/1)  4(1/0) |            nr |               np nw
        -(An)         |  8(1/1)  6(1/0) | n          nr |               np nw
        (d16,An)      |  8(1/1)  8(2/0) |      np    nr |               np nw
        (d8,An,Xn)    |  8(1/1) 10(2/0) | n    np    nr |               np nw
        (xxx).W       |  8(1/1)  8(2/0) |      np    nr |               np nw
        (xxx).L       |  8(1/1) 12(3/0) |   np np    nr |               np nw
      .L :            |                 |               |
        Dn            |  6(1/0)  0(0/0) |               |               np       n
        (An)          | 12(1/2)  8(2/0) |         nR nr |               np nw nW
        (An)+         | 12(1/2)  8(2/0) |         nR nr |               np nw nW
        -(An)         | 12(1/2) 10(2/0) | n       nR nr |               np nw nW
        (d16,An)      | 12(1/2) 12(3/0) |      np nR nr |               np nw nW
        (d8,An,Xn)    | 12(1/2) 14(3/0) | n    np nR nr |               np nw nW
        (xxx).W       | 12(1/2) 12(3/0) |      np nR nr |               np nw nW
        (xxx).L       | 12(1/2) 16(4/0) |   np np nR nr |               np nw nW
    */
    final override def execute(): Unit =
      import StatusRegister.StatusFlag.*
      val eaOp = ctx.getEA(mode,reg,size)
      val n = eaOp.get(size)
      val r = -n - (if negx then flags.getFlag(X) else 0)

      setFlags(n,r)

      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================

      eaOp.set(r,size)

      if size == Size.Long && eaOp.isRegisterMode then
        ctx.busIdle(2)

    protected def setFlags(n:Int,r:Int): Unit =
      import StatusRegister.StatusFlag.*
      var ccr = flags.getCCR
      val sm = (n & size.msb) != 0
      val rm = (r & size.msb) != 0
      val zm = (r & size.mask) == 0

      if rm then ccr |= N.flag else ccr &= ~N.flag
      if zm then
        ccr |= Z.flag
        ccr &= ~(C.flag | X.flag)
      else
        ccr &= ~Z.flag
        ccr |= C.flag | X.flag

      if sm && rm then ccr |= V.flag else ccr &= ~V.flag

      flags.setCCR(ccr)

    override def disassemble(address: Int): DisassembledInstruction =
      val eaOp = ctx.getEA(mode,reg,size,Some(address))
      DisassembledInstruction(address,opcode,s"${instructionType.mnemonic}${size.ext}",eaOp.getExtensionWords,Some(eaOp.getMnemonic(address)))

class NEG(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     NEG < ea >

     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 1 | 0 | 0 | 0 | 1 | 0 | 0 |  Size |    Mode   |    Reg    |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     Size = (Byte, Word, Long)

     Condition codes:
      X — Set the same as the carry bit.
      N — Set if the result is negative; cleared otherwise.
      Z — Set if the result is zero; cleared otherwise.
      V — Set if an overflow occurs; cleared otherwise.
      C — Cleared if the result is zero; set otherwise.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    import Size.*
    val code = genOpcode("01000100________")
    for(size <- Seq(Byte,Word,Long))
      for(mode <- Seq(0,2,3,4,5,6,7))
        val regEnd = if mode == 7 then 1 else 7
        for(reg <- 0 to regEnd)
          val opcode = code | size.ordinal << 6 | mode << 3 | reg
          instructionSetHandler.registerInstruction(opcode,new NEG.NEG(ctx,opcode,size))
