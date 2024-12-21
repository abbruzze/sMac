package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object TST:
  class TST(override protected val ctx: M6800X0.Context,
            override protected val opcode:Int,
            size:Size) extends Instruction(ctx,opcode):
    import RegisterType.*

    override val instructionType : InstructionType = InstructionType.TST

    /*
    -------------------------------------------------------------------------------
                      |     Exec Time   |               Data Bus Usage
           TST        |  INSTR     EA   |  1st OP (ea)  |          INSTR
    ------------------+-----------------+---------------+--------------------------
    <ea> :            |                 |               |
      .B or .W :      |                 |               |
        Dn            |  4(1/0)  0(0/0) |               |               np
        (An)          |  4(1/0)  4(1/0) |            nr |               np
        (An)+         |  4(1/0)  4(1/0) |            nr |               np
        -(An)         |  4(1/0)  6(1/0) | n          nr |               np
        (d16,An)      |  4(1/0)  8(2/0) |      np    nr |               np
        (d8,An,Xn)    |  4(1/0) 10(2/0) | n    np    nr |               np
        (xxx).W       |  4(1/0)  8(2/0) |      np    nr |               np
        (xxx).L       |  4(1/0) 12(3/0) |   np np    nr |               np
      .L              |                 |               |
        Dn            |  4(1/0)  0(0/0) |               |               np
        (An)          |  4(1/0)  8(2/0) |         nR nr |               np
        (An)+         |  4(1/0)  8(2/0) |         nR nr |               np
        -(An)         |  4(1/0) 10(2/0) | n       nR nr |               np
        (d16,An)      |  4(1/0) 12(3/0) |      np nR nr |               np
        (d8,An,Xn)    |  4(1/0) 14(3/0) | n    np nR nr |               np
        (xxx).W       |  4(1/0) 12(3/0) |      np nR nr |               np
        (xxx).L       |  4(1/0) 16(4/0) |   np np nR nr |               np
    */
    final override def execute(): Unit =
      import StatusRegister.StatusFlag.*
      val eaOp = ctx.getEA(mode,reg,size)
      val test = eaOp.get(size)
      var ccr = flags.getCCR
      if (test & size.msb) != 0 then ccr |= N.flag else ccr &= ~N.flag
      if (test & size.mask) == 0 then ccr |= Z.flag else ccr &= ~Z.flag
      ccr &= ~(C.flag | V.flag)
      flags.setCCR(ccr)
      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================

    override def disassemble(address: Int): DisassembledInstruction =
      val eaOp = ctx.getEA(mode,reg,size,Some(address))
      DisassembledInstruction(address,opcode,s"${instructionType.mnemonic}${size.ext}",eaOp.getExtensionWords,Some(eaOp.getMnemonic(address)))

class TST(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     TST < ea >
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 1 | 0 | 0 | 1 | 0 | 1 | 0 |  Size |   Mode    |    Reg    |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

      Size = (Byte, Word, Long)

     Condition codes:
      X — Not affected.
      N — Set if the operand is negative; cleared otherwise.
      Z — Set if the operand is zero; cleared otherwise.
      V — Always cleared.
      C — Always cleared.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    import Size.*
    val code = genOpcode("01001010________")
    for(size <- Seq(Byte,Word,Long))
      for(mode <- List(0,2,3,4,5,6,7))
        val regEnd = if mode == 7 then 1 else 7
        for(reg <- 0 to regEnd)
          val opcode = code | size.ordinal << 6 | mode << 3 | reg
          instructionSetHandler.registerInstruction(opcode,new TST.TST(ctx,opcode,size))
