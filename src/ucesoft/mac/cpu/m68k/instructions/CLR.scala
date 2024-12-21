package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object CLR:
  class CLR(override protected val ctx: M6800X0.Context,
            override protected val opcode:Int,
            size:Size) extends Instruction(ctx,opcode):

    override val instructionType : InstructionType = InstructionType.CLR

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
        (xxx).L       |  8(1/1) 12(2/0) |   np np    nr |               np nw
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
    final override def execute(): Unit = {
      import StatusRegister.StatusFlag.*
      val eaOp = ctx.getEA(mode,reg,size)
      eaOp.get(size) // read & discard for 68000 & 68008
      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================
      eaOp.set(0,size)
      var ccr = flags.getCCR
      ccr |= Z.flag
      ccr &= ~(N.flag | V.flag | C.flag)
      flags.setCCR(ccr)

      if size == Size.Long && eaOp.isRegisterMode then
        ctx.busIdle(2)
    }

    override def disassemble(address: Int): DisassembledInstruction = {
      val eaOp = ctx.getEA(mode,reg,size,Some(address))
      DisassembledInstruction(address, opcode, s"${instructionType.mnemonic}${size.ext}", eaOp.getExtensionWords, Some(eaOp.getMnemonic(address)))
    }

class CLR(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     1) CLR < ea >
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 1 | 0 | 0 | 0 | 0 | 1 | 0 |  Size |   Mode    |    Reg    |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     Size = (Byte,Word, Long)

    X — Not affected.
    N — Always cleared.
    Z — Always set.
    V — Always cleared.
    C — Always cleared.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    import Size.*
    val code = genOpcode("01000010________")
    for(size <- Seq(Byte,Word,Long))
      for(mode <- Seq(0,2,3,4,5,6,7))
        val regEnd = if mode == 7 then 1 else 7
        for(reg <- 0 to regEnd)
          val opcode = code | size.ordinal << 6 | mode << 3 | reg
          instructionSetHandler.registerInstruction(opcode, new CLR.CLR(ctx, opcode,size))