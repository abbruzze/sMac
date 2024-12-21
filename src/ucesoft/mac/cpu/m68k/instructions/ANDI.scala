package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object ANDI:
  class ANDI(override protected val ctx: M6800X0.Context,
             override protected val opcode:Int,
             size:Size) extends Logical.Logical(ctx,opcode):

    override val instructionType : InstructionType = InstructionType.ANDI

    /*
    -------------------------------------------------------------------------------
     EORI, ORI, ANDI, |    Exec Time    |               Data Bus Usage
        SUBI, ADDI    |  INSTR     EA   | 1st Operand |  2nd OP (ea)  |   INSTR
    ------------------+-----------------+-------------+---------------+------------
    #<data>,<ea> :    |                 |             |               |
      .B or .W :      |                 |             |               |
        Dn            |  8(2/0)  0(0/0) |          np |               | np
        (An)          | 12(2/1)  4(1/0) |          np |            nr | np nw
        (An)+         | 12(2/1)  4(1/0) |          np |            nr | np nw
        -(An)         | 12(2/1)  6(1/0) |          np | n          nr | np nw
        (d16,An)      | 12(2/1)  8(2/0) |          np |      np    nr | np nw
        (d8,An,Xn)    | 12(2/1) 10(2/0) |          np | n    np    nr | np nw
        (xxx).W       | 12(2/1)  8(2/0) |          np |      np    nr | np nw
        (xxx).L       | 12(2/1) 12(3/0) |          np |   np np    nr | np nw
      .L :                              |             |               |
        Dn            | 16(3/0)  0(0/0) |       np np |               | np       nn
        (An)          | 20(3/2)  8(2/0) |       np np |         nR nr | np nw nW
        (An)+         | 20(3/2)  8(2/0) |       np np |         nR nr | np nw nW
        -(An)         | 20(3/2) 10(2/0) |       np np | n       nR nr | np nw nW
        (d16,An)      | 20(3/2) 12(3/0) |       np np |      np nR nr | np nw nW
        (d8,An,Xn)    | 20(3/2) 14(3/0) |       np np | n    np nR nr | np nw nW
        (xxx).W       | 20(3/2) 12(3/0) |       np np |      np nR nr | np nw nW
        (xxx).L       | 20(3/2) 16(4/0) |       np np |   np np nR nr | np nw nW
    */
    final override def execute(): Unit = {
      val immOp = ctx.getEA(AddressingMode.IM, 0, size, None)
      val b = immOp.get(size)
      val eaOp = ctx.getEA(mode, reg, size)
      val a = eaOp.get(size)
      val r = a & b
      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================
      eaOp.set(r, size)
      setFlags(a, b, r, size)

      if size == Size.Long && mode == 0 then
        ctx.busIdle(4)
    }

    override def disassemble(address: Int): DisassembledInstruction = {
      val eaOp = ctx.getEA(mode, reg, size, Some(address))
      val immOp = ctx.getEA(AddressingMode.IM, 0, size, Some(address))

      DisassembledInstruction(address, opcode, s"${instructionType.mnemonic}${size.ext}", immOp.getExtensionWords ++ eaOp.getExtensionWords, Some(immOp.getMnemonic(address)), Some(eaOp.getMnemonic(address)))
    }

class ANDI(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 0 | 0 | 0 | 0 | 0 | 1 | 0 | Size  |   Mode    |   Reg     |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     Size = (Byte, Word, Long)
  
     X — Not affected.
     N — Set if the most significant bit of the result is set; cleared otherwise.
     Z — Set if the result is zero; cleared otherwise.
     V — Always cleared.
     C — Always cleared.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    import Size.*
    val code = genOpcode("00000010________")
    for (size <- Seq(Byte, Word, Long)) {
      for (mode <- Seq(0, 2, 3, 4, 5, 6, 7)) {
        val regEnd = if mode == 7 then 1 else 7
        for (reg <- 0 to regEnd) {
          val opcode = code | size.ordinal << 6 | mode << 3 | reg
          instructionSetHandler.registerInstruction(opcode, new ANDI.ANDI(ctx, opcode, size))
        }
      }
    }
