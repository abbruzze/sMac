package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object CMPI:
  class CMPI(override protected val ctx: M6800X0.Context,
             override protected val opcode:Int,
             override protected val size:Size) extends CMP.CMP(ctx,opcode,size):

    override val instructionType : InstructionType = InstructionType.CMPI

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
           CMPI       |  INSTR     EA   | 1st Operand |  2nd OP (ea)  |   INSTR
    ------------------+-----------------+-------------+-------------+--------------
    #<data>,<ea> :    |                 |             |               |
      .B or .W :      |                 |             |               |
        Dn            |  8(2/0)  0(0/0) |          np |               | np
        (An)          |  8(2/0)  4(1/0) |          np |            nr | np
        (An)+         |  8(2/0)  4(1/0) |          np |            nr | np
        -(An)         |  8(2/0)  6(1/0) |          np | n          nr | np
        (d16,An)      |  8(2/0)  8(2/0) |          np |      np    nr | np
        (d8,An,Xn)    |  8(2/0) 10(2/0) |          np | n    np    nr | np
        (xxx).W       |  8(2/0)  8(2/0) |          np |      np    nr | np
        (xxx).L       |  8(2/0) 12(3/0) |          np |   np np    nr | np
      .L :            |                 |             |               |
        Dn            | 14(3/0)  0(0/0) |       np np |               | np       n
        (An)          | 12(3/0)  8(2/0) |       np np |         nR nr | np
        (An)+         | 12(3/0)  8(2/0) |       np np |         nR nr | np
        -(An)         | 12(3/0) 10(2/0) |       np np | n       nR nr | np
        (d16,An)      | 12(3/0) 12(3/0) |       np np |      np nR nr | np
        (d8,An,Xn)    | 12(3/0) 14(3/0) |       np np | n    np nR nr | np
        (xxx).W       | 12(3/0) 12(3/0) |       np np |      np nR nr | np
        (xxx).L       | 12(3/0) 16(4/0) |       np np |   np np nR nr | np
    */
    final override def execute(): Unit = {
      val src = ctx.getEA(AddressingMode.IM, 0, size, None).get(size)
      val dst = ctx.getEA(mode,reg,size).get(size)
      setFlags(src,dst,dst - src,size)

      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================

      if size == Size.Long && mode == 0 then
        ctx.busIdle(2)
    }

    override def disassemble(address: Int): DisassembledInstruction = {
      val imm = ctx.getEA(AddressingMode.IM, 0, size, Some(address))
      val eaOp = ctx.getEA(mode,reg,size,Some(address + size.bytes))
      DisassembledInstruction(address, opcode, s"${instructionType.mnemonic}${size.ext}", eaOp.getExtensionWords ++ imm.getExtensionWords, Some(imm.getMnemonic(address)),Some(eaOp.getMnemonic(address + size.bytes)))
    }

class CMPI(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     1) CMPI # < data > , < ea >
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 0 | 0 | 0 | 1 | 1 | 0 | 0 | Size  |   Mode    |    Reg    |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     |   16bit word data             |       8bit byte data          |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     |                       32bit long data                         |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     Size = (Word, Long)

    X — Not affected.
    N — Set if the result is negative; cleared otherwise.
    Z — Set if the result is zero; cleared otherwise.
    V — Set if an overflow occurs; cleared otherwise.
    C — Set if a borrow occurs; cleared otherwise.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    import Size.*
    val code = genOpcode("00001100________")
    for(size <- Seq(Byte,Word,Long))
      for(mode <- Seq(0,2,3,4,5,6,7))
        val regEnd = if mode == 7 then 1 else 7
        for(reg <- 0 to regEnd)
          val opcode = code | size.ordinal << 6 | mode << 3 | reg
          instructionSetHandler.registerInstruction(opcode, new CMPI.CMPI(ctx, opcode,size))