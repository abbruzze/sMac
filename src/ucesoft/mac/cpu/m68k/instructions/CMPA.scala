package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object CMPA:
  class CMPA(override protected val ctx: M6800X0.Context,
             override protected val opcode:Int,
             override protected val size:Size) extends CMP.CMP(ctx,opcode,size):
   
    override val instructionType : InstructionType = InstructionType.CMPA

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
           CMPA       |  INSTR     EA   |  1st OP (ea)  |          INSTR
    ------------------+-----------------+---------------+--------------------------
    <ea>,An :         |                 |               |
      .B or .W :      |                 |               |
        Dn            |  6(1/0)  0(0/0) |               |               np       n
        An            |  6(1/0)  0(0/0) |               |               np       n
        (An)          |  6(1/0)  4(1/0) |            nr |               np       n
        (An)+         |  6(1/0)  4(1/0) |            nr |               np       n
        -(An)         |  6(1/0)  6(1/0) | n          nr |               np       n
        (d16,An)      |  6(1/0)  8(2/0) |      np    nr |               np       n
        (d8,An,Xn)    |  6(1/0) 10(2/0) | n    np    nr |               np       n
        (xxx).W       |  6(1/0)  8(2/0) |      np    nr |               np       n
        (xxx).L       |  6(1/0) 12(3/0) |   np np    nr |               np       n
        #<data>       |  6(1/0)  4(1/0) |      np       |               np       n
      .L :            |                 |               |
        Dn            |  6(1/0)  0(0/0) |               |               np       n
        An            |  6(1/0)  0(0/0) |               |               np       n
        (An)          |  6(1/0)  8(1/0) |         nR nr |               np       n
        (An)+         |  6(1/0)  8(1/0) |         nR nr |               np       n
        -(An)         |  6(1/0) 10(1/0) | n       nR nr |               np       n
        (d16,An)      |  6(1/0) 12(2/0) |      np nR nr |               np       n
        (d8,An,Xn)    |  6(1/0) 14(2/0) | n    np nR nr |               np       n
        (xxx).W       |  6(1/0) 12(2/0) |      np nR nr |               np       n
        (xxx).L       |  6(1/0) 16(3/0) |   np np nR nr |               np       n
        #<data>       |  6(1/0)  8(2/0) |   np np       |               np       n
    */
    final override def execute(): Unit = {
      val src = ctx.getEA(mode,reg,size).get(size,signExtended = true)
      val an = ctx.getRegister(RegisterType.Address,register).get(Size.Long)
      setFlags(src,an,an - src,Size.Long)

      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================
      ctx.busIdle(2)
    }

    override def disassemble(address: Int): DisassembledInstruction = {
      val eaOp = ctx.getEA(mode,reg,size,Some(address))
      DisassembledInstruction(address, opcode, s"${instructionType.mnemonic}${size.ext}", eaOp.getExtensionWords, Some(eaOp.getMnemonic(address)),Some(s"a$register"))
    }

class CMPA(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     1) CMPA < ea > , An
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 1 | 0 | 1 | 1 |  Register |  Opmode   |   Mode    |    Reg    |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     Size = (Word, Long)

    Word length source operands are sign-extended to 32 bits for comparison.

    X — Not affected.
    N — Set if the result is negative; cleared otherwise.
    Z — Set if the result is zero; cleared otherwise.
    V — Set if an overflow occurs; cleared otherwise.
    C — Set if a borrow occurs; cleared otherwise.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    import Size.*
    val code = genOpcode("1011____________")
    for(size <- Seq(Word,Long))
      for(register <- 0 to 7)
        for(mode <- 0 to 7)
          val regEnd = if mode == 7 then 4 else 7
          for(reg <- 0 to regEnd)
            val opcode = code | register << 9 | (if size == Size.Word then 3 else 7) << 6 | mode << 3 | reg
            instructionSetHandler.registerInstruction(opcode, new CMPA.CMPA(ctx, opcode,size))