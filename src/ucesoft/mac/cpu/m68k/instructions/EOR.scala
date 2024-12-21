package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object EOR:
  class EOR(override protected val ctx: M6800X0.Context,
            override protected val opcode:Int,
            size:Size) extends Logical.Logical(ctx,opcode):
    import RegisterType.*

    override val instructionType : InstructionType = InstructionType.EOR
    protected final val register = (opcode >> 9) & 7

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
            EOR       |  INSTR     EA   |  1st OP (ea)  |          INSTR
    ------------------+-----------------+---------------+--------------------------
    Dn,<ea> :         |                 |               |
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
        Dn            |  8(1/0)  0(0/0) |               |               np       nn
        (An)          | 12(1/2)  8(2/0) |         nR nr |               np nw nW
        (An)+         | 12(1/2)  8(2/0) |         nR nr |               np nw nW
        -(An)         | 12(1/2) 10(2/0) | n       nR nr |               np nw nW
        (d16,An)      | 12(1/2) 12(3/0) |      np nR nr |               np nw nW
        (d8,An,Xn)    | 12(1/2) 14(3/0) | n    np nR nr |               np nw nW
        (xxx).W       | 12(1/2) 12(3/0) |      np nR nr |               np nw nW
        (xxx).L       | 12(1/2) 16(4/0) |   np np nR nr |               np nw nW
    */
    final override def execute(): Unit = {
      val eaOp = ctx.getEA(mode, reg, size)
      val dn = ctx.getRegister(Data, register)

      val a = eaOp.get(size)
      val b = dn.get(size)
      val r = a ^ b

      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================

      eaOp.set(r, size)

      setFlags(a,b,r,size)

      if size == Size.Long && eaOp.isRegisterMode then
        ctx.busIdle(4)
    }

    override def disassemble(address: Int): DisassembledInstruction = {
      val eaOp = ctx.getEA(mode,reg,size,Some(address))
      val dn = ctx.getRegister(Data,register)

      DisassembledInstruction(address,opcode,s"${instructionType.mnemonic}${size.ext}",eaOp.getExtensionWords,Some(dn.mnemonic),Some(eaOp.getMnemonic(address)))
    }

class EOR(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     EOR Dn, < ea >
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 1 | 0 | 1 | 1 |  Register |  Opmode   |   Mode    |   Reg     |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     Size = (Byte, Word, Long)

     Opmode:
     Byte Word Long         Operation
     100  101  110          Dn and <ea> -> <ea>

     X — Not affected.
     N — Set if the most significant bit of the result is set; cleared otherwise.
     Z — Set if the result is zero; cleared otherwise.
     V — Always cleared.
     C — Always cleared.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    import Size.*
    val code = genOpcode("1011____________")
    for(size <- Seq(Byte,Word,Long))
      for(register <- 0 to 7)
        for(mode <- Seq(0,2,3,4,5,6,7))
          val regEnd = if mode == 7 then 1 else 7
          for (reg <- 0 to regEnd)
            val opcode = code | register << 9 | (size.ordinal | 4) << 6 | mode << 3 | reg
            instructionSetHandler.registerInstruction(opcode, new EOR.EOR(ctx,opcode,size))
