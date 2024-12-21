package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object CMP:
  class CMP(override protected val ctx: M6800X0.Context,
            override protected val opcode:Int,
            protected val size:Size) extends Instruction(ctx,opcode):

    override val instructionType : InstructionType = InstructionType.CMP
    protected final val register = (opcode >> 9) & 7

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
           CMP        |  INSTR     EA   |  1st OP (ea)  |          INSTR
    ------------------+-----------------+---------------+--------------------------
    <ea>,Dn :         |                 |               |
      .B or .W :      |                 |               |
        Dn            |  4(1/0)  0(0/0) |               |               np
        An            |  4(1/0)  0(0/0) |               |               np
        (An)          |  4(1/0)  4(1/0) |            nr |               np
        (An)+         |  4(1/0)  4(1/0) |            nr |               np
        -(An)         |  4(1/0)  6(1/0) | n          nr |               np
        (d16,An)      |  4(1/0)  8(2/0) |      np    nr |               np
        (d8,An,Xn)    |  4(1/0) 10(2/0) | n    np    nr |               np
        (xxx).W       |  4(1/0)  8(2/0) |      np    nr |               np
        (xxx).L       |  4(1/0) 12(3/0) |   np np    nr |               np
        #<data>       |  4(1/0)  4(1/0) |      np       |               np
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
    override def execute(): Unit = {
      val src = ctx.getEA(mode,reg,size).get(size)
      val dn = ctx.getRegister(RegisterType.Data,register).get(size)
      setFlags(src,dn,dn - src,size)

      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================

      if size == Size.Long then
        ctx.busIdle(2)
    }

    protected def setFlags(src:Int,dn:Int,cmp:Int,size:Size): Unit =
      import StatusRegister.StatusFlag.*
      val sm = (src & size.msb) != 0
      val dm = (dn & size.msb) != 0
      val rm = (cmp & size.msb) != 0
      val zm = (cmp & size.mask) == 0
      var ccr = flags.getCCR
      if rm then ccr |= N.flag else ccr &= ~N.flag
      if zm then ccr |= Z.flag else ccr &= ~Z.flag
      if (!sm && dm && !rm) || (sm && !dm && rm) then ccr |= V.flag else ccr &= ~V.flag
      if (sm && !dm) || (rm && !dm) || (sm && rm) then ccr |= C.flag else ccr &= ~C.flag
      flags.setCCR(ccr)

    override def disassemble(address: Int): DisassembledInstruction = {
      val eaOp = ctx.getEA(mode,reg,size,Some(address))
      DisassembledInstruction(address, opcode, s"${instructionType.mnemonic}${size.ext}", eaOp.getExtensionWords, Some(eaOp.getMnemonic(address)),Some(s"d$register"))
    }

class CMP(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     1) CMP < ea > , Dn
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 1 | 0 | 1 | 1 |  Register |  Opmode   |   Mode    |    Reg    |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     Size = (Byte,Word, Long)

    X — Not affected.
    N — Set if the result is negative; cleared otherwise.
    Z — Set if the result is zero; cleared otherwise.
    V — Set if an overflow occurs; cleared otherwise.
    C — Set if a borrow occurs; cleared otherwise.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    import Size.*
    val code = genOpcode("1011____________")
    for(size <- Seq(Byte,Word,Long))
      for(register <- 0 to 7)
        for(mode <- 0 to 7)
          if !(mode == 1 && size == Byte) then
            val regEnd = if mode == 7 then 4 else 7
            for(reg <- 0 to regEnd)
              val opcode = code | register << 9 | size.ordinal << 6 | mode << 3 | reg
              instructionSetHandler.registerInstruction(opcode, new CMP.CMP(ctx, opcode,size))