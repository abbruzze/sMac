package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object MULS:
  class MULS(override protected val ctx: M6800X0.Context,
             override protected val opcode:Int) extends Instruction(ctx,opcode):
    import RegisterType.*

    override val instructionType : InstructionType = InstructionType.MULS
    private final val register = (opcode >> 9) & 7

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
        MULU,MULS     |  INSTR     EA   |  1st OP (ea)  |          INSTR
    ------------------+-----------------+---------------+--------------------------
    <ea>,Dn :        /                  |               |
      .W :          /                   |               |
        Dn         | 38+2m(1/0)  0(0/0) |               |               np       n*
        (An)       | 38+2m(1/0)  4(1/0) |            nr |               np       n*
        (An)+      | 38+2m(1/0)  4(1/0) |            nr |               np       n*
        -(An)      | 38+2m(1/0)  6(1/0) | n          nr |               np       n*
        (d16,An)   | 38+2m(1/0)  8(2/0) |      np    nr |               np       n*
        (d8,An,Xn) | 38+2m(1/0) 10(2/0) | n    np    nr |               np       n*
        (xxx).W    | 38+2m(1/0)  8(2/0) |      np    nr |               np       n*
        (xxx).L    | 38+2m(1/0) 12(2/0) |   np np    nr |               np       n*
        #<data>    | 38+2m(1/0)  4(1/0) |      np       |               np       n*

    NOTES :
      .for MULU 'm' = the number of ones in the source
        - Best case 38 cycles with $0
        - Worst case : 70 cycles with $FFFF
      .for MULS 'm' = concatenate the 16-bit pointed by <ea> with a zero as the LSB
       'm' is the resultant number of 10 or 01 patterns in the 17-bit source.
        - Best case : 38 cycles with $0 or $FFFF
        - Worst case : 70 cycles with $5555
      .in both cases : 'n*' should be replaced by 17+m consecutive 'n'
    FLOWCHART :
                                np
             LSB=1              |           LSB=0
                   +------------+------------+
                   |                         |
                +->n------------------------>n<----+
                |                            |     | LSB=0 or
                |           No more bits     |     | 2LSB=00,11
       LSB=1 or +---------------+------------+-----+
       2LSB=01,10               |
                                n
      .LSB = less significant bit : bit at the far right of the source.
      .2LSB = 2 less significant bits : 2 last bits at the far right of the source.
    */
    final override def execute(): Unit =
      import StatusRegister.StatusFlag.*
      val eaOp = ctx.getEA(mode,reg,Size.Word)
      val m1 = eaOp.get(Size.Word,signExtended = true)
      val m2Reg = ctx.getRegister(RegisterType.Data,register)
      val m2 = m2Reg.get(Size.Word,signExtended = true)
      val r = m1 * m2

      var ccr = flags.getCCR
      if r < 0 then ccr |= N.flag else ccr &= ~N.flag
      if r == 0 then ccr |= Z.flag else ccr &= ~Z.flag
      ccr &= ~(V.flag | C.flag)
      flags.setCCR(ccr)

      m2Reg.set(r,Size.Long)

      val m = m1 & 0xFFFF
      var pattern = ((m << 1) ^ m) & 0xFFFF
      var cycles = 17

      while pattern != 0 do
        if (pattern & 1) == 1 then cycles += 1
        pattern >>= 1
      end while

      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================

      ctx.busIdle(cycles << 1)

    override def disassemble(address: Int): DisassembledInstruction =
      val eaOp = ctx.getEA(mode,reg,Size.Word,Some(address))
      DisassembledInstruction(address,opcode,instructionType.mnemonic,Nil,Some(eaOp.getMnemonic(address)),Some(s"d$register"))

class MULS(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     MULS.W < ea > ,Dn
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 1 | 1 | 0 | 0 |  Register | 1 | 1 | 1 |    Mode   |    Reg    |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     Size = (Word)

     Condition codes:
     X — Not affected.
     N — Set if the result is negative; cleared otherwise.
     Z — Set if the result is zero; cleared otherwise.
     V — Set if overflow; cleared otherwise. Never happen for Word size
     C — Always cleared.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    val code = genOpcode("1100___111______")
    for(register <- 0 to 7)
      for(mode <- Seq(0,2,3,4,5,6,7))
        val regEnd = if mode == 7 then 4 else 7
        for(reg <- 0 to regEnd)
          val opcode = code | register << 9 | mode << 3 | reg
          instructionSetHandler.registerInstruction(opcode,new MULS.MULS(ctx,opcode))
