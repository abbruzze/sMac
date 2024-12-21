package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object DIVU:
  class DIVU(override protected val ctx: M6800X0.Context,
             override protected val opcode:Int) extends Instruction(ctx,opcode):
    import RegisterType.*

    override val instructionType : InstructionType = InstructionType.DIVU
    protected final val register = (opcode >> 9) & 7

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
           DIVU       |  INSTR     EA   |  1st OP (ea)  |          INSTR
    ------------------+-----------------+---------------+--------------------------
    <ea>,Dn :        /                  |               |
      .W :          |                   |               |
        Dn          | 76+?(1/0)  0(0/0) |               | nn n [seq.] n nn np
        (An)        | 76+?(1/0)  4(1/0) |            nr | nn n [seq.] n nn np
        (An)+       | 76+?(1/0)  4(1/0) |            nr | nn n [seq.] n nn np
        -(An)       | 76+?(1/0)  6(1/0) | n          nr | nn n [seq.] n nn np
        (d16,An)    | 76+?(1/0)  8(2/0) |      np    nr | nn n [seq.] n nn np
        (d8,An,Xn)  | 76+?(1/0) 10(2/0) | n    np    nr | nn n [seq.] n nn np
        (xxx).W     | 76+?(1/0)  8(2/0) |      np    nr | nn n [seq.] n nn np
        (xxx).L     | 76+?(1/0) 12(2/0) |   np np    nr | nn n [seq.] n nn np
        #<data>     | 76+?(1/0)  8(2/0) |   np np       | nn n [seq.] n nn np
    NOTES :
      .Overflow always cost 10 cycles (n nn np).
      .For more information about division by zero see exceptions section below.
      .[seq.] refers to 15 consecutive blocks to be chosen in the following 3 :
       (nn), (nnn-) or (nnn-n).
       (see following flowchart for details).
      .Best case : 76 cycles (nn n [nn]*15 n nn np)
      .Worst case : 136 (140 ?) cycles.
    FLOWCHART :
                                           n
                                           n             Divide by zero
                      +--------------------+--------------------+
      Overflow        n                                         n
          +-----------+----------+                              n
         np                      n<---------------------+-+-+   nw
                                 n                      | | |   nw
                   No more bits  |  pMSB=0       pMSB=1 | | |   nw
                           +-----+-----+-----------+    | | |   np
                           n    MSB=0  n- MSB=1    |    | | |   np
                           np      +---+---+       +----+ | |   np
                                   |       n              | |   np
                                   |       +--------------+ |
                                   +------------------------+
      .for each iteration of the loop : shift dividend to the left by 1 bit then
       subtract divisor to the MSW of new dividend, discard after test if result
       is negative keep it if positive.
      .MSB = most significant bit : bit at the far left of the dividend
      .pMSB = previous MSB : MSB used in the previous iteration of the loop
    */
    final override def execute(): Unit =
      import StatusRegister.StatusFlag.*

      ctx.busIdle(4)
      val eaOp = ctx.getEA(mode,reg,Size.Word)
      val divisor = eaOp.get(Size.Word)

      flags.clearFlag(C)

      if divisor == 0 then
        // Divide by zero
        ctx.busIdle(4)
        ctx.raiseException(5)
      else {
        ctx.busIdle(2)
        val dividendReg = ctx.getRegister(RegisterType.Data,register)
        val dividend = dividendReg.get(Size.Long)

        // quotient & remainder calculus on long
        val dividendLong = dividend & 0xFFFFFFFFL
        val quotient = dividendLong / divisor

        // Overflow ?
        if quotient > 0xFFFFL then
          flags.setFlag(V)
          flags.clearFlag(C)
        else {
          val remainder = ((dividendLong % divisor) & 0xFFFF).asInstanceOf[Int]
          var cycles = 33 // 2 * 15 + 3 for exit
          // loop
          var c = 0
          var div = dividend & 0xFFFFFFFFL // must be unsigned
          val hdivisor = (divisor << 16) & 0xFFFFFFFFL
          while c < 15 do
            if (div & 0x80000000) != 0 then
              div = (div << 1) - hdivisor
            else
              div <<= 1
              if div >= hdivisor then
                div -= hdivisor
                cycles += 1
              else
                cycles += 2
            c += 1
          end while
          // end loop

          // set result
          if (quotient & 0x8000) != 0 then
            flags.setFlag(N)
            flags.clearFlag(Z)
          else
            flags.clearFlag(N)
            if quotient == 0 then flags.setFlag(Z) else flags.clearFlag(Z)

          flags.clearFlag(V)

          dividendReg.set(remainder << 16 | (quotient & 0xFFFF).asInstanceOf[Int],Size.Long)
          ctx.busIdle(cycles << 1)
        }
      }
      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================

    override def disassemble(address: Int): DisassembledInstruction =
      val eaOp = ctx.getEA(mode,reg,Size.Word,Some(address))
      DisassembledInstruction(address,opcode,instructionType.mnemonic,eaOp.getExtensionWords,Some(eaOp.getMnemonic(address)),Some(s"d$register"))

class DIVU(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     DIVS.W < ea > ,Dn
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 1 | 0 | 0 | 0 |  Register | 0 | 1 | 1 |   Mode    |    Reg    |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

    X — Not affected.
    N — Set if the quotient is negative; cleared otherwise; undefined if overflow or divide by zero occurs.
    Z — Set if the quotient is zero; cleared otherwise; undefined if overflow or divide by zero occurs.
    V — Set if division overflow occurs; undefined if divide by zero occurs; cleared otherwise.
    C — Always cleared.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    val code = genOpcode("1000___011______")
    for(register <- 0 to 7)
      for(mode <- Seq(0,2,3,4,5,6,7))
        val regEnd = if mode == 7 then 4 else 7
        for(reg <- 0 to regEnd)
          val opcode = code | register << 9 | mode << 3 | reg
          instructionSetHandler.registerInstruction(opcode,new DIVU.DIVU(ctx,opcode))
