package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object DIVS:
  class DIVS(override protected val ctx: M6800X0.Context,
             override protected val opcode:Int) extends Instruction(ctx,opcode):
    import RegisterType.*

    override val instructionType : InstructionType = InstructionType.DIVS
    protected final val register = (opcode >> 9) & 7

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
           DIVS       |  INSTR     EA   |  1st OP (ea)  |          INSTR
    ------------------+-----------------+---------------+--------------------------
    <ea>,Dn :       /                  /            /
     .W :         /                  /          /
      Dn        |120+?(1/0)  0(0/0)|          | nn nnn (n)n [seq] nnn-nnn n(n(n))np
      (An)      |120+?(1/0)  4(1/0)|        nr| nn nnn (n)n [seq] nnn-nnn n(n(n))np
      (An)+     |120+?(1/0)  4(1/0)|        nr| nn nnn (n)n [seq] nnn-nnn n(n(n))np
      -(An)     |120+?(1/0)  6(1/0)|n       nr| nn nnn (n)n [seq] nnn-nnn n(n(n))np
      (d16,An)  |120+?(1/0)  8(2/0)|     np nr| nn nnn (n)n [seq] nnn-nnn n(n(n))np
      (d8,An,Xn)|120+?(1/0) 10(2/0)|n    np nr| nn nnn (n)n [seq] nnn-nnn n(n(n))np
      (xxx).W   |120+?(1/0)  8(2/0)|     np nr| nn nnn (n)n [seq] nnn-nnn n(n(n))np
      (xxx).L   |120+?(1/0) 12(2/0)|  np np nr| nn nnn (n)n [seq] nnn-nnn n(n(n))np
      #<data>   |120+?(1/0)  8(2/0)|  np np   | nn nnn (n)n [seq] nnn-nnn n(n(n))np
    NOTES :
      .Overflow cost 16 or 18 cycles depending on dividend sign (n nn nn (n)n np).
      .For more informations about division by zero see exceptions section below.
      .[seq.] refers to 15 consecutive blocks to be chosen in the following 2 :
       (nnn-n) or (nn-n).
       (see following flowchart for details).
      .Best case : 120-122 cycles depending on dividend sign.
      .Worst case : 156 (158 ?) cycles.
    FLOWCHART :
                                             n
                                             n                       Divide by zero
                                    +--------+--------------------------------+
                                    n                                         n
                                    n                                         n
                      Dividend<0    n    Dividend>0                           nw
                              +-----+-----+                                   nw
                              n           |                                   nw
                              +---------->n                                   np
                                          |    Overflow                       np
            +-----------------+      +----+----+                              np
            | +----------+    +----->n         np                             np
            | |          +---------->n
            | |                      n-
            | | MSB=1      MSB=0     n   No more bits
            | | +-----------+--------+--------+
            | +-+           |                 n
            +---------------+   divisor<0     n        divisor>0
                                   +----------+-----------+
                                   n         dividend<0   n   dividend>0
                                   n           +----------+----+
                                   np          n               np
                                               n
                                               np
      .for each iteration of the loop : shift quotient to the left by 1 bit.
      .MSB = most significant bit : bit at the far left of the quotient.
    */
    final override def execute(): Unit =
      import StatusRegister.StatusFlag.*

      ctx.busIdle(4)
      val eaOp = ctx.getEA(mode,reg,Size.Word)
      val divisor = eaOp.get(Size.Word,signExtended = true)

      flags.clearFlag(C)

      if divisor == 0 then
        // Divide by zero
        ctx.busIdle(4)
        ctx.raiseException(5)
      else {
        val dividendReg = ctx.getRegister(RegisterType.Data,register)
        val dividend = dividendReg.get(Size.Long)
        var cycles = 0
        // Dividend < 0 ?
        cycles += (if dividend < 0 then 5 else 4)

        val quotient = dividend / divisor
        val remainder = dividend % divisor

        // Overflow ?
        if quotient > 32767 || quotient < -32768 then
          flags.setFlag(V)
          flags.clearFlag(C)
        else {
          cycles += 49
          // loop
          var c = 0
          var q = math.abs(dividend) / math.abs(divisor)
          while c < 15 do
            if (q & 0x8000) == 0 then cycles += 1
            q <<= 1
            c += 1
          end while
          // end loop
          if divisor < 0 then cycles += 4
          else
            if dividend < 0 then cycles += 5 else cycles += 3

          // set result
          if (quotient & 0x8000) != 0 then
            flags.setFlag(N)
            flags.clearFlag(Z)
          else
            flags.clearFlag(N)
            if quotient == 0 then flags.setFlag(Z) else flags.clearFlag(Z)

          flags.clearFlag(V)

          dividendReg.set((remainder & 0xFFFF) << 16 | quotient & 0xFFFF,Size.Long)
        }
        ctx.busIdle(cycles << 1)
      }
      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================

    override def disassemble(address: Int): DisassembledInstruction =
      val eaOp = ctx.getEA(mode,reg,Size.Word,Some(address))
      DisassembledInstruction(address,opcode,instructionType.mnemonic,eaOp.getExtensionWords,Some(eaOp.getMnemonic(address)),Some(s"d$register"))

class DIVS(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     DIVS.W < ea > ,Dn
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 1 | 0 | 0 | 0 |  Register | 1 | 1 | 1 |   Mode    |    Reg    |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

    X — Not affected.
    N — Set if the quotient is negative; cleared otherwise; undefined if overflow or divide by zero occurs.
    Z — Set if the quotient is zero; cleared otherwise; undefined if overflow or divide by zero occurs.
    V — Set if division overflow occurs; undefined if divide by zero occurs; cleared otherwise.
    C — Always cleared.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    val code = genOpcode("1000___111______")
    for(register <- 0 to 7)
      for(mode <- Seq(0,2,3,4,5,6,7))
        val regEnd = if mode == 7 then 4 else 7
        for(reg <- 0 to regEnd)
          val opcode = code | register << 9 | mode << 3 | reg
          instructionSetHandler.registerInstruction(opcode,new DIVS.DIVS(ctx,opcode))
