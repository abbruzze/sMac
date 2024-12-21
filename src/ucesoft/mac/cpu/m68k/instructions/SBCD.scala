package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k
import ucesoft.mac.cpu.m68k.*

object SBCD:
  class SBCD(override protected val ctx: M6800X0.Context,
             override protected val opcode:Int,
             override protected val rtype:RegisterType) extends ABCD.ABCD(ctx,opcode,rtype):
    import RegisterType.*

    override val instructionType : InstructionType = InstructionType.SBCD

    override protected def bcd_op(s: Int, d: Int): Int =
      import StatusRegister.StatusFlag.*

      var x = flags.getFlag(X)
      var z = flags.getFlag(Z)
      var c = flags.getFlag(C)
      val dd = (d - s - x) & 0xFF
      val bc = ((~d & s) | (dd & ~d) | (dd & s)) & 0x88
      val corf = (bc - (bc >> 2)) & 0xFF
      val rr = (dd - corf) & 0xFF
      c = ((bc | (~dd & rr)) >> 7) & 0xFF
      x = c
      val v = ((dd & ~rr) >> 7) & 0xFF
      z &= (if rr == 0 then 1 else 0)
      val n = (rr >> 7) & 0xFF
      val ccr = ((x & 1) << X.ordinal) | ((n & 1) << N.ordinal) | ((z & 1) << Z.ordinal) | ((v & 1) << V.ordinal) | ((c & 1) << C.ordinal)
      flags.setCCR(ccr)
      rr

class SBCD(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     SBCD Dx,Dy
     SBCD –(Ax),–(Ay)
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 1 | 0 | 0 | 0 | Reg Dy/Ay | 1 | 0 | 0 | 0 | 0 |R/M| Reg Dx/Ax |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     Size=(Byte)

     Register Dy/Ay field—Specifies the destination register.
      If R/M = 0, specifies a data register.
      If R/M = 1, specifies an address register for the predecrement addressing mode.

     R/M field—Specifies the operand addressing mode.
      0 — The operation is data register to data register.
      1 — The operation is memory to memory.

     Register Dx/Ax field—Specifies the source register.
      If R/M = 0, specifies a data register.
      If R/M = 1, specifies an address register for the predecrement addressing mode.

     Condition codes:
     X — Set the same as the carry bit.
     N — Undefined.
     Z — Cleared if the result is nonzero; unchanged otherwise.
     V — Undefined.
     C — Set if a borrow (decimal) is generated; cleared otherwise.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    import RegisterType.*
    val code = genOpcode("1000___10000____")
    for (rm <- Seq(Data, Address))
      for (regSrc <- 0 to 7)
        for (regDst <- 0 to 7)
          val opcode = code | regDst << 9 | rm.ordinal << 3 | regSrc
          instructionSetHandler.registerInstruction(opcode, new SBCD.SBCD(ctx, opcode,rm))
