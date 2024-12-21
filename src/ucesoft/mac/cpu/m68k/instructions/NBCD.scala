package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k
import ucesoft.mac.cpu.m68k.*

object NBCD:
  class NBCD(override protected val ctx: M6800X0.Context,
             override protected val opcode:Int) extends SBCD.SBCD(ctx,opcode,RegisterType.Data):
    import RegisterType.*

    override val instructionType : InstructionType = InstructionType.NBCD

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
           NBCD       |      INSTR      |  1st OP (ea)  |          INSTR
    ------------------+-----------------+---------------+--------------------------
    <ea> :            |                 |               |
      .B :            |                 |               |
        Dn            |  6(1/0)  0(0/0) |               |               np       n
        (An)          |  8(1/1)  4(1/0) |            nr |               np nw
        (An)+         |  8(1/1)  4(1/0) |            nr |               np nw
        -(An)         |  8(1/1)  6(1/0) | n          nr |               np nw
        (d16,An)      |  8(1/1)  8(2/0) |      np    nr |               np nw
        (d8,An,Xn)    |  8(1/1) 10(2/0) | n    np    nr |               np nw
        (xxx).W       |  8(1/1)  8(2/0) |      np    nr |               np nw
        (xxx).L       |  8(1/1) 12(3/0) |   np np    nr |               np nw
    */
    final override def execute(): Unit =
      val eaOp = ctx.getEA(mode,reg,Size.Byte)
      val d = eaOp.get(Size.Byte)
      val r = bcd_op(d,0)
      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================
      eaOp.set(r,Size.Byte)
      if eaOp.isRegisterMode then
        ctx.busIdle(2)

    override def disassemble(address: Int): DisassembledInstruction =
      val eaOp = ctx.getEA(mode,reg,Size.Byte,Some(address))
      DisassembledInstruction(address, opcode, instructionType.mnemonic,eaOp.getExtensionWords,Some(eaOp.getMnemonic(address)))

class NBCD(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     NBCD <ea>
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 1 | 0 | 0 | 1 | 0 | 0 | 0 | 0 | 0 |   Mode    |    Reg    |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     Size=(Byte)

     Condition codes:
     X — Set the same as the carry bit.
     N — Undefined.
     Z — Cleared if the result is nonzero; unchanged otherwise.
     V — Undefined.
     C — Set if a borrow (decimal) is generated; cleared otherwise.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    import RegisterType.*
    val code = genOpcode("0100100000______")
      for (mode <- Seq(0,2,3,4,5,6,7))
        val regEnd = if mode == 7 then 1 else 7
        for (reg <- 0 to regEnd)
          val opcode = code | mode << 3 | reg
          instructionSetHandler.registerInstruction(opcode, new NBCD.NBCD(ctx, opcode))
