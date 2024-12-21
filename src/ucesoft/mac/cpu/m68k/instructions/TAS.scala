package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object TAS:
  class TAS(override protected val ctx: M6800X0.Context,
            override protected val opcode:Int) extends Instruction(ctx,opcode):
    import RegisterType.*

    override val instructionType : InstructionType = InstructionType.TAS

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
           TAS        |  INSTR     EA   |  1st OP (ea)  |          INSTR
    ------------------+-----------------+---------------+--------------------------
    <ea> :            |                 |               |
      .B :            |                 |               |
        Dn            |  4(1/0)  0(0/0) |               |               np
        (An)          | 10(1/1)  4(1/0) |            nr |          n nw np
        (An)+         | 10(1/1)  4(1/0) |            nr |          n nw np
        -(An)         | 10(1/1)  6(1/0) | n          nr |          n nw np
        (d16,An)      | 10(1/1)  8(2/0) |      np    nr |          n nw np
        (d8,An,Xn)    | 10(1/1) 10(2/0) | n    np    nr |          n nw np
        (xxx).W       | 10(1/1)  8(1/0) |               |          n nw np
        (xxx).L       | 10(1/1) 12(2/0) |               |          n nw np
    */
    final override def execute(): Unit =
      import ucesoft.mac.cpu.m68k.StatusRegister.StatusFlag.*
      val eaOp = ctx.getEA(mode,reg,Size.Byte)
      val b = eaOp.get(Size.Byte)
      var ccr = flags.getCCR
      if (b & 0x80) > 0 then ccr |= N.flag else ccr &= ~N.flag
      if b == 0 then ccr |= Z.flag else ccr &= ~Z.flag
      ccr &= ~(C.flag | V.flag)
      flags.setCCR(ccr)
      if !eaOp.isRegisterMode then
        ctx.busIdle(2)
      eaOp.set(b | 0x80,Size.Byte)
      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================

    override def disassemble(address: Int): DisassembledInstruction =
      val eaOp = ctx.getEA(mode,reg,Size.Byte,Some(address))
      DisassembledInstruction(address,opcode,instructionType.mnemonic,eaOp.getExtensionWords,Some(eaOp.getMnemonic(address)))

class TAS(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     TAS < ea >
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 1 | 0 | 0 | 1 | 0 | 1 | 0 | 1 | 1 |   Mode    |    Reg    |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     Size = (Byte)
     Condition codes:
      X — Not affected.
      N — Set if the most significant bit of the operand is currently set; cleared otherwise.
      Z — Set if the operand was zero; cleared otherwise.
      V — Always cleared.
      C — Always cleared.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    val code = genOpcode("0100101011______")
    for(mode <- Seq(0,2,3,4,5,6,7))
      val regEnd = if mode == 7 then 1 else 7
      for(reg <- 0 to regEnd)
        val opcode = code | mode << 3 | reg
        instructionSetHandler.registerInstruction(opcode,new TAS.TAS(ctx,opcode))
