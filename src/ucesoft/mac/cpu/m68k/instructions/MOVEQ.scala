package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object MOVEQ:
  class MOVEQ(override protected val ctx: M6800X0.Context,
              override protected val opcode:Int) extends Instruction(ctx,opcode):
    import RegisterType.*

    override val instructionType : InstructionType = InstructionType.MOVEQ
    private final val data = opcode & 0xFF
    private final val register = (opcode >> 9) & 7

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
           MOVEQ      |      INSTR      |  1st OP (ea)  |          INSTR
    ------------------+-----------------+---------------+--------------------------
    #<data>,Dn :      |                 |
      .L :            |  4(1/0)         |                               np
    */
    final override def execute(): Unit =
      import StatusRegister.StatusFlag.*
      ctx.getRegister(RegisterType.Data,register).set(extendSign(Size.Byte,data),Size.Long)
      var ccr = flags.getCCR
      if (data & 0x80) != 0 then ccr |= N.flag else ccr &= ~N.flag
      if data == 0 then ccr |= Z.flag else ccr &= ~Z.flag
      ccr &= ~(V.flag | C.flag)
      flags.setCCR(ccr)
      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================

    override def disassemble(address: Int): DisassembledInstruction =
      DisassembledInstruction(address,opcode,instructionType.mnemonic,Nil,Some(s"#${data.toHexString}"),Some(s"d$register"))

class MOVEQ(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     MOVEQ # < data > ,Dn
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 1 | 1 | 1 |  Register | 0 |          Data                 |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     Size = (Long)
     Condition Codes:
      X — Not affected.
      N — Set if the result is negative; cleared otherwise.
      Z — Set if the result is zero; cleared otherwise.
      V — Always cleared.
      C — Always cleared.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    val code = genOpcode("0111___0________")
    for(reg <- 0 to 7)
      for(data <- 0 to 255)
        val opcode = code | reg << 9 | data
        instructionSetHandler.registerInstruction(opcode,new MOVEQ.MOVEQ(ctx,opcode))
