package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object LEA:
  class LEA(override protected val ctx: M6800X0.Context,
            override protected val opcode:Int) extends Instruction(ctx,opcode):
    import RegisterType.*

    override val instructionType : InstructionType = InstructionType.LEA
    private final val register = (opcode >> 9) & 7

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
            LEA       |      INSTR      |                  INSTR
    ------------------+-----------------+------------------------------------------
    <ea>,An :         |                 |
      .L :            |                 |
        (An)          |  4(1/0)         |                               np
        (d16,An)      |  8(2/0)         |                          np   np
        (d8,An,Xn)    | 12(2/0)         |                        n np n np
        (xxx).W       |  8(2/0)         |                          np   np
        (xxx).L       | 12(3/0)         |                       np np   np
    */
    final override def execute(): Unit =
      val eaOp = ctx.getEA(mode,reg,Size.Long,None)
      if mode == 6 || (mode == 7 && reg == 3) then ctx.busIdle(2)
      val rega = ctx.getRegister(RegisterType.Address,register)
      rega.set(eaOp.getAddress,Size.Long)
      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================

    override def disassemble(address: Int): DisassembledInstruction =
      val eaOp = ctx.getEA(mode,reg,Size.Long,Some(address))
      DisassembledInstruction(address,opcode,instructionType.mnemonic,eaOp.getExtensionWords,Some(eaOp.getMnemonic(address)),Some(s"a$register"))

class LEA(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     LEA < ea >, An
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 1 | 0 | 0 |  Register | 1 | 1 | 1 |   Mode    |   Reg     |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

    Size = (Long)

    Condition Codes:
      Not affected.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    val code = genOpcode("0100___111______")
    for(register <- 0 to 7)
      for(mode <- Seq(2,5,6,7))
        val regEnd = if mode == 7 then 3 else 7
        for(reg <- 0 to regEnd)
          val opcode = code | register << 9 | mode << 3 | reg
          instructionSetHandler.registerInstruction(opcode,new LEA.LEA(ctx,opcode))
