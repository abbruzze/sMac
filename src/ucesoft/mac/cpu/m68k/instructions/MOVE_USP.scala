package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object MOVE_USP:
  class MOVE_USP(override protected val ctx: M6800X0.Context,
                 override protected val opcode:Int) extends Instruction(ctx,opcode):
    import RegisterType.*

    override val instructionType : InstructionType = InstructionType.MOVEUSP
    private final val USP2An = (opcode & 8) != 0

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
         MOVE USP     |      INSTR      |                  INSTR
    ------------------+-----------------+------------------------------------------
     An,USP :         |  4(1/0)         |                               np
     USP,An :         |  4(1/0)         |                               np
    */
    final override def execute(): Unit =
      val r = ctx.getRegister(RegisterType.Address,reg)
      val usp = ctx.getRegister(RegisterType.USP)

      if USP2An then
        r.set(usp.get(Size.Long),Size.Long)
      else
        usp.set(r.get(Size.Long),Size.Long)

      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================

    override def disassemble(address: Int): DisassembledInstruction =
      if USP2An then
        DisassembledInstruction(address,opcode,instructionType.mnemonic,Nil,Some("USP"),Some(s"a$reg"))
      else
        DisassembledInstruction(address,opcode,instructionType.mnemonic,Nil,Some(s"a$reg"),Some("USP"))

class MOVE_USP(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     MOVE USP,An
     MOVE An,USP
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 1 | 0 | 0 | 1 | 1 | 1 | 0 | 0 | 1 | 1 | 0 | dr|   reg     |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     Size = (Long)

     Condition codes:
      Set according to the source operand.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    import Size.*
    val code = genOpcode("010011100110____")
    for(dr <- 0 to 1)
      for(reg <- 0 to 7)
        val opcode = code | dr << 3 | reg
        instructionSetHandler.registerInstruction(opcode,new MOVE_USP.MOVE_USP(ctx,opcode))
