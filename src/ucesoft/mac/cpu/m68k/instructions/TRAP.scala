package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object TRAP:
  class TRAP(override protected val ctx: M6800X0.Context,
            override protected val opcode:Int) extends Instruction(ctx,opcode):
    import RegisterType.*

    override val instructionType : InstructionType = InstructionType.TRAP
    private final val vector = opcode & 0xF

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
      ILLEGAL, TRAP   |      INSTR      |                  INSTR
    ------------------+-----------------+------------------------------------------
                      | 34(4/3)         |             nn    ns ns nS nV nv np  n np
    */
    final override def execute(): Unit =
      ctx.busIdle(4)
      ctx.raiseException(32 + vector)
      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================

    override def disassemble(address: Int): DisassembledInstruction =
      DisassembledInstruction(address,opcode,instructionType.mnemonic,Nil,Some(s"# $vector"))

class TRAP(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     TRAP #vector
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 1 | 0 | 0 | 1 | 1 | 1 | 0 | 0 | 1 | 0 | 0 |   vector      |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    val code = genOpcode("010011100100____")
    for(vec <- 0 to 15)
      val opcode = code | vec
      instructionSetHandler.registerInstruction(opcode,new TRAP.TRAP(ctx,opcode))
