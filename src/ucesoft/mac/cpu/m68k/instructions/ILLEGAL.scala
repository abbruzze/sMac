package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object ILLEGAL:
  class ILLEGAL(override protected val ctx: M6800X0.Context,
                override protected val opcode:Int) extends Instruction(ctx,opcode):
    import RegisterType.*

    override val instructionType : InstructionType = InstructionType.ILLEGAL

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
      ILLEGAL, TRAP   |      INSTR      |                  INSTR
    ------------------+-----------------+------------------------------------------
                      | 34(4/3)         |             nn    ns ns nS nV nv np  n np
    */
    final override def execute(): Unit =
      ctx.busIdle(4)
      ctx.raiseException(4)
      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================

    override def disassemble(address: Int): DisassembledInstruction =
      DisassembledInstruction(address,opcode,instructionType.mnemonic)

class ILLEGAL(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 1 | 0 | 0 | 1 | 0 | 1 | 0 | 1 | 1 | 1 | 1 | 1 | 1 | 0 | 0 |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    val opcode = genOpcode("0100101011111100")
    instructionSetHandler.registerInstruction(opcode,new ILLEGAL.ILLEGAL(ctx,opcode))
