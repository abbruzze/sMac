package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object RESET:
  class RESET(override protected val ctx: M6800X0.Context,
              override protected val opcode:Int) extends Instruction(ctx,opcode):
    import RegisterType.*

    override val instructionType : InstructionType = InstructionType.RESET

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
           RESET      |      INSTR      |                  INSTR
    ------------------+-----------------+------------------------------------------
                      | 132(1/0)        |                      nn (??-)* np
    */
    final override def execute(): Unit =
      ctx.resetDevices()
      ctx.busIdle(132)
      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================

    override def disassemble(address: Int): DisassembledInstruction =
      DisassembledInstruction(address,opcode,instructionType.mnemonic)

class RESET(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 1 | 0 | 0 | 1 | 1 | 1 | 0 | 0 | 1 | 1 | 1 | 0 | 0 | 0 | 0 |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    val opcode = genOpcode("0100111001110000")
    instructionSetHandler.registerInstruction(opcode,new RESET.RESET(ctx,opcode))
