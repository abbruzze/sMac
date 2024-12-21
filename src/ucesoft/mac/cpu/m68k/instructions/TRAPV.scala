package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object TRAPV:
  class TRAPV(override protected val ctx: M6800X0.Context,
            override protected val opcode:Int) extends Instruction(ctx,opcode):
    import RegisterType.*

    override val instructionType : InstructionType = InstructionType.TRAPV
    private final val vector = opcode & 0xF

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
          TRAPV       |      INSTR      |                  INSTR
    ------------------+-----------------+------------------------------------------
    no trap           |  4(1/0)         |    np
       trap           | 34(5/3)         |    np             ns nS ns nV nv np  n np
    */
    final override def execute(): Unit =
      if flags.isFlag(StatusRegister.StatusFlag.V) then
        ctx.busIdle(4)
        ctx.raiseException(7)
      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================

    override def disassemble(address: Int): DisassembledInstruction =
      DisassembledInstruction(address,opcode,instructionType.mnemonic)

class TRAPV(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     TRAPV
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 1 | 0 | 0 | 1 | 1 | 1 | 0 | 0 | 1 | 1 | 1 | 0 | 1 | 1 | 0 |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    val opcode = genOpcode("0100111001110110")
    instructionSetHandler.registerInstruction(opcode,new TRAPV.TRAPV(ctx,opcode))
