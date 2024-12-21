package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object STOP:
  class STOP(override protected val ctx: M6800X0.Context,
             override protected val opcode:Int) extends Instruction(ctx,opcode):
    override val instructionType : InstructionType = InstructionType.STOP

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
           STOP       |      INSTR      |                  INSTR
    ------------------+-----------------+------------------------------------------
                      |  4(0/0)         |                               n
    */
    final override def execute(): Unit =
      val data = ctx.fetchWord()
      flags.set(data, Size.Word)
      ctx.stopCPU()
      ctx.busIdle(4)

    override def disassemble(address: Int): DisassembledInstruction =
      val immOp = ctx.getEA(AddressingMode.IM, 0, Size.Word, Some(address))
      DisassembledInstruction(address,opcode,instructionType.mnemonic,immOp.getExtensionWords,Some(immOp.getMnemonic(address)))

class STOP(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 1 | 0 | 0 | 1 | 1 | 1 | 0 | 0 | 1 | 1 | 1 | 0 | 0 | 1 | 0 |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     |              Immediate Data                                   |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    val opcode = genOpcode("0100111001110010")
    instructionSetHandler.registerInstruction(opcode,new STOP.STOP(ctx,opcode))
