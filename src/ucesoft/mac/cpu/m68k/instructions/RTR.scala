package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*
import ucesoft.mac.cpu.m68k.M6800X0.BusAccessMode

object RTR:
  class RTR(override protected val ctx: M6800X0.Context,
            override protected val opcode:Int) extends Instruction(ctx,opcode):
    import RegisterType.*

    override val instructionType : InstructionType = InstructionType.RTR

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
         RTE, RTR     |      INSTR      |                  INSTR
    ------------------+-----------------+------------------------------------------
                      | 20(5/0)         |                      nU nu nu np np
    */
    final override def execute(): Unit =
      val sp = ctx.getRegister(RegisterType.SP)
      val ccr = ctx.readMemory(sp.get(Size.Long),Size.Word,BusAccessMode.Pop) & 0x1F
      sp.increment(Size.Word)
      val restoredPC = ctx.readMemory(sp.get(Size.Long),Size.Long,BusAccessMode.Pop)
      flags.setCCR(ccr)
      sp.increment(Size.Long)
      ctx.branch(restoredPC)
      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================

    override def disassemble(address: Int): DisassembledInstruction =
      DisassembledInstruction(address,opcode,instructionType.mnemonic)

class RTR(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 1 | 0 | 0 | 1 | 1 | 1 | 0 | 0 | 1 | 1 | 1 | 0 | 1 | 1 | 1 |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    val opcode = genOpcode("0100111001110111")
    instructionSetHandler.registerInstruction(opcode,new RTR.RTR(ctx,opcode))
