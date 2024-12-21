package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*
import ucesoft.mac.cpu.m68k.M6800X0.BusAccessMode

object RTS:
  class RTS(override protected val ctx: M6800X0.Context,
            override protected val opcode:Int) extends Instruction(ctx,opcode):
    import RegisterType.*

    override val instructionType : InstructionType = InstructionType.RTS

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
           RTS        |      INSTR      |                  INSTR
    ------------------+-----------------+------------------------------------------
                      | 16(4/0)         |                   nU nu    np np
    */
    final override def execute(): Unit =
      val sp = ctx.getRegister(RegisterType.SP)
      val restoredPC = ctx.readMemory(sp.get(Size.Long),Size.Long,BusAccessMode.Pop)
      sp.increment(Size.Long)
      ctx.branch(restoredPC)
      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================

    override def disassemble(address: Int): DisassembledInstruction =
      DisassembledInstruction(address,opcode,instructionType.mnemonic)

class RTS(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 1 | 0 | 0 | 1 | 1 | 1 | 0 | 0 | 1 | 1 | 1 | 0 | 1 | 0 | 1 |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    val opcode = genOpcode("0100111001110101")
    instructionSetHandler.registerInstruction(opcode,new RTS.RTS(ctx,opcode))
