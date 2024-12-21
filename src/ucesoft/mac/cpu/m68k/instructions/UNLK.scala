package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*
import ucesoft.mac.cpu.m68k.M6800X0.BusAccessMode

/**
 * Last operation on 26/6/2023.
 */
object UNLK:
  class UNLK(override protected val ctx: M6800X0.Context,
            override protected val opcode:Int) extends Instruction(ctx,opcode):
    import RegisterType.*

    override val instructionType : InstructionType = InstructionType.UNLK

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
          UNLNK       |      INSTR      |                  INSTR
    ------------------+-----------------+------------------------------------------
    An :              | 12(3/0)         |                         nU nu np
    */
    final override def execute(): Unit =
      val an = ctx.getRegister(RegisterType.Address,reg)
      val sp = ctx.getRegister(RegisterType.SP)
      val address = an.get(Size.Long)
      sp.set(address,Size.Long)
      an.set(ctx.readMemory(address,Size.Long,BusAccessMode.Pop),Size.Long)
      if reg != 7 then
        sp.increment(Size.Long)
      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================

    override def disassemble(address: Int): DisassembledInstruction =
      DisassembledInstruction(address,opcode,instructionType.mnemonic,Nil,Some(s"a$reg"))

class UNLK(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     UNLK An
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 1 | 0 | 0 | 1 | 1 | 1 | 0 | 0 | 1 | 0 | 1 | 1 |    Reg    |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     Condition codes:
      Not affected.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    val code = genOpcode("0100111001011___")
    for(reg <- 0 to 7)
      val opcode = code | reg
      instructionSetHandler.registerInstruction(opcode,new UNLK.UNLK(ctx,opcode))
