package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*
import ucesoft.mac.cpu.m68k.M6800X0.BusAccessMode

object LINK:
  class LINK(override protected val ctx: M6800X0.Context,
             override protected val opcode:Int) extends Instruction(ctx,opcode):
    import RegisterType.*

    override val instructionType : InstructionType = InstructionType.LINK

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
           LINK       |      INSTR      |  2nd Operand  |   INSTR
    ------------------+-----------------+---------------+--------------------------
    An,#<data> :      |                 |
      .W :            | 16(2/2)         |                      np nS ns np
    */
    final override def execute(): Unit =
      val immOp = ctx.getEA(AddressingMode.IM, 0, Size.Word, None)
      val displacement = immOp.get(Size.Word, signExtended = true)

      val sp = ctx.getRegister(RegisterType.SP) // cannot be put as attribute in the class: depending on the supervisor bit it will be usp or sp
      sp.decrement(Size.Long)
      val r = ctx.getRegister(RegisterType.Address,reg)
      val spValue = sp.get(Size.Long)
      ctx.writeMemory(spValue,r.get(Size.Long),Size.Long,BusAccessMode.Push)
      r.set(spValue,Size.Long)
      sp.set(spValue + displacement,Size.Long)
      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================

    override def disassemble(address: Int): DisassembledInstruction =
      val immOp = ctx.getEA(AddressingMode.IM, 0, Size.Word, Some(address))
      val displacement = immOp.get(Size.Word, signExtended = true)

      DisassembledInstruction(address,opcode,instructionType.mnemonic,immOp.getExtensionWords,Some(s"a$reg"),Some(displacement.toHexString))

class LINK(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     LINK An, # < displacement >
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 1 | 0 | 0 | 1 | 1 | 1 | 0 | 0 | 1 | 0 | 1 | 0 | Register  |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     Size = (Word, Long*)
     *MC68020, MC68030, MC68040 and CPU32 only
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    val code = genOpcode("0100111001010___")
    for(register <- 0 to 7)
      val opcode = code | register
      instructionSetHandler.registerInstruction(opcode,new LINK.LINK(ctx,opcode))
