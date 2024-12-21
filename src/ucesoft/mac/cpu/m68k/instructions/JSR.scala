package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*
import ucesoft.mac.cpu.m68k.M6800X0.BusAccessMode

object JSR:
  class JSR(override protected val ctx: M6800X0.Context,
            override protected val opcode:Int) extends Instruction(ctx,opcode):
    import RegisterType.*

    override val isBranch = true
    override val instructionType : InstructionType = InstructionType.JSR
    private final val idleCycles = mode match
      case 5 /*(d16,An)*/   => 2
      case 6 /*(d8,An,Xn)*/ => 4
      case 7 =>
        reg match
          case 0 | 2 /*(xxx).W (d16,PC)*/ => 2
          case 3 /*(d8,PC,Xn)*/           => 4
          case _ => 0
      case _ => 0

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
           JSR        |      INSTR      |                  INSTR
    ------------------+-----------------+------------------------------------------
    <ea> :            |                 |
        (An)          | 16(2/2)         |                      np nS ns np
        (d16,An)      | 18(2/2)         |                 n    np nS ns np
        (d8,An,Xn)    | 22(2/2)         |                 n nn np nS ns np
        (xxx).W       | 18(2/2)         |                 n    np nS ns np
        (xxx).L       | 20(3/2)         |                   np np nS ns np
    */
    final override def execute(): Unit =
      if idleCycles > 0 then ctx.busIdle(idleCycles)

      val eaOp = ctx.getEA(mode,reg,Size.Long,None)
      val sp = ctx.getRegister(RegisterType.SP) // cannot be put as attribute in the class: depending on the supervisor bit it will be usp or sp
      val pc = ctx.getPC
      ctx.branch(eaOp.getAddress) // check address bus error before decrementing SP
      sp.decrement(Size.Long)
      ctx.writeMemory(sp.get(Size.Long),pc,Size.Long,BusAccessMode.Push)

      if mode != 2 /*(An)*/then ctx.adjustCycles(-4)
      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================

    override def disassemble(address: Int): DisassembledInstruction =
      val eaOp = ctx.getEA(mode,reg,Size.Long,Some(address))
      DisassembledInstruction(address,opcode,instructionType.mnemonic,eaOp.getExtensionWords,Some(eaOp.getMnemonic(address)))

class JSR(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     JSR < ea >
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 1 | 0 | 0 | 1 | 1 | 1 | 0 | 1 | 0 |   Mode    |   Reg     |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

    Condition Codes:
      Not affected.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    val code = genOpcode("0100111010______")
    for(mode <- Seq(2,5,6,7))
      val regEnd = if mode == 7 then 3 else 7
      for(reg <- 0 to regEnd)
        val opcode = code | mode << 3 | reg
        instructionSetHandler.registerInstruction(opcode,new JSR.JSR(ctx,opcode))
