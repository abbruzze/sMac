package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object JMP:
  class JMP(override protected val ctx: M6800X0.Context,
            override protected val opcode:Int) extends Instruction(ctx,opcode):
    import RegisterType.*

    override val isBranch = true
    override val instructionType : InstructionType = InstructionType.JMP
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
           JMP        |      INSTR      |                  INSTR
    ------------------+-----------------+------------------------------------------
    <ea> :            |                 |
        (An)          |  8(2/0)         |                      np       np
        (d16,An)      | 10(2/0)         |                 n    np       np
        (d8,An,Xn)    | 14(2/0)         |                 n nn np       np
        (xxx).W       | 10(2/0)         |                 n    np       np
        (xxx).L       | 12(3/0)         |                   np np       np
    */
    final override def execute(): Unit =
      if idleCycles > 0 then ctx.busIdle(idleCycles)

      val eaOp = ctx.getEA(mode,reg,Size.Long,None)
      ctx.branch(eaOp.getAddress)
      if mode != 2 /*(An)*/then ctx.adjustCycles(-4)
      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================

    override def disassemble(address: Int): DisassembledInstruction =
      val eaOp = ctx.getEA(mode,reg,Size.Long,Some(address))
      DisassembledInstruction(address,opcode,instructionType.mnemonic,eaOp.getExtensionWords,Some(eaOp.getMnemonic(address)))

class JMP(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     JMP < ea >
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 1 | 0 | 0 | 1 | 1 | 1 | 0 | 1 | 1 |   Mode    |   Reg     |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

    Condition Codes:
      Not affected.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    val code = genOpcode("0100111011______")
    for(mode <- Seq(2,5,6,7))
      val regEnd = if mode == 7 then 3 else 7
      for(reg <- 0 to regEnd)
        val opcode = code | mode << 3 | reg
        instructionSetHandler.registerInstruction(opcode,new JMP.JMP(ctx,opcode))
