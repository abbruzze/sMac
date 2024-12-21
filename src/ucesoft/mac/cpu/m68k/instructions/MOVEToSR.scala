package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object MOVEToSR:
  class MOVEToSR(override protected val ctx: M6800X0.Context,
                 override protected val opcode:Int) extends Instruction(ctx,opcode):
    import RegisterType.*

    override val instructionType : InstructionType = InstructionType.MOVEToSR

    /*
    -------------------------------------------------------------------------------
           MOVE       |    Exec Time    |               Data Bus Usage
      to CCR, to SR   |  INSTR     EA   |  1st OP (ea)  |          INSTR
    ------------------+-----------------+---------------+--------------------------
    <ea>,CCR :        |                 |               |
      .W :            |                 |               |
        Dn            | 12(1/0)  0(0/0) |               |         nn np np
        (An)          | 12(1/0)  4(1/0) |            nr |         nn np np
        (An)+         | 12(1/0)  4(1/0) |            nr |         nn np np
        -(An)         | 12(1/0)  6(1/0) | n          nr |         nn np np
        (d16,An)      | 12(1/0)  8(2/0) |      np    nr |         nn np np
        (d8,An,Xn)    | 12(1/0) 10(2/0) | n    np    nr |         nn np np
        (xxx).W       | 12(1/0)  8(2/0) |      np    nr |         nn np np
        (xxx).L       | 12(1/0) 12(3/0) |   np np    nr |         nn np np
        #<data>       | 12(1/0)  4(1/0) |      np       |         nn np np
    */
    final override def execute(): Unit =
      val eaOp = ctx.getEA(mode,reg,Size.Word,None)
      val source = eaOp.get(Size.Word) & 0xA71F // All implemented bits of the status register are affected.

      ctx.busIdle(4)

      flags.set(source,Size.Word)

      // =============== prefetch==================
      ctx.fetchWord(false,clearPrefetchQueue = true)
      // ==========================================

    override def disassemble(address: Int): DisassembledInstruction =
      val eaOp = ctx.getEA(mode,reg,Size.Word,Some(address))
      DisassembledInstruction(address,opcode,instructionType.mnemonic,eaOp.getExtensionWords,Some(eaOp.getMnemonic(address)),Some("SR"))

class MOVEToSR(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     MOVE < ea >, SR
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 1 | 0 | 0 | 0 | 1 | 1 | 0 | 1 | 1 |   mode    |   reg     |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     Size = (Word)

     Condition codes:
      Set according to the source operand.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    import Size.*
    val code = genOpcode("0100011011______")
    for(srcMode <- Seq(0,2,3,4,5,6,7))
      val regEnd = if srcMode == 7 then 4 else 7
      for(srcReg <- 0 to regEnd)
        val opcode = code | srcMode << 3 | srcReg
        instructionSetHandler.registerInstruction(opcode,new MOVEToSR.MOVEToSR(ctx,opcode))
