package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object MOVEFromSR:
  class MOVEFromSR(override protected val ctx: M6800X0.Context,
                   override protected val opcode:Int) extends Instruction(ctx,opcode):
    import RegisterType.*

    override val instructionType : InstructionType = InstructionType.MOVEFromSR
    private final val destReg = (opcode >> 9) & 7

    /*
    -------------------------------------------------------------------------------
           MOVE       |    Exec Time    |               Data Bus Usage
         from SR      |  INSTR     EA   |  2nd Op (ea)  |          INSTR
    ------------------+-----------------+---------------+--------------------------
    SR,<ea> :         |                 |               |
      .W :            |                 |               |
        Dn            |  6(1/0)  0(0/0) |               |               np       n
        (An)          |  8(1/1)  4(1/0) |            nr |               np nw
        (An)+         |  8(1/1)  4(1/0) |            nr |               np nw
        -(An)         |  8(1/1)  6(1/0) | n          nr |               np nw
        (d16,An)      |  8(1/1)  8(2/0) |      np    nr |               np nw
        (d8,An,Xn)    |  8(1/1) 10(2/0) | n    np    nr |               np nw
        (xxx).W       |  8(1/1)  8(2/0) |      np    nr |               np nw
        (xxx).L       |  8(1/1) 12(3/0) |   np np    nr |               np nw
    */
    final override def execute(): Unit =
      val eaOp = ctx.getEA(mode,reg,Size.Word,None)
      // Memory destination is read before it is written to.
      eaOp.get(Size.Word)
      eaOp.set(flags.get(Size.Word) & 0xA71F,Size.Word)

      if eaOp.isRegisterMode then
        ctx.busIdle(2)

      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================

    override def disassemble(address: Int): DisassembledInstruction =
      val eaOp = ctx.getEA(mode,reg,Size.Word,Some(address))

      DisassembledInstruction(address,opcode,instructionType.mnemonic,eaOp.getExtensionWords,Some("SR"),Some(eaOp.getMnemonic(address)))

class MOVEFromSR(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     MOVE SR, < ea >
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 1 | 0 | 0 | 0 | 0 | 0 | 0 | 1 | 1 |   mode    |   reg     |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     Size = (Word)

     Condition codes:
      Not affected.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    import Size.*
    val code = genOpcode("0100000011______")
    for(srcMode <- Seq(0,2,3,4,5,6,7))
      val regEnd = if srcMode == 7 then 1 else 7
      for(srcReg <- 0 to regEnd)
        val opcode = code | srcMode << 3 | srcReg
        instructionSetHandler.registerInstruction(opcode,new MOVEFromSR.MOVEFromSR(ctx,opcode))
