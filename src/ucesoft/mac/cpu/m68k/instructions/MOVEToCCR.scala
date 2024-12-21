package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object MOVEToCCR:
  class MOVEToCCR(override protected val ctx: M6800X0.Context,
                  override protected val opcode:Int) extends Instruction(ctx,opcode):
    import RegisterType.*

    override val instructionType : InstructionType = InstructionType.MOVEToCCR

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
      val source = eaOp.get(Size.Word) & 0x1F

      ctx.busIdle(4)

      flags.setCCR(source)

      // =============== prefetch==================
      ctx.fetchWord(false,clearPrefetchQueue = true)
      // ==========================================

    override def disassemble(address: Int): DisassembledInstruction =
      val eaOp = ctx.getEA(mode,reg,Size.Byte,Some(address))

      DisassembledInstruction(address,opcode,instructionType.mnemonic,eaOp.getExtensionWords,Some(eaOp.getMnemonic(address)),Some("CCR"))

class MOVEToCCR(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     MOVE < ea >, CCR
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 1 | 0 | 0 | 0 | 1 | 0 | 0 | 1 | 1 |   mode    |   reg     |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     Size = (Word)

     Condition codes:
      X — Set to the value of bit 4 of the source operand.
      N — Set to the value of bit 3 of the source operand.
      Z — Set to the value of bit 2 of the source operand.
      V — Set to the value of bit 1 of the source operand.
      C — Set to the value of bit 0 of the source operand.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    import Size.*
    val code = genOpcode("0100010011______")
    for(srcMode <- Seq(0,2,3,4,5,6,7))
      val regEnd = if srcMode == 7 then 4 else 7
      for(srcReg <- 0 to regEnd)
        val opcode = code | srcMode << 3 | srcReg
        instructionSetHandler.registerInstruction(opcode,new MOVEToCCR.MOVEToCCR(ctx,opcode))
