package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object MOVEA:
  class MOVEA(override protected val ctx: M6800X0.Context,
              override protected val opcode:Int,
              size:Size) extends Instruction(ctx,opcode):
    import RegisterType.*

    override val instructionType : InstructionType = InstructionType.MOVEA
    private final val destReg = (opcode >> 9) & 7

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
          MOVEA       |      INSTR      |  1st OP (ea)  |          INSTR
    ------------------+-----------------+---------------+--------------------------
    <ea>,An :         |                 |               |
      .W :            |                 |               |
        Dn            |  4(1/0)         |               |               np
        An            |  4(1/0)         |               |               np
        (An)          |  8(2/0)         |            nr |               np
        (An)+         |  8(2/0)         |            nr |               np
        -(An)         | 10(2/0)         | n          nr |               np
        (d16,An)      | 12(3/0)         |      np    nr |               np
        (d8,An,Xn)    | 14(3/0)         | n    np    nr |               np
        (xxx).W       | 12(3/0)         |      np    nr |               np
        (xxx).L       | 16(4/0)         |   np np    nr |               np
        #<data>       |  8(2/0)         |      np       |               np
      .L :            |                 |               |
        Dn            |  4(1/0)         |               |               np
        An            |  4(1/0)         |               |               np
        (An)          | 12(3/0)         |         nR nr |               np
        (An)+         | 12(3/0)         |         nR nr |               np
        -(An)         | 14(3/0)         | n       nR nr |               np
        (d16,An)      | 16(4/0)         |      np nR nr |               np
        (d8,An,Xn)    | 18(4/0)         | n    np nR nr |               np
        (xxx).W       | 16(4/0)         |      np nR nr |               np
        (xxx).L       | 20(5/0)         |   np np nR nr |               np
        #<data>       | 12(3/0)         |   np np       |               np
    */
    final override def execute(): Unit =

      val srcEaOp = ctx.getEA(mode,reg,size,None)
      val source = srcEaOp.get(size,signExtended = true)

      val areg = ctx.getRegister(RegisterType.Address,destReg)
      areg.set(source,Size.Long)

      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================

    override def disassemble(address: Int): DisassembledInstruction =
      val srcEaOp = ctx.getEA(mode,reg,size,Some(address))

      DisassembledInstruction(address,opcode,s"${instructionType.mnemonic}${size.ext}",srcEaOp.getExtensionWords,Some(srcEaOp.getMnemonic(address)),Some(s"a$destReg"))

class MOVEA(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     MOVEA < ea > , An
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 0 |  Size | dst reg   | 0 | 0 | 1 | src mode  | src reg   |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     Size = (Word, Long)

     Condition codes:
      Not affected.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    import Size.*
    val code = genOpcode("00_____001______")
    for(size <- Seq(Word,Long))
      for(destReg <- 0 to 7)
        for(srcMode <- 0 to 7)
          val regEnd = if srcMode == 7 then 4 else 7
          for(srcReg <- 0 to regEnd)
            val sizeCode = (size : @unchecked) match
              case Word => 3
              case Long => 2
            val opcode = code | sizeCode << 12 | destReg << 9 | srcMode << 3 | srcReg
            instructionSetHandler.registerInstruction(opcode,new MOVEA.MOVEA(ctx,opcode,size))
