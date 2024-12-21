package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object MOVE:
  class MOVE(override protected val ctx: M6800X0.Context,
             override protected val opcode:Int,
             size:Size) extends Instruction(ctx,opcode):
    import RegisterType.*

    override val instructionType : InstructionType = InstructionType.MOVE
    private final val destReg = (opcode >> 9) & 7
    private final val destMode = (opcode >> 6) & 7
    private final val addIdleCycles = destMode == 6
    private final val isDstPreIndirect = destMode == 4

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
           MOVE       |      INSTR      |  1st OP (ea)  |          INSTR
    ------------------+-----------------+---------------+--------------------------
    <ea>,Dn :         |                 |               |
      .B or .W :      |                 |               |
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
    <ea>,(An) :       |                 |               |
      .B or .W :      |                 |               |
        Dn            |  8(1/1)         |               |            nw np
        An            |  8(1/1)         |               |            nw np
        (An)          | 12(2/1)         |            nr |            nw np
        (An)+         | 12(2/1)         |            nr |            nw np
        -(An)         | 14(2/1)         | n          nr |            nw np
        (d16,An)      | 16(3/1)         |      np    nr |            nw np
        (d8,An,Xn)    | 18(3/1)         | n    np    nr |            nw np
        (xxx).W       | 16(3/1)         |      np    nr |            nw np
        (xxx).L       | 20(4/1)         |   np np    nr |            nw np
        #<data>       | 12(2/1)         |      np       |            nw np
      .L :            |                 |               |
        Dn            | 12(1/2)         |               |         nW nw np
        An            | 12(1/2)         |               |         nW nw np
        (An)          | 20(3/2)         |         nR nr |         nW nw np
        (An)+         | 20(3/2)         |         nR nr |         nW nw np
        -(An)         | 22(3/2)         | n       nR nr |         nW nw np
        (d16,An)      | 24(4/2)         |      np nR nr |         nW nw np
        (d8,An,Xn)    | 26(4/2)         | n    np nR nr |         nW nw np
        (xxx).W       | 24(4/2)         |      np nR nr |         nW nw np
        (xxx).L       | 28(5/2)         |   np np nR nr |         nW nw np
        #<data>       | 20(3/2)         |   np np       |         nW nw np
    <ea>,(An)+ :      |                 |               |
      .B or .W :      |                 |               |
        Dn            |  8(1/1)         |               |            nw np
        An            |  8(1/1)         |               |            nw np
        (An)          | 12(2/1)         |            nr |            nw np
        (An)+         | 12(2/1)         |            nr |            nw np
        -(An)         | 14(2/1)         | n          nr |            nw np
        (d16,An)      | 16(3/1)         |      np    nr |            nw np
        (d8,An,Xn)    | 18(3/1)         | n    np    nr |            nw np
        (xxx).W       | 16(3/1)         |      np    nr |            nw np
        (xxx).L       | 20(4/1)         |   np np    nr |            nw np
        #<data>       | 12(2/1)         |      np       |            nw np
      .L :            |                 |               |
        Dn            | 12(1/2)         |               |         nW nw np
        An            | 12(1/2)         |               |         nW nw np
        (An)          | 20(3/2)         |         nR nr |         nW nw np
        (An)+         | 20(3/2)         |         nR nr |         nW nw np
        -(An)         | 22(3/2)         | n       nR nr |         nW nw np
        (d16,An)      | 24(4/2)         |      np nR nr |         nW nw np
        (d8,An,Xn)    | 26(4/2)         | n    np nR nr |         nW nw np
        (xxx).W       | 24(4/2)         |      np nR nr |         nW nw np
        (xxx).L       | 28(5/2)         |   np np nR nr |         nW nw np
        #<data>       | 20(3/2)         |   np np       |         nW nw np
    <ea>,-(An) :      |                 |               |
      .B or .W :      |                 |               |
        Dn            |  8(1/1)         |               |                  np nw
        An            |  8(1/1)         |               |                  np nw
        (An)          | 12(2/1)         |            nr |                  np nw
        (An)+         | 12(2/1)         |            nr |                  np nw
        -(An)         | 14(2/1)         | n          nr |                  np nw
        (d16,An)      | 16(3/1)         |      np    nr |                  np nw
        (d8,An,Xn)    | 18(3/1)         | n    np    nr |                  np nw
        (xxx).W       | 16(3/1)         |      np    nr |                  np nw
        (xxx).L       | 20(4/1)         |   np np    nr |                  np nw
        #<data>       | 12(2/1)         |      np       |                  np nw
      .L :            |                 |               |
        Dn            | 12(1/2)         |               |                  np nw nW
        An            | 12(1/2)         |               |                  np nw nW
        (An)          | 20(3/2)         |         nR nr |                  np nw nW
        (An)+         | 20(3/2)         |         nR nr |                  np nw nW
        -(An)         | 22(3/2)         | n       nR nr |                  np nw nW
        (d16,An)      | 24(4/2)         |      np nR nr |                  np nw nW
        (d8,An,Xn)    | 26(4/2)         | n    np nR nr |                  np nw nW
        (xxx).W       | 24(4/2)         |      np nR nr |                  np nw nW
        (xxx).L       | 28(5/2)         |   np np nR nr |                  np nw nW
        #<data>       | 20(3/2)         |   np np       |                  np nw nW
    <ea>,(d16,An) :   |                 |               |
      .B or .W :      |                 |               |
        Dn            | 12(2/1)         |               |      np    nw np
        An            | 12(2/1)         |               |      np    nw np
        (An)          | 16(3/1)         |            nr |      np    nw np
        (An)+         | 16(3/1)         |            nr |      np    nw np
        -(An)         | 18(3/1)         | n          nr |      np    nw np
        (d16,An)      | 20(4/1)         |      np    nr |      np    nw np
        (d8,An,Xn)    | 22(4/1)         | n    np    nr |      np    nw np
        (xxx).W       | 20(4/1)         |      np    nr |      np    nw np
        (xxx).L       | 24(5/1)         |   np np    nr |      np    nw np
        #<data>       | 16(3/1)         |      np       |      np    nw np
      .L :            |                 |               |
        Dn            | 16(2/2)         |               |      np nW nw np
        An            | 16(2/2)         |               |      np nW nw np
        (An)          | 24(4/2)         |         nR nr |      np nW nw np
        (An)+         | 24(4/2)         |         nR nr |      np nW nw np
        -(An)         | 26(4/2)         | n       nR nr |      np nW nw np
        (d16,An)      | 28(5/2)         |      np nR nr |      np nW nw np
        (d8,An,Xn)    | 30(5/2)         | n    np nR nr |      np nW nw np
        (xxx).W       | 28(5/2)         |      np nR nr |      np nW nw np
        (xxx).L       | 32(6/2)         |   np np nR nr |      np nW nw np
        #<data>       | 24(4/2)         |   np np       |      np nW nw np
    <ea>,(d8,An,Xn) : |                 |               |
      .B or .W :      |                 |               |
        Dn            | 14(2/1)         |               | n    np    nw np
        An            | 14(2/1)         |               | n    np    nw np
        (An)          | 18(3/1)         |            nr | n    np    nw np
        (An)+         | 18(3/1)         |            nr | n    np    nw np
        -(An)         | 20(3/1)         | n          nr | n    np    nw np
        (d16,An)      | 22(4/1)         |      np    nr | n    np    nw np
        (d8,An,Xn)    | 24(4/1)         | n    np    nr | n    np    nw np
        (xxx).W       | 22(4/1)         |      np    nr | n    np    nw np
        (xxx).L       | 26(5/1)         |   np np    nr | n    np    nw np
        #<data>       | 18(3/1)         |      np       | n    np    nw np
      .L :            |                 |               |
        Dn            | 18(2/2)         |               | n    np nW nw np
        An            | 18(2/2)         |               | n    np nW nw np
        (An)          | 26(4/2)         |         nR nr | n    np nW nw np
        (An)+         | 26(4/2)         |         nR nr | n    np nW nw np
        -(An)         | 28(4/2)         | n       nR nr | n    np nW nw np
        (d16,An)      | 30(5/2)         |      np nR nr | n    np nW nw np
        (d8,An,Xn)    | 32(5/2)         | n    np nR nr | n    np nW nw np
        (xxx).W       | 30(5/2)         |      np nR nr | n    np nW nw np
        (xxx).L       | 34(6/2)         |   np np nR nr | n    np nW nw np
        #<data>       | 26(4/2)         |   np np       | n    np nW nw np
    <ea>,(xxx).W :    |                 |               |
      .B or .W :      |                 |               |
        Dn            | 12(2/1)         |               |      np    nw np
        An            | 12(2/1)         |               |      np    nw np
        (An)          | 16(3/1)         |            nr |      np    nw np
        (An)+         | 16(3/1)         |            nr |      np    nw np
        -(An)         | 18(3/1)         | n          nr |      np    nw np
        (d16,An)      | 20(4/1)         |      np    nr |      np    nw np
        (d8,An,Xn)    | 22(4/1)         | n    np    nr |      np    nw np
        (xxx).W       | 20(4/1)         |      np    nr |      np    nw np
        (xxx).L       | 24(5/1)         |   np np    nr |      np    nw np
        #<data>       | 16(3/1)         |      np       |      np    nw np
      .L :            |                 |               |
        Dn            | 16(2/2)         |               |      np nW nw np
        An            | 16(2/2)         |               |      np nW nw np
        (An)          | 24(4/2)         |         nR nr |      np nW nw np
        (An)+         | 24(4/2)         |         nR nr |      np nW nw np
        -(An)         | 26(4/2)         | n       nR nr |      np nW nw np
        (d16,An)      | 28(5/2)         |      np nR nr |      np nW nw np
        (d8,An,Xn)    | 30(5/2)         | n    np nR nr |      np nW nw np
        (xxx).W       | 28(5/2)         |      np nR nr |      np nW nw np
        (xxx).L       | 32(6/2)         |   np np nR nr |      np nW nw np
        #<data>       | 24(4/2)         |   np np       |      np nW nw np
    <ea>,(xxx).L :    |                 |               |
      .B or .W :      |                 |               |
        Dn            | 16(3/1)         |               |   np np    nw np
        An            | 16(3/1)         |               |   np np    nw np
        (An)          | 20(4/1)         |            nr |      np    nw np np
        (An)+         | 20(4/1)         |            nr |      np    nw np np
        -(An)         | 22(4/1)         | n          nr |      np    nw np np
        (d16,An)      | 24(5/1)         |      np    nr |      np    nw np np
        (d8,An,Xn)    | 26(5/1)         | n    np    nr |      np    nw np np
        (xxx).W       | 24(5/1)         |      np    nr |      np    nw np np
        (xxx).L       | 28(6/1)         |   np np    nr |      np    nw np np
        #<data>       | 20(4/1)         |      np       |   np np    nw np
      .L :            |                 |               |
        Dn            | 20(3/2)         |               |   np np nW nw np
        An            | 20(3/2)         |               |   np np nW nw np
        (An)          | 28(5/2)         |         nR nr |      np nW nw np np
        (An)+         | 28(5/2)         |         nR nr |      np nW nw np np
        -(An)         | 30(5/2)         | n       nR nr |      np nW nw np np
        (d16,An)      | 32(6/2)         |      np nR nr |      np nW nw np np
        (d8,An,Xn)    | 34(6/2)         | n    np nR nr |      np nW nw np np
        (xxx).W       | 32(6/2)         |      np nR nr |      np nW nw np np
        (xxx).L       | 36(7/2)         |   np np nR nr |      np nW nw np np
        #<data>       | 28(5/2)         |   np np       |   np np nW nw np
    */
    final override def execute(): Unit =
      import StatusRegister.StatusFlag.*
      var ccr = flags.getCCR

      val srcEaOp = ctx.getEA(mode,reg,size,None)
      val source = srcEaOp.get(size)

      if (source & size.msb) != 0 then ccr |= N.flag else ccr &= ~N.flag
      if (source & size.mask) == 0 then ccr |= Z.flag else ccr &= ~Z.flag
      ccr &= ~(V.flag | C.flag)
      flags.setCCR(ccr)

      val dstEaOp = ctx.getEA(destMode,destReg,size,None,includeIdleBusCycles = false)

      if addIdleCycles then
        ctx.busIdle(2)

      if isDstPreIndirect then
        // =============== prefetch==================
        ctx.fetchWord(false)
        // ==========================================
        dstEaOp.set(source,size)
      else
        dstEaOp.set(source,size)
        // =============== prefetch==================
        ctx.fetchWord(false)
        // ==========================================

    override def disassemble(address: Int): DisassembledInstruction =
      val srcEaOp = ctx.getEA(mode,reg,size,Some(address))
      // copy op properties because .getEA of destination modifies source one
      val srcExtWords = srcEaOp.getExtensionWords
      val srcMnemonic = srcEaOp.getMnemonic(address)
      val dstEaOp = ctx.getEA(destMode,destReg,size,Some(address + srcEaOp.getExtensionWords.size * 2))

      DisassembledInstruction(address,opcode,s"${instructionType.mnemonic}${size.ext}",srcExtWords ++ dstEaOp.getExtensionWords,Some(srcMnemonic),Some(dstEaOp.getMnemonic(address + srcExtWords.size * 2)))

class MOVE(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     MOVE < ea > , < ea >
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 0 |  Size | dst reg   |  dst mode | src mode  | src reg   |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     Size = (Byte, Word, Long)

     Condition codes:
      X — Not affected.
      N — Set if the result is negative; cleared otherwise.
      Z — Set if the result is zero; cleared otherwise.
      V — Always cleared.
      C — Always cleared.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    import Size.*
    val code = genOpcode("00______________")
    for(size <- Seq(Byte,Word,Long))
      for(destMode <- Seq(0,2,3,4,5,6,7))
        val regEnd = if destMode == 7 then 1 else 7
        for(destReg <- 0 to regEnd)
          for(srcMode <- 0 to 7)
            if !(srcMode == 1 && size == Byte) then
              val regEnd = if srcMode == 7 then 4 else 7
              for(srcReg <- 0 to regEnd)
                val sizeCode = size match
                  case Byte => 1
                  case Word => 3
                  case Long => 2
                val opcode = code | sizeCode << 12 | destReg << 9 | destMode << 6 | srcMode << 3 | srcReg
                instructionSetHandler.registerInstruction(opcode,new MOVE.MOVE(ctx,opcode,size))
