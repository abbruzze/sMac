package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object ADDA:
  class ADDA(override protected val ctx: M6800X0.Context,
             override protected val opcode:Int,
             size:Size) extends Instruction(ctx,opcode):
    import RegisterType.*

    override val instructionType : InstructionType = InstructionType.ADDA
    protected final val register = (opcode >> 9) & 7
    /*
      -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
        ADDA, SUBA    |  INSTR     EA   |  1st OP (ea)  |          INSTR
      ------------------+-----------------+---------------+--------------------------
      <ea>,An :         |                 |               |
        .B or .W :      |                 |               |
          Dn            |  8(1/0)  0(0/0) |               |               np       nn
          An            |  8(1/0)  0(0/0) |               |               np       nn
          (An)          |  8(1/0)  4(1/0) |            nr |               np       nn
          (An)+         |  8(1/0)  4(1/0) |            nr |               np       nn
          -(An)         |  8(1/0)  6(1/0) | n          nr |               np       nn
          (d16,An)      |  8(1/0)  8(2/0) |      np    nr |               np       nn
          (d8,An,Xn)    |  8(1/0) 10(2/0) | n    np    nr |               np       nn
          (xxx).W       |  8(1/0)  8(2/0) |      np    nr |               np       nn
          (xxx).L       |  8(1/0) 12(3/0) |   np np    nr |               np       nn
          #<data>       |  8(1/0)  4(1/0) |      np       |               np       nn
        .L :            |                 |               |
          Dn            |  8(1/0)  0(0/0) |               |               np       nn
          An            |  8(1/0)  0(0/0) |               |               np       nn
          (An)          |  6(1/0)  8(2/0) |         nR nr |               np       n
          (An)+         |  6(1/0)  8(2/0) |         nR nr |               np       n
          -(An)         |  6(1/0) 10(2/0) | n       nR nr |               np       n
          (d16,An)      |  6(1/0) 12(3/0) |      np nR nr |               np       n
          (d8,An,Xn)    |  6(1/0) 14(3/0) | n    np nR nr |               np       n
          (xxx).W       |  6(1/0) 12(3/0) |      np nR nr |               np       n
          (xxx).L       |  6(1/0) 16(4/0) |   np np nR nr |               np       n
          #<data>       |  8(1/0)  8(2/0) |   np np       |               np       nn
    */
    final override def execute(): Unit = {
      val srcOp = ctx.getEA(mode,reg,size)
      val a = srcOp.get(size,signExtended = true)
      val an = ctx.getRegister(Address,register)
      val b = an.get(Size.Long)
      val r = a + b
      an.set(r,Size.Long)

      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================

      if size == Size.Long then
        if srcOp.isRegisterMode || srcOp.mode == AddressingMode.IM then
          ctx.busIdle(4)
        else
          ctx.busIdle(2)
      else
          ctx.busIdle(4)
    }

    override def disassemble(address: Int): DisassembledInstruction = {
      val srcOp = ctx.getEA(mode,reg,size,Some(address))
      val an = ctx.getRegister(Address,register)

      DisassembledInstruction(address,opcode,s"${instructionType.mnemonic}${size.ext}",srcOp.getExtensionWords,Some(srcOp.getMnemonic(address)),Some(an.mnemonic))
    }

class ADDA(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 1 | 1 | 0 | 1 |  Register |  Opmode   |   Mode    |   Reg     |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     Size = (Word, Long)
     Opmode:
     - 011: Word operation; the source operand is sign-extended to a long operand and the operation is performed on the address register using all 32 bits.
     - 111: Long operation.
  
     Flags not affected.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    import Size.*
    val code = genOpcode("1101____________")
    for(size <- Seq(Word,Long)) {
      val opmode = if size == Word then 3 else 7
      for(register <- 0 to 7) {
        for(mode <- 0 to 7) {
          val regEnd = if mode == 7 then 4 else 7
          for (reg <- 0 to regEnd) {
            val opcode = code | register << 9 | opmode << 6 | mode << 3 | reg
            instructionSetHandler.registerInstruction(opcode, new ADDA.ADDA(ctx,opcode,size))
          }
        }
      }
    }
