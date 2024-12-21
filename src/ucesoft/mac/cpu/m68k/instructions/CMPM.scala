package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object CMPM:
  class CMPM(override protected val ctx: M6800X0.Context,
             override protected val opcode:Int,
             override protected val size:Size) extends CMP.CMP(ctx,opcode,size):

    override val instructionType : InstructionType = InstructionType.CMPM
    protected val ay = opcode & 7
    protected val ax = (opcode >> 9) & 7

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
           CMPM       |      INSTR      |                  INSTR
    ------------------+-----------------+------------------------------------------
    (Ay)+,(Ax)+       |                 |
      .B or .W :      | 12(3/0)         |                      nr    nr np
      .L :            | 20(5/0)         |                   nR nr nR nr np
    */
    final override def execute(): Unit = {
      val regAx = ctx.getRegister(RegisterType.Address, ax)
      val regAy = ctx.getRegister(RegisterType.Address, ay)
      val src = ctx.readMemory(regAy.get(Size.Long), size)
      regAy.increment(size)
      val dst = ctx.readMemory(regAx.get(Size.Long),size)
      regAx.increment(size)

      setFlags(src,dst,dst - src,size)

      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================
    }

    override def disassemble(address: Int): DisassembledInstruction = {
      val eaOp = ctx.getEA(mode,reg,size,Some(address))
      DisassembledInstruction(address, opcode, s"${instructionType.mnemonic}${size.ext}", Nil, Some(s"(a$ay)+"),Some(s"(a$ax)+"))
    }

class CMPM(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     1) CMPM (Ay) + ,(Ax) +
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 1 | 0 | 1 | 1 |Register Ax| 1 | Size  | 0 | 0 | 1 |Register Ay|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     Size = (Byte,Word, Long)

    X — Not affected.
    N — Set if the result is negative; cleared otherwise.
    Z — Set if the result is zero; cleared otherwise.
    V — Set if an overflow occurs; cleared otherwise.
    C — Set if a borrow occurs; cleared otherwise.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    import Size.*
    val code = genOpcode("1011___1__001___")
    for(size <- Seq(Byte,Word,Long))
      for(ay <- 0 to 7)
        for(ax <- 0 to 7)
          val opcode = code | ax << 9 | size.ordinal << 6 | ay
          instructionSetHandler.registerInstruction(opcode, new CMPM.CMPM(ctx, opcode,size))