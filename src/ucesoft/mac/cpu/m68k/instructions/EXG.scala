package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object EXG:
  class EXG(override protected val ctx: M6800X0.Context,
            override protected val opcode:Int) extends Instruction(ctx,opcode):
    import RegisterType.*

    override val instructionType : InstructionType = InstructionType.EXG
    private final val rx = (opcode >> 9) & 7
    private final val D2D = ((opcode >> 3) & 0x1F) == 0x8
    private final val A2A = ((opcode >> 3) & 0x1F) == 0x9
    private final val D2A = ((opcode >> 3) & 0x1F) == 0x11

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
            EXG       |      INSTR      |                  INSTR
    ------------------+-----------------+------------------------------------------
      .L :            |                 |
        Dx,Dy         |  6(1/0)         |                               np       n
        Ax,Ay         |  6(1/0)         |                               np       n
        Dx,Ay         |  6(1/0)         |                               np       n

    -------------------------------------------------------------------------------
    */
    final override def execute(): Unit =
      var rxType = RegisterType.Data
      var ryType = RegisterType.Data

      if A2A then
        rxType = RegisterType.Address
        ryType = RegisterType.Address
      else if D2A then
        ryType = RegisterType.Address

      val regx = ctx.getRegister(rxType, rx)
      val regy = ctx.getRegister(ryType, reg)
      val tmp = regx.get(Size.Long)
      regx.set(regy.get(Size.Long), Size.Long)
      regy.set(tmp, Size.Long)

      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================
      ctx.busIdle(2)

    override def disassemble(address: Int): DisassembledInstruction =
      if D2D then
        DisassembledInstruction(address,opcode,instructionType.mnemonic,Nil,Some(s"d$rx"),Some(s"d$reg"))
      else if A2A then
        DisassembledInstruction(address,opcode,instructionType.mnemonic,Nil,Some(s"a$rx"),Some(s"a$reg"))
      else
        DisassembledInstruction(address,opcode,instructionType.mnemonic,Nil,Some(s"d$rx"),Some(s"a$reg"))

class EXG(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     EXG Dx,Dy
     EXG Ax,Ay
     EXG Dx,Ay

     Size = (Long)
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 1 | 1 | 0 | 0 |Register rx| 1 |     opmode        |Register ry|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     Opmode field—Specifies the type of exchange.
      01000 — Data registers
      01001 — Address registers
      10001 — Data register and address register

     Condition Codes:
      Not affected.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    val code = genOpcode("1100___1________")
    for(opmode <- Seq(0x8,0x9,0x11))
      for(rx <- 0 to 7)
        for(ry <- 0 to 7)
          val opcode = code | rx << 9 | opmode << 3 | ry
          instructionSetHandler.registerInstruction(opcode,new EXG.EXG(ctx,opcode))
