package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object SUBX:
  class SUBX(override protected val ctx: M6800X0.Context,
             override protected val opcode:Int,
             dr:Int,
             sr:Int,
             size:Size,
             data2data:Boolean) extends SUB.SUB(ctx,opcode,size,false):

    override val instructionType : InstructionType = InstructionType.SUBX

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
        ADDX, SUBX    |      INSTR      |                  INSTR
    ------------------+-----------------+------------------------------------------
    Dy,Dx :           |                 |
      .B or .W :      |  4(1/0)         |                               np
      .L :            |  8(1/0)         |                               np       nn
    -(Ay),-(Ax) :     |                 |
      .B or .W :      | 18(3/1)         |              n nr    nr       np nw
      .L :            | 30(5/2)         |              n nr nR nr nR nw np    nW
    */
    final override def execute(): Unit = {
      val x = flags.getFlag(StatusRegister.StatusFlag.X)

      if (data2data) {
        val dstReg = ctx.getRegister(RegisterType.Data, dr)
        val s = ctx.getRegister(RegisterType.Data, sr).get(size, signExtended = true)
        val d = dstReg.get(size, signExtended = true)
        val r = d - s - x
        dstReg.set(r, size)
        setFlags(s, d, r, size)
        // =============== prefetch==================
        ctx.fetchWord(false)
        // ==========================================
        if size == Size.Long then
          ctx.busIdle(4)
      }
      else {
        ctx.busIdle(2)
        val sReg = ctx.getRegister(RegisterType.Address, sr)
        sReg.decrement(size)
        val sAddr = sReg.get(Size.Long)
        val s = extendSign(size, ctx.readMemory(sAddr, size))
        val dReg = ctx.getRegister(RegisterType.Address, dr)
        dReg.decrement(size)
        val dAddr = dReg.get(Size.Long)
        val d = extendSign(size, ctx.readMemory(dAddr, size))
        val r = d - s - x
        // =============== prefetch==================
        ctx.fetchWord(false)
        // ==========================================
        ctx.writeMemory(dAddr, r, size)
        setFlags(s, d, r, size)
      }
    }

    override protected def setFlags(s: Int, d: Int, r: Int, size: Size): Unit = {
      import StatusRegister.StatusFlag.*
      var ccr = flags.getCCR
      val sm = (s & size.msb) != 0
      val dm = (d & size.msb) != 0
      val rm = (r & size.msb) != 0
      val rz = (r & size.mask) == 0

      if (!sm && dm && !rm) || (sm && !dm && rm) then ccr |= V.flag else ccr &= ~V.flag
      if (sm && !dm) || (rm && !dm) || (sm && rm) then ccr |= C.flag | X.flag else ccr &= ~(C.flag | X.flag)
      if !rz then ccr &= ~Z.flag
      if rm then ccr |= N.flag else ccr &= ~N.flag

      flags.setCCR(ccr)
    }

    override def disassemble(address: Int): DisassembledInstruction = {
      if data2data then
        DisassembledInstruction(address,opcode,s"${instructionType.mnemonic}${size.ext}",Nil,Some(s"d$sr"),Some(s"d$dr"))
      else
        DisassembledInstruction(address,opcode,s"${instructionType.mnemonic}${size.ext}",Nil,Some(s"-(a$sr)"),Some(s"-(a$dr)"))
    }

class SUBX(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     SUBX Dx,Dy
     SUBX –(Ax), –(Ay)
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 1 | 0 | 0 | 1 |  Reg Ry   | 1 | Size  | 0 | 0 |R/M|  Reg Rx   |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

    Register Rx field—Specifies the destination register.
      If R/M = 0, specifies a data register.
      If R/M = 1, specifies an address register for the predecrement addressing mode

    Size = (Byte, Word, Long)
      00 — Byte operation
      01 — Word operation
      10 — Long operation

    R/M field—Specifies the operand address mode.
      0 — The operation is data register to data register.
      1 — The operation is memory to memory

    Register Ry field—Specifies the source register.
      If R/M = 0, specifies a data register.
      If R/M = 1, specifies an address register for the predecrement addressing mode
  
    X — Set to the value of the carry bit.
    N — Set if the result is negative; cleared otherwise.
    Z — Cleared if the result is nonzero; unchanged otherwise.
    V — Set if an overflow occurs; cleared otherwise.
    C — Set if a borrow occurs; cleared otherwise.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    import Size.*
    val code = genOpcode("1001___1__00____")
    for(size <- Seq(Byte,Word,Long)) {
      for(rm <- 0 to 1) {
        for (rx <- 0 to 7) {
          for (ry <- 0 to 7) {
            val opcode = code | rx << 9 | size.ordinal << 6 | rm << 3| ry
            instructionSetHandler.registerInstruction(opcode, new SUBX.SUBX(ctx,opcode,rx,ry,size,rm == 0))
          }
        }
      }
    }
