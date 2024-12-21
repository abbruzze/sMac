package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object ADDX:
  class ADDX(override protected val ctx: M6800X0.Context,
             override protected val opcode:Int,
             rx:Int,
             ry:Int,
             size:Size,
             data2data:Boolean) extends ADD.ADD(ctx,opcode,size,false):

    override val instructionType : InstructionType = InstructionType.ADDX

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

      if data2data then
        val dstReg = ctx.getRegister(RegisterType.Data,rx)
        val a = ctx.getRegister(RegisterType.Data,ry).get(size,signExtended = true)
        val b = dstReg.get(size,signExtended = true)
        val r = a + b + x
        dstReg.set(r,size)
        setFlags(a,b,r,size)
        // =============== prefetch==================
        ctx.fetchWord(false)
        // ==========================================
        if size == Size.Long then
          ctx.busIdle(4)
      else
        ctx.busIdle(2)
        val regy = ctx.getRegister(RegisterType.Address,ry)
        regy.decrement(size)
        val ayAddr = regy.get(Size.Long)
        val a = extendSign(size,ctx.readMemory(ayAddr,size))
        val regx = ctx.getRegister(RegisterType.Address,rx)
        regx.decrement(size)
        val axAddr = regx.get(Size.Long)
        val b = extendSign(size,ctx.readMemory(axAddr,size))
        val r = a + b + x
        // =============== prefetch==================
        ctx.fetchWord(false)
        // ==========================================
        ctx.writeMemory(axAddr,r,size)
        setFlags(a,b,r,size)
    }

    override protected def setFlags(a: Int, b: Int, r: Int, size: Size): Unit = {
      import StatusRegister.StatusFlag.*
      var ccr = flags.getCCR
      val am = (a & size.msb) != 0
      val bm = (b & size.msb) != 0
      val rm = (r & size.msb) != 0
      val rz = (r & size.mask) == 0

      if (am && bm && !rm) || (!am && !bm && rm) then ccr |= V.flag else ccr &= ~V.flag
      if (am && bm) || (!rm && bm) || (am && !rm) then ccr |= C.flag | X.flag else ccr &= ~(C.flag | X.flag)
      if !rz then ccr &= ~Z.flag
      if rm then ccr |= N.flag else ccr &= ~N.flag

      flags.setCCR(ccr)
    }

    override def disassemble(address: Int): DisassembledInstruction = {
      if data2data then
        DisassembledInstruction(address,opcode,s"${instructionType.mnemonic}${size.ext}",Nil,Some(s"d$ry"),Some(s"d$rx"))
      else
        DisassembledInstruction(address,opcode,s"${instructionType.mnemonic}${size.ext}",Nil,Some(s"-(a$ry)"),Some(s"-(a$rx)"))
    }

class ADDX(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 1 | 1 | 0 | 1 |  Reg Rx   | 1 | Size  | 0 | 0 |R/M|  Reg Ry   |
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
  
    X — Set the same as the carry bit.
    N — Set if the result is negative; cleared otherwise.
    Z — Cleared if the result is nonzero; unchanged otherwise.
    V — Set if an overflow occurs; cleared otherwise.
    C — Set if a carry is generated; cleared otherwise.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    import Size.*
    val code = genOpcode("1101___1__00____")
    for(size <- Seq(Byte,Word,Long)) {
      for(rm <- 0 to 1) {
        for (rx <- 0 to 7) {
          for (ry <- 0 to 7) {
            val opcode = code | rx << 9 | size.ordinal << 6 | rm << 3| ry
            instructionSetHandler.registerInstruction(opcode, new ADDX.ADDX(ctx,opcode,rx,ry,size,rm == 0))
          }
        }
      }
    }
