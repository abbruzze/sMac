package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object LSL_LSR:
  class LSL_LSR(override protected val ctx: M6800X0.Context,
                override protected val opcode:Int,
                size:Size,
                creg:Int,
                shiftLeft:Boolean,
                immediateShiftCount:Boolean) extends Instruction(ctx,opcode):

    override val instructionType : InstructionType = if shiftLeft then InstructionType.LSL else InstructionType.LSR
    protected final val eaMode = ((opcode >> 6) & 3) == 3

    /*
    ------------------------------------------------------------------------------- 
         ASL, ASR,    |    Exec Time    |               Data Bus Usage              
         LSL, LSR,    |                 |                                           
         ROL, ROR,    |                 |                                           
        ROXL, ROXR    |  INSTR     EA   |  1st OP (ea)  |          INSTR            
    ------------------+-----------------+---------------+-------------------------- 
    Dx,Dy :           |                 |               |                           
      .B or .W :      |  6+2m(1/0)      |               |               np    n* n  
      .L :            |  8+2m(1/0)      |               |               np    n* nn 
    #<data>,Dy :      |                 |               |                           
      .B or .W :      |  6+2m(1/0)      |               |               np    n  n* 
      .L :            |  8+2m(1/0)      |               |               np    nn n* 
    <ea> :            |                 |               |                           
      .B or .W :      |                 |               |                           
        (An)          |  8(1/1)  4(1/0) |            nr |               np    nw    
        (An)+         |  8(1/1)  4(1/0) |            nr |               np    nw    
        -(An)         |  8(1/1)  6(1/0) | n          nr |               np    nw    
        (d16,An)      |  8(1/1)  8(2/0) |      np    nr |               np    nw    
        (d8,An,Xn)    |  8(1/1) 10(2/0) | n    np    nr |               np    nw
        (xxx).W       |  8(1/1)  8(2/0) |      np    nr |               np    nw    
        (xxx).L       |  8(1/1) 12(3/0) |   np np    nr |               np    nw    
    */
    final override def execute(): Unit = {
      var shift = 0
      var count = 0
      var shiftSize = size
      var eaOp : Operand = null
      var regOp : Register = null

      if eaMode then
        eaOp = ctx.getEA(mode,reg,Size.Word)
        shift = eaOp.get(Size.Word)
        shiftSize = Size.Word
        count = 1
      else
        regOp = ctx.getRegister(RegisterType.Data,reg)
        shift = regOp.get(shiftSize)
        if (immediateShiftCount) {
          count = creg
          if count == 0 then count = 8
        }
        else {
          count = ctx.getRegister(RegisterType.Data,creg).get(Size.Byte) & 0x3F
        }

      // shifting
      var carry = false
      var i = 0
      while i < count do
        if shiftLeft then
          carry = (shift & shiftSize.msb) != 0
          shift <<= 1
        else
          carry = (shift & 1) != 0
          shift >>>= 1
        i += 1
      end while

      // status register flags
      import StatusRegister.StatusFlag.*
      var ccr = flags.getCCR
      if count > 0 then
        if carry then ccr |= X.flag else ccr &= ~X.flag
      if carry then ccr |= C.flag else ccr &= ~C.flag
      if (shift & shiftSize.msb) != 0 then ccr |= N.flag else ccr &= ~N.flag
      ccr &= ~V.flag
      if (shift & shiftSize.mask) == 0 then ccr |= Z.flag else ccr &= ~Z.flag
      flags.setCCR(ccr)

      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================

      if !eaMode then
        if shiftSize == Size.Long then ctx.busIdle((count + 2) << 1) else ctx.busIdle((count + 1) << 1)

      // storing result
      if eaMode then
        eaOp.set(shift,Size.Word)
      else
        regOp.set(shift,shiftSize)
    }

    override def disassemble(address: Int): DisassembledInstruction = {
      val size = if this.size == null then Size.Word else this.size
      if eaMode then
        val eaOp = ctx.getEA(mode, reg, size,Some(address))
        DisassembledInstruction(address,opcode,s"${instructionType.mnemonic}",eaOp.getExtensionWords,Some(eaOp.getMnemonic(address)))
      else
        if immediateShiftCount then
          var count = creg
          if count == 0 then count = 8
          DisassembledInstruction(address,opcode,s"${instructionType.mnemonic}${size.ext}",Nil,Some(s"#$count"),Some(s"d$reg"))
        else
          DisassembledInstruction(address,opcode,s"${instructionType.mnemonic}${size.ext}",Nil,Some(s"d$creg"),Some(s"d$reg"))
    }

class LSL_LSR(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     1) LSd # < data > ,Dy
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 1 | 1 | 1 | 0 |C.Register | dr|  size | ir| 0 | 1 | Register  |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     Size = (Byte, Word, Long)
     Count/Register field—Specifies shift count or register that contains the shift count:
      If i/r = 0, this field contains the shift count. The values 1 – 7 represent counts of 1 –
      7; a value of zero represents a count of eight.
      If i/r = 1, this field specifies the data register that contains the shift count (modulo 64).
     dr field—Specifies the direction of the shift.
      0 — Shift right
      1 — Shift left
     Size field—Specifies the size of the operation.
      00 — Byte operation
      01 — Word operation
      10 — Long operation
     i/r field
      If i/r = 0, specifies immediate shift count.
      If i/r = 1, specifies register shift count.
     Register field—Specifies a data register to be shifted.

     2) LSd < ea >
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 1 | 1 | 1 | 0 | 0 | 0 | 1 | dr| 1 | 1 |    Mode   |   Reg     |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     dr field—Specifies the direction of the shift.
      0 — Shift right
      1 — Shift left

    X — Set according to the last bit shifted out of the operand; unaffected for a shift count of zero.
    N — Set if the result is negative; cleared otherwise.
    Z — Set if the result is zero; cleared otherwise.
    V — Always cleared.
    C — Set according to the last bit shifted out of the operand; cleared for a shift count of zero.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    import Size.*
    // 1)
    var code = genOpcode("1110_______01___")
    for(size <- Seq(Byte,Word,Long))
      for(dr <- 0 to 1)
        for(ir <- 0 to 1)
          for(creg <- 0 to 7)
            for(reg <- 0 to 7)
              val opcode = code | creg << 9 | dr << 8 | size.ordinal << 6 | ir << 5 | reg
              instructionSetHandler.registerInstruction(opcode,new LSL_LSR.LSL_LSR(ctx,opcode,size,creg,dr == 1,ir == 0))

    // 2)
    code = genOpcode("1110001_11______")
    for(dr <- 0 to 1)
      for (mode <- 2 to 7)
        val regEnd = if mode == 7 then 1 else 7
        for (reg <- 0 to regEnd)
          val opcode = code | dr << 8 | mode << 3 | reg
          instructionSetHandler.registerInstruction(opcode, new LSL_LSR.LSL_LSR(ctx, opcode, null, 0, dr == 1, false))
