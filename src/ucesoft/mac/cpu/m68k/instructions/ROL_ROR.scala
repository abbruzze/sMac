package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object ROL_ROR:
  class ROL_ROR(override protected val ctx: M6800X0.Context,
                override protected val opcode:Int,
                protected val size:Size,
                protected val creg:Int,
                protected val rotateLeft:Boolean,
                protected val immediateRotateCount:Boolean) extends Instruction(ctx,opcode):

    override val instructionType : InstructionType = if rotateLeft then InstructionType.ROL else InstructionType.ROR
    protected final val eaMode = ((opcode >> 6) & 3) == 3
    protected final lazy val isROTX = instructionType == InstructionType.ROXL || instructionType == InstructionType.ROXR

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
      import StatusRegister.StatusFlag.*
      var rotate = 0
      var count = 0
      var rotateSize = size
      var eaOp : Operand = null
      var regOp : Register = null

      if eaMode then
        eaOp = ctx.getEA(mode,reg,Size.Word)
        rotate = eaOp.get(Size.Word)
        rotateSize = Size.Word
        count = 1
      else
        regOp = ctx.getRegister(RegisterType.Data,reg)
        rotate = regOp.get(rotateSize)
        if (immediateRotateCount) {
          count = creg
          if count == 0 then count = 8
        }
        else {
          count = ctx.getRegister(RegisterType.Data,creg).get(Size.Byte) & 0x3F
        }

      // rotate
      var carry = false
      var x = flags.isFlag(X)

      var i = 0
      while i < count do
        if rotateLeft then
          carry = (rotate & rotateSize.msb) != 0
          rotate <<= 1

          if isROTX then
            if x then rotate |= 1
            x = carry
          else if carry then
            rotate |= 1
        else
          carry = (rotate & 1) != 0
          rotate >>>= 1

          if isROTX then
            if x then rotate |= rotateSize.msb
            x = carry
          else if carry then
              rotate |= rotateSize.msb
        i += 1
      end while

      // status register flags
      var ccr = flags.getCCR
      if isROTX then
        if x then ccr |= X.flag else ccr &= ~X.flag
        if count == 0 then carry = x
      if carry then ccr |= C.flag else ccr &= ~C.flag
      if (rotate & rotateSize.msb) != 0 then ccr |= N.flag else ccr &= ~N.flag
      ccr &= ~V.flag
      if (rotate & rotateSize.mask) == 0 then ccr |= Z.flag else ccr &= ~Z.flag
      flags.setCCR(ccr)

      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================

      if !eaMode then
        if rotateSize == Size.Long then ctx.busIdle((count + 2) << 1) else ctx.busIdle((count + 1) << 1)

      // storing result
      if eaMode then
        eaOp.set(rotate,Size.Word)
      else
        regOp.set(rotate,rotateSize)
    }

    override def disassemble(address: Int): DisassembledInstruction = {
      val size = if this.size == null then Size.Word else this.size
      if eaMode then
        val eaOp = ctx.getEA(mode, reg, size,Some(address))
        DisassembledInstruction(address,opcode,s"${instructionType.mnemonic}",eaOp.getExtensionWords,Some(eaOp.getMnemonic(address)))
      else
        if immediateRotateCount then
          var count = creg
          if count == 0 then count = 8
          DisassembledInstruction(address,opcode,s"${instructionType.mnemonic}${size.ext}",Nil,Some(s"#$count"),Some(s"d$reg"))
        else
          DisassembledInstruction(address,opcode,s"${instructionType.mnemonic}${size.ext}",Nil,Some(s"d$creg"),Some(s"d$reg"))
    }

class ROL_ROR(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     1) ROd Dx,Dy
        ROd # < data > ,Dy
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 1 | 1 | 1 | 0 |C.Register | dr|  size | ir| 1 | 1 | Register  |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     Size = (Byte, Word, Long)
     Count/Register field—Specifies shift count or register that contains the shift count:
      If i/r = 0, this field contains the shift count. The values 1 – 7 represent counts of 1 –
      7; a value of zero represents a count of eight.
      If i/r = 1, this field specifies the data register that contains the rotate count (modulo 64).
     dr field—Specifies the direction of the rotate.
      0 — Rotate right
      1 — Rotate left
     Size field—Specifies the size of the operation.
      00 — Byte operation
      01 — Word operation
      10 — Long operation
     i/r field
      If i/r = 0, specifies immediate rotate count.
      If i/r = 1, specifies register rotate count.
     Register field—Specifies a data register to be rotated.

     2) ROd < ea >
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 1 | 1 | 1 | 0 | 0 | 1 | 1 | dr| 1 | 1 |    Mode   |   Reg     |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     dr field—Specifies the direction of the rotate.
      0 — Rotate right
      1 — Rotate left

    X — Not affected.
    N — Set if the most significant bit of the result is set; cleared otherwise.
    Z — Set if the result is zero; cleared otherwise.
    V — Always cleared.
    C — Set according to the last bit rotated out of the operand; cleared when the rotate count is zero.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    import Size.*
    // 1)
    var code = genOpcode("1110_______11___")
    for(size <- Seq(Byte,Word,Long))
      for(dr <- 0 to 1)
        for(ir <- 0 to 1)
          for(creg <- 0 to 7)
            for(reg <- 0 to 7)
              val opcode = code | creg << 9 | dr << 8 | size.ordinal << 6 | ir << 5 | reg
              instructionSetHandler.registerInstruction(opcode,new ROL_ROR.ROL_ROR(ctx,opcode,size,creg,dr == 1,ir == 0))

    // 2)
    code = genOpcode("1110011_11______")
    for(dr <- 0 to 1)
      for (mode <- 2 to 7)
        val regEnd = if mode == 7 then 1 else 7
        for (reg <- 0 to regEnd)
          val opcode = code | dr << 8 | mode << 3 | reg
          instructionSetHandler.registerInstruction(opcode, new ROL_ROR.ROL_ROR(ctx, opcode, null, 0, dr == 1, false))
