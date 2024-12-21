package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object ROXL_ROXR:
  class ROXL_ROXR(override protected val ctx: M6800X0.Context,
                  override protected val opcode:Int,
                  override protected val size:Size,
                  override protected val creg:Int,
                  override protected val rotateLeft:Boolean,
                  override protected val immediateRotateCount:Boolean) extends ROL_ROR.ROL_ROR(ctx,opcode,size,creg,rotateLeft,immediateRotateCount):

    override val instructionType : InstructionType = if rotateLeft then InstructionType.ROXL else InstructionType.ROXR

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


class ROXL_ROXR(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     1) ROXd Dx,Dy
        ROXd # < data > ,Dy
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 1 | 1 | 1 | 0 |C.Register | dr|  size | ir| 1 | 0 | Register  |
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

     2) ROXd < ea >
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 1 | 1 | 1 | 0 | 0 | 1 | 0 | dr| 1 | 1 |    Mode   |   Reg     |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     dr field—Specifies the direction of the rotate.
      0 — Rotate right
      1 — Rotate left

    X — Set to the value of the last bit rotated out of the operand; unaffected when the rotate count is zero.
    N — Set if the most significant bit of the result is set; cleared otherwise.
    Z — Set if the result is zero; cleared otherwise.
    V — Always cleared.
    C — Set according to the last bit rotated out of the operand; when the rotate count is zero, set to the value of the extend bit.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    import Size.*
    // 1)
    var code = genOpcode("1110_______10___")
    for(size <- Seq(Byte,Word,Long))
      for(dr <- 0 to 1)
        for(ir <- 0 to 1)
          for(creg <- 0 to 7)
            for(reg <- 0 to 7)
              val opcode = code | creg << 9 | dr << 8 | size.ordinal << 6 | ir << 5 | reg
              instructionSetHandler.registerInstruction(opcode,new ROXL_ROXR.ROXL_ROXR(ctx,opcode,size,creg,dr == 1,ir == 0))

    // 2)
    code = genOpcode("1110010_11______")
    for(dr <- 0 to 1)
      for (mode <- 2 to 7)
        val regEnd = if mode == 7 then 1 else 7
        for (reg <- 0 to regEnd)
          val opcode = code | dr << 8 | mode << 3 | reg
          instructionSetHandler.registerInstruction(opcode, new ROXL_ROXR.ROXL_ROXR(ctx, opcode, null, 0, dr == 1, false))
