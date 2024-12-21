package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object ADDQ:
  class ADDQ(override protected val ctx: M6800X0.Context,
             override protected val opcode:Int,
             size:Size,
             imm:Int,
             isEaAddrReg:Boolean,
             eaAddrReg:Int) extends ADD.ADD(ctx,opcode,size,false):

    override val instructionType : InstructionType = InstructionType.ADDQ

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
        ADDQ, SUBQ    |  INSTR     EA   |  1st OP (ea)  |          INSTR
    ------------------+-----------------+---------------+--------------------------
    #<data>,<ea> :    |                 |               |
      .B or .W :      |                 |               |
        Dn            |  4(1/0)  0(0/0) |               |               np
        An            |  8(1/0)  0(0/0) |               |               np       nn
        (An)          |  8(1/1)  4(1/0) |            nr |               np nw
        (An)+         |  8(1/1)  4(1/0) |            nr |               np nw
        -(An)         |  8(1/1)  6(1/0) | n          nr |               np nw
        (d16,An)      |  8(1/1)  8(2/0) |      np    nr |               np nw
        (d8,An,Xn)    |  8(1/1) 10(2/0) | n    np    nr |               np nw
        (xxx).W       |  8(1/1)  8(2/0) |      np    nr |               np nw
        (xxx).L       |  8(1/1) 12(3/0) |   np np    nr |               np nw
      .L :            |                 |               |
        Dn            |  8(1/0)  0(0/0) |               |               np       nn
        An            |  8(1/0)  0(0/0) |               |               np       nn
        (An)          | 12(1/2)  8(2/0) |         nR nr |               np nw nW
        (An)+         | 12(1/2)  8(2/0) |         nR nr |               np nw nW
        -(An)         | 12(1/2) 10(2/0) | n       nR nr |               np nw nW
        (d16,An)      | 12(1/2) 12(3/0) |      np nR nr |               np nw nW
        (d8,An,Xn)    | 12(1/2) 14(3/0) | n    np nR nr |               np nw nW
        (xxx).W       | 12(1/2) 12(3/0) |      np nR nr |               np nw nW
        (xxx).L       | 12(1/2) 16(4/0) |   np np nR nr |               np nw nW
    */
    final override def execute(): Unit = {
      if isEaAddrReg then
        val reg = ctx.getRegister(RegisterType.Address,eaAddrReg)
        val a = reg.get(Size.Long)
        // =============== prefetch==================
        ctx.fetchWord(false)
        // ==========================================
        reg.set(a + imm,Size.Long)
        ctx.busIdle(4)
      else
        val eaOp = ctx.getEA(mode, reg, size)
        val a = eaOp.get(size, signExtended = true)

        val r = a + imm
        // =============== prefetch==================
        ctx.fetchWord(false)
        // ==========================================
        eaOp.set(r,size)
        if !isEaAddrReg then
          setFlags(a,imm,r,size)

        if size == Size.Long then
          if eaOp.isRegisterMode then
            ctx.busIdle(4)
        else if mode == 1 then
            ctx.busIdle(4)
    }

    override def disassemble(address: Int): DisassembledInstruction = {
      val eaOp = ctx.getEA(mode,reg,size,Some(address))

      DisassembledInstruction(address,opcode,s"${instructionType.mnemonic}${size.ext}",eaOp.getExtensionWords,Some(s"#${imm.toHexString}"),Some(eaOp.getMnemonic(address)))
    }

class ADDQ(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 1 | 0 | 1 |   Data    | 0 | Size  |   Mode    |   Reg     |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     When adding to address registers, the condition codes are not altered, and the entire destination address
     register is used regardless of the operation size.

     Size = (Byte, Word, Long)
      00 — Byte operation
      01 — Word operation
      10 — Long operation
  
     X — Set the same as the carry bit.
     N — Set if the result is negative; cleared otherwise.
     Z — Set if the result is zero; cleared otherwise.
     V — Set if an overflow is generated; cleared otherwise.
     C — Set if a carry is generated; cleared otherwise.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    import Size.*
    val code = genOpcode("0101___0________")
    for(size <- Seq(Byte,Word,Long)) {
      val modes = if size == Byte then Seq(0,2,3,4,5,6,7) else 0 to 7 // if size is byte no address register is allowed for ea
      for(mode <- modes) {
        val regEnd = if mode == 7 then 1 else 7
        for(reg <- 0 to regEnd) {
          for(imm <- 0 to 7) {
            val im = if imm == 0 then 8 else imm
            val opcode = code | imm << 9 | size.ordinal << 6 | mode << 3 | reg
            instructionSetHandler.registerInstruction(opcode, new ADDQ.ADDQ(ctx,opcode, size, im, mode == 1, reg))
          }
        }
      }
    }
