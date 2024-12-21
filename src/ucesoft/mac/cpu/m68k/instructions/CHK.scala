package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object CHK:
  class CHK(override protected val ctx: M6800X0.Context,
            override protected val opcode:Int,
            size:Size) extends Instruction(ctx,opcode):

    override val instructionType : InstructionType = InstructionType.CHK
    private final val register = (opcode >> 9) & 7

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
            CHK       |  INSTR     EA   |  1st OP (ea)  |          INSTR
    ------------------+-----------------+---------------+--------------------------
    no trap :         |                 |               |
      <ea>,Dn :       |                 |              /
        .W :          |                 |             /
          Dn          | 10(1/0)  0(0/0) |            |                  np nn n
          (An)        | 10(1/0)  4(1/0) |         nr |                  np nn n
          (An)+       | 10(1/0)  4(1/0) |         nr |                  np nn n
          -(An)       | 10(1/0)  6(1/0) | n       nr |                  np nn n
          (d16,An)    | 10(1/0)  8(2/0) |      np nr |                  np nn n
          (d8,An,Xn)  | 10(1/0) 10(2/0) | n    np nr |                  np nn n
          (xxx).W     | 10(1/0)  8(2/0) |      np nr |                  np nn n
          (xxx).L     | 10(1/0) 12(2/0) |   np np nr |                  np nn n
          #<data>     | 10(1/0)  4(1/0) |      np    |                  np nn n
    trap :            |                 |            |
      <ea>,Dn :       |                 |            |
        .W :          |                 |            |
          Dn > Src :  |                 |            |
            Dn        | 36(5/3)  0(0/0) |            |   np nn ns nS ns np np np np
            (An)      | 36(5/3)  4(1/0) |         nr |   np nn ns nS ns np np np np
            (An)+     | 36(5/3)  4(1/0) |         nr |   np nn ns nS ns np np np np
            -(An)     | 36(5/3)  6(1/0) | n       nr |   np nn ns nS ns np np np np
            (d16,An)  | 36(5/3)  8(2/0) |      np nr |   np nn ns nS ns np np np np
            (d8,An,Xn)| 36(5/3) 10(2/0) | n    np nr |   np nn ns nS ns np np np np
            (xxx).W   | 36(5/3)  8(2/0) |      np nr |   np nn ns nS ns np np np np
            (xxx).L   | 36(5/3) 12(2/0) |   np np nr |   np nn ns nS ns np np np np
            #<data>   | 36(5/3)  4(1/0) |      np    |   np nn ns nS ns np np np np
          Dn <0 :     |                 |            |
            Dn        | 40(5/3)  0(0/0) |            |np n- nn ns nS ns np np np np
            (An)      | 40(5/3)  4(1/0) |         nr |np n- nn ns nS ns np np np np
            (An)+     | 40(5/3)  4(1/0) |         nr |np n- nn ns nS ns np np np np
            -(An)     | 40(5/3)  6(1/0) | n       nr |np n- nn ns nS ns np np np np
            (d16,An)  | 40(5/3)  8(2/0) |      np nr |np n- nn ns nS ns np np np np
            (d8,An,Xn)| 40(5/3) 10(2/0) | n    np nr |np n- nn ns nS ns np np np np
            (xxx).W   | 40(5/3)  8(2/0) |      np nr |np n- nn ns nS ns np np np np
            (xxx).L   | 40(5/3) 12(2/0) |   np np nr |np n- nn ns nS ns np np np np
            #<data>   | 40(5/3)  4(1/0) |      np    |np n- nn ns nS ns np np np np
    */
    final override def execute(): Unit = {
      import StatusRegister.StatusFlag.*
      val eaOp = ctx.getEA(mode,reg,size)
      val upperBound = eaOp.get(size,signExtended = true)
      val dn = ctx.getRegister(RegisterType.Data,register).get(size,signExtended = true)

      // cleared to pass tests, but documentation says UNDEFINED
      flags.clearFlag(Z)
      flags.clearFlag(V)
      flags.clearFlag(C)

      if dn < 0 then
        flags.setFlag(N)
        ctx.busIdle(8)  // n- nn  ?
        ctx.raiseException(6)
        // =============== prefetch==================
          ctx.fetchWord(false)
        // ==========================================
      else if dn > upperBound then
        flags.clearFlag(N)
        ctx.busIdle(4) // nn
        ctx.raiseException(6)
        // =============== prefetch==================
          ctx.fetchWord(false)
        // ==========================================
      else
        // =============== prefetch==================
        ctx.fetchWord(false)
        // ==========================================
        ctx.busIdle(6)
    }

    override def disassemble(address: Int): DisassembledInstruction = {
      val eaOp = ctx.getEA(mode,reg,size,Some(address))
      DisassembledInstruction(address, opcode, s"${instructionType.mnemonic}${size.ext}", eaOp.getExtensionWords, Some(eaOp.getMnemonic(address)),Some(s"d$register"))
    }

class CHK(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     1) CHK < ea >, Dn
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 1 | 0 | 0 | Register  |  Size | 0 |   Mode    |    Reg    |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     Size = (Word, Long*)
     *(MC68020, MC68030, MC68040 only)

    X — Not affected.
    N — Set if Dn < 0; cleared if Dn > effective address operand; undefined otherwise.
    Z — Undefined.
    V — Undefined.
    C — Undefined.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    val code = genOpcode("0100_____0______")
    for(register <- 0 to 7)
      for(mode <- Seq(0,2,3,4,5,6,7))
        val regEnd = if mode == 7 then 4 else 7
        for(reg <- 0 to regEnd)
          val opcode = code | register << 9 | 3 << 7 | mode << 3 | reg // 3 = Size.Word
          instructionSetHandler.registerInstruction(opcode, new CHK.CHK(ctx, opcode,Size.Word))