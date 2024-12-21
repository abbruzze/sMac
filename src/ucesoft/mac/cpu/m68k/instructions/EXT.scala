package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object EXT:
  class EXT(override protected val ctx: M6800X0.Context,
            override protected val opcode:Int) extends Instruction(ctx,opcode):
    import RegisterType.*

    override val instructionType : InstructionType = InstructionType.EXT
    private final val B2W = ((opcode >> 6) & 7) == 2
    private final val W2L = ((opcode >> 6) & 7) == 3

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
            EXT       |      INSTR      |  1st Operand  |          INSTR
    ------------------+-----------------+---------------+--------------------------
    Dn :              |                 |               |
      .W :            |  4(1/0)         |               |               np
      .L :            |  4(1/0)         |               |               np

    */
    final override def execute(): Unit =
      var ccr = flags.getCCR

      import StatusRegister.StatusFlag.*
      val dn = ctx.getRegister(RegisterType.Data,reg)
      var size = Size.Byte
      var sv = 0
      if B2W then
        sv = dn.get(Size.Byte,signExtended = true)
        dn.set(sv,Size.Word)
      else if W2L then
        size = Size.Word
        sv = dn.get(Size.Word, signExtended = true)
        dn.set(sv, Size.Long)
        if (sv & Size.Word.msb) != 0 then flags.setFlag(N) else flags.clearFlag(N)

      if (sv & size.msb) != 0 then ccr |= N.flag else ccr &= ~N.flag
      if (sv & size.mask) == 0 then ccr |= Z.flag else ccr &= ~Z.flag

      ccr &= ~(V.flag | C.flag)
      flags.setCCR(ccr)

      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================

    override def disassemble(address: Int): DisassembledInstruction =
      val ext = if B2W then ".w" else ".l"
      DisassembledInstruction(address,opcode,s"${instructionType.mnemonic}$ext",Nil,Some(s"d$reg"))

class EXT(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     EXT.W Dn
     EXT.L Dn

     Size = (Word,Long)
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 1 | 0 | 0 | 1 | 0 | 0 |   Opmode  | 0 | 0 | 0 | Register  |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     Opmode field—Specifies the size of the sign-extension operation.
      010 — Sign-extend low-order byte of data register to word.
      011 — Sign-extend low-order word of data register to long.
      111 — Sign-extend low-order byte of data register to long.

    Condition codes:
    X — Not affected.
    N — Set if the result is negative; cleared otherwise.
    Z — Set if the result is zero; cleared otherwise.
    V — Always cleared.
    C — Always cleared.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    val code = genOpcode("0100100___000___")
    for(opmode <- Seq(0x2,0x3))
      for(reg <- 0 to 7)
        val opcode = code | opmode << 6 | reg
        instructionSetHandler.registerInstruction(opcode,new EXT.EXT(ctx,opcode))
