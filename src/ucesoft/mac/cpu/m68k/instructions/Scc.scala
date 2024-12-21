package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*
import ucesoft.mac.cpu.m68k.M6800X0.BusAccessMode

object Scc:
  class Scc(override protected val ctx: M6800X0.Context,
            override protected val opcode:Int) extends Bcc.Bcc(ctx,opcode):
    import InstructionType.*
    import RegisterType.*

    override val isBranch = false

    private val instructions = Array(
      ST, SF, SHI, SLS, SCC, SCS, SNE, SEQ,
      SVC, SVS, SPL, SMI, SGE, SLT, SGT, SLE
    )
    override val instructionType : InstructionType = instructions(conditionCode)


    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
            Scc       |  INSTR     EA   |  1st OP (ea)  |          INSTR
    ------------------+-----------------+---------------+--------------------------
    <ea> :            |                 |               |
      .B :            |                 |               |
        cc false      |                 |               |
          Dn          |  4(1/0)  0(0/0) |               |               np
          (An)        |  8(1/1)  4(1/0) |            nr |               np    nw
          (An)+       |  8(1/1)  4(1/0) |            nr |               np    nw
          -(An)       |  8(1/1)  6(1/0) | n          nr |               np    nw
          (d16,An)    |  8(1/1)  8(2/0) |      np    nr |               np    nw
          (d8,An,Xn)  |  8(1/1) 10(2/0) | n    np    nr |               np    nw
          (xxx).W     |  8(1/1)  8(2/0) |      np    nr |               np    nw
          (xxx).L     |  8(1/1) 12(3/0) |   np np    nr |               np    nw
        cc true       |                 |               |
          Dn          |  6(1/0)  0(0/0) |               |               np       n
          (An)        |  8(1/1)  4(1/0) |            nr |               np    nw
          (An)+       |  8(1/1)  4(1/0) |            nr |               np    nw
          -(An)       |  8(1/1)  6(1/0) | n          nr |               np    nw
          (d16,An)    |  8(1/1)  8(2/0) |      np    nr |               np    nw
          (d8,An,Xn)  |  8(1/1) 10(2/0) | n    np    nr |               np    nw
          (xxx).W     |  8(1/1)  8(2/0) |      np    nr |               np    nw
          (xxx).L     |  8(1/1) 12(3/0) |   np np    nr |               np    nw
    */
    final override def execute(): Unit = {
      val eaOp = ctx.getEA(mode,reg,Size.Byte)
      // In the MC68000 and MC68008, a memory destination is read before it is written.
      eaOp.get(Size.Byte)
      val test = testCC
      val value = if test then 0xFF else 0x00
      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================
      if test && eaOp.isRegisterMode then
        ctx.busIdle(2)

      eaOp.set(value,Size.Byte)
    }

    override def disassemble(address: Int): DisassembledInstruction = {
      val eaOp = ctx.getEA(mode,reg,Size.Byte,Some(address))

      DisassembledInstruction(address, opcode, instructionType.mnemonic, eaOp.getExtensionWords, Some(eaOp.getMnemonic(address)))
    }

class Scc(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     1) Scc <ea>
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 1 | 0 | 1 |   Condition   | 1 | 1 |   Mode    |    Reg    |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


     Size = (Byte)

    Condition Codes:
    Not affected.

    Mnemonic Condition          Encoding   Test
    =================================================================
    T*          True              0000    1
    F*          False             0001    0
    HI          High              0010    !(C or Z)
    LS          Low or Same       0011    C or Z
    CC(HI)      Carry Clear       0100    !C
    CS(LO)      Carry Set         0101    C
    NE          Not Equal         0110    !Z
    EQ          Equal             0111    Z
    VC          Overflow Clear    1000    !V
    VS          Overflow Set      1001    V
    PL          Plus              1010    !N
    MI          Minus             1011    N
    GE          Greater or Equal  1100    N and V or !N and !V
    LT          Less Than         1101    N and !V or !N and V
    GT          Greater Than      1110    N and V and !Z or !N and !V and !Z
    LE          Less or Equal     1111    Z or N and !V or !N and V

  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    val code = genOpcode("0101____11______")
    for(condition <- 0 to 15)
      for(mode <- Seq(0,2,3,4,5,6,7))
        val regEnd = if mode == 7 then 1 else 7
        for(reg <- 0 to regEnd)
          val opcode = code | condition << 8 | mode << 3 | reg
          instructionSetHandler.registerInstruction(opcode,new Scc.Scc(ctx,opcode))
