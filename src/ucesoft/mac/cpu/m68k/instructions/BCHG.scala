package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object BCHG:
  class BCHG(override protected val ctx: M6800X0.Context,
             override protected val opcode:Int) extends Instruction(ctx,opcode):

    override val instructionType : InstructionType = InstructionType.BCHG
    private final val register = (opcode >> 9) & 7
    private final val isStatic = ((opcode >> 6) & 7) == 1

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
        BCHG, BSET    |  INSTR     EA   | 1st Operand |  2nd OP (ea)  |      INSTR
    ------------------+-----------------+-------------+---------------+------------
    Dn,<ea> :         |                 |             |               |
      .B :            |                 |             |               |
        (An)          |  8(1/1)  4(1/0) |             |            nr | np    nw
        (An)+         |  8(1/1)  4(1/0) |             |            nr | np    nw
        -(An)         |  8(1/1)  6(1/0) |             | n          nr | np    nw
        (d16,An)      |  8(1/1)  8(2/0) |             |      np    nr | np    nw
        (d8,An,Xn)    |  8(1/1) 10(2/0) |             | n    np    nr | np    nw
        (xxx).W       |  8(1/1)  8(2/0) |             |      np    nr | np    nw
        (xxx).L       |  8(1/1) 12(3/0) |             |   np np    nr | np    nw
    Dn,Dm :           |                 |             |               |
      .L :            |                 |             |               |
        if Dn<16      |  6(1/0)  0(0/0) |             |               | np       n
        if Dn>15      |  8(1/0)  0(0/0) |             |               | np       nn
    #<data>,<ea> :    |                 |             |               |
      .B :            |                 |             |               |
        (An)          | 12(2/1)  4(1/0) |          np |            nr | np    nw
        (An)+         | 12(2/1)  4(1/0) |          np |            nr | np    nw
        -(An)         | 12(2/1)  6(1/0) |          np | n          nr | np    nw
        (d16,An)      | 12(2/1)  8(2/0) |          np |      np    nr | np    nw
        (d8,An,Xn)    | 12(2/1) 10(2/0) |          np | n    np    nr | np    nw
        (xxx).W       | 12(2/1)  8(2/0) |          np |      np    nr | np    nw
        (xxx).L       | 12(2/1) 12(3/0) |          np |   np np    nr | np    nw
    #<data>,Dn :      |                 |             |               |
      .L :            |                 |             |               |
        if data<16    | 10(2/0)  0(0/0) |          np |               | np       n
        if data>15    | 12(2/0)  0(0/0) |          np |               | np       nn

    */
    final override def execute(): Unit = {
      import StatusRegister.StatusFlag.*

      var bit = if isStatic then
        val immOp = ctx.getEA(AddressingMode.IM, 0, Size.Byte, None)
        immOp.get(Size.Byte) & 31
      else
        ctx.getRegister(RegisterType.Data,register).get(Size.Byte) & 31

      var condition = false

      val eaOp = ctx.getEA(mode,reg,Size.Byte)
      if eaOp.isRegisterMode then
        val regOp = ctx.getRegister(RegisterType.Data,reg)
        condition = regOp.isBit(bit)
        regOp.invertBit(bit)
        // =============== prefetch==================
        ctx.fetchWord(false)
      // ==========================================
      else
        val regValue = eaOp.get(Size.Byte)
        bit &= 7
        condition = (regValue & (1 << bit)) != 0
        // =============== prefetch==================
        ctx.fetchWord(false)
        // ==========================================
        eaOp.set(regValue ^ (1 << bit),Size.Byte)

      if condition then
        flags.clearFlag(Z)
      else
        flags.setFlag(Z)

      if eaOp.isRegisterMode then
        if bit < 16 then ctx.busIdle(2)
        else ctx.busIdle(4)
    }

    override def disassemble(address: Int): DisassembledInstruction = {
      if isStatic then
        val immOp = ctx.getEA(AddressingMode.IM, 0, Size.Byte, Some(address))
        val eaOp = ctx.getEA(mode,reg,Size.Byte,Some(address + 2))
        DisassembledInstruction(address, opcode, s"${instructionType.mnemonic}", if isStatic then immOp.getExtensionWords else Nil, Some(s"#${immOp.get(Size.Byte) & 31}"),Some(eaOp.getMnemonic(address + 2)))
      else
        val eaOp = ctx.getEA(mode,reg,Size.Byte,Some(address))
        DisassembledInstruction(address, opcode, s"${instructionType.mnemonic}", Nil, Some(ctx.getRegister(RegisterType.Data,register).mnemonic),Some(eaOp.getMnemonic(address)))
    }

class BCHG(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     1) BCHG Dn, < ea >
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 0 | 0 | 0 | Register  | 1 | 0 | 1 |   Mode    |    Reg    |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     2) BCHG # < data > , < ea >
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 0 | 0 | 0 | 1 | 0 | 0 | 0 | 0 | 1 |   Mode    |    Reg    |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |        Bit number             |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     Size = (Byte, Long)

    X — Not affected.
    N — Not affected.
    Z — Set if the bit tested is zero; cleared otherwise.
    V — Not affected.
    C — Not affected.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    // 1)
    var code = genOpcode("0000___101______")
    for(register <- 0 to 7)
      for(mode <- Seq(0,2,3,4,5,6,7))
        val regEnd = if mode == 7 then 1 else 7
        for(reg <- 0 to regEnd)
          val opcode = code | register << 9 | mode << 3 | reg
          instructionSetHandler.registerInstruction(opcode,new BCHG.BCHG(ctx,opcode))
    // 2)
    code = genOpcode("0000100001______")
    for (mode <- Seq(0, 2, 3, 4, 5, 6, 7))
      val regEnd = if mode == 7 then 1 else 7
      for (reg <- 0 to regEnd)
        val opcode = code | mode << 3 | reg
        instructionSetHandler.registerInstruction(opcode, new BCHG.BCHG(ctx, opcode))