package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object BTST:
  class BTST(override protected val ctx: M6800X0.Context,
             override protected val opcode:Int) extends Instruction(ctx,opcode):

    override val instructionType : InstructionType = InstructionType.BTST
    private final val register = (opcode >> 9) & 7
    private final val isStatic = ((opcode >> 6) & 7) == 0

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
           BTST       |  INSTR     EA   | 1st Operand |  2nd OP (ea)  |   INSTR
    ------------------+-----------------+-------------+---------------+------------
    Dn,<ea> :         |                 |             |               |
      .B :            |                 |             |               |
        (An)          |  4(1/0)  4(1/0) |             |            nr | np
        (An)+         |  4(1/0)  4(1/0) |             |            nr | np
        -(An)         |  4(1/0)  6(1/0) |             | n          nr | np
        (d16,An)      |  4(1/0)  8(2/0) |             |      np    nr | np
        (d8,An,Xn)    |  4(1/0) 10(2/0) |             | n    np    nr | np
        (xxx).W       |  4(1/0)  8(2/0) |             |      np    nr | np
        (xxx).L       |  4(1/0) 12(3/0) |             |   np np    nr | np
        #<data>       |  6(1/0)  4(1/0) |             |      np       | np n
    Dn,Dm :           |                 |             |               |
      .L :            |  6(1/0)  0(0/0) |             |               | np n
    #<data>,<ea> :    |                 |             |               |
      .B :            |                 |             |               |
        (An)          |  8(2/0)  4(1/0) |          np |            nr | np
        (An)+         |  8(2/0)  4(1/0) |          np |            nr | np
        -(An)         |  8(2/0)  6(1/0) |          np | n          nr | np
        (d16,An)      |  8(2/0)  8(2/0) |          np |      np    nr | np
        (d8,An,Xn)    |  8(2/0) 10(2/0) |          np | n    np    nr | np
        (xxx).W       |  8(2/0)  8(2/0) |          np |      np    nr | np
        (xxx).L       |  8(2/0) 12(3/0) |          np |   np np    nr | np
    #<data>,Dn :      |                 |             |               |
      .L :            | 10(2/0)  0(0/0) |          np |               | np n

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
      else
        val regValue = eaOp.get(Size.Byte)
        bit &= 7
        condition = (regValue & (1 << bit)) != 0

      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================

      if condition then
        flags.clearFlag(Z)
      else
        flags.setFlag(Z)

      if eaOp.isRegisterMode || eaOp.mode == AddressingMode.IM then
        ctx.busIdle(2)
    }

    override def disassemble(address: Int): DisassembledInstruction = {
      if isStatic then
        val immOp = ctx.getEA(AddressingMode.IM, 0, Size.Byte, Some(address))
        val eaOp = ctx.getEA(mode,reg,Size.Byte,Some(address + 2))
        DisassembledInstruction(address, opcode, s"${instructionType.mnemonic}", immOp.getExtensionWords ++ eaOp.getExtensionWords, Some(s"#${immOp.get(Size.Byte) & 31}"),Some(eaOp.getMnemonic(address + 2)))
      else
        val eaOp = ctx.getEA(mode,reg,Size.Byte,Some(address))
        DisassembledInstruction(address, opcode, s"${instructionType.mnemonic}", eaOp.getExtensionWords, Some(ctx.getRegister(RegisterType.Data,register).mnemonic),Some(eaOp.getMnemonic(address)))
    }

class BTST(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     1) BTST Dn, < ea >
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 0 | 0 | 0 | Register  | 1 | 0 | 0 |   Mode    |    Reg    |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     2) BTST # < data > , < ea >
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 0 | 0 | 0 | 1 | 0 | 0 | 0 | 0 | 0 |   Mode    |    Reg    |
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
    var code = genOpcode("0000___100______")
    for(register <- 0 to 7)
      for(mode <- Seq(0,2,3,4,5,6,7))
        val regEnd = if mode == 7 then 4 else 7
        for(reg <- 0 to regEnd)
          val opcode = code | register << 9 | mode << 3 | reg
          instructionSetHandler.registerInstruction(opcode,new BTST.BTST(ctx,opcode))
    // 2)
    code = genOpcode("0000100000______")
    for (mode <- Seq(0, 2, 3, 4, 5, 6, 7))
      val regEnd = if mode == 7 then 3 else 7
      for (reg <- 0 to regEnd)
        val opcode = code | mode << 3 | reg
        instructionSetHandler.registerInstruction(opcode, new BTST.BTST(ctx, opcode))