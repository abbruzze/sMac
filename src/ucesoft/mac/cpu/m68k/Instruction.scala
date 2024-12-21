package ucesoft.mac.cpu.m68k

abstract class Instruction(protected val ctx: M6800X0.Context, protected val opcode:Int):
  val instructionType : InstructionType
  val isBranch = false

  protected final val mode = (opcode >> 3) & 7
  protected final val reg = opcode & 7

  protected final val flags = ctx.getRegister(RegisterType.SR).asInstanceOf[StatusRegister]
  
  def execute(): Unit
  def disassemble(address: Int): DisassembledInstruction

  protected def extendSign(size: Size, value: Int): Int =
    size match
      case Size.Byte =>
        if (value & 0x80) > 0 then
          0xFFFFFF00 | value & 0xFF
        else
          value & 0xFF
      case Size.Word =>
        if (value & 0x8000) > 0 then
          0xFFFF0000 | value & 0xFFFF
        else
          value & 0xFFFF
      case Size.Long =>
        value
