package ucesoft.mac.cpu.m68k

abstract class InstructionGenerator:
  def generate(instructionSetHandler:InstructionSet): Unit

  protected def genOpcode(mask:String): Int =
    var bit = 1
    var opcode = 0
    var bitCount = 0
    for(c <- mask.reverse)
      c match
        case '0'|'_' =>
          bit <<= 1
          bitCount += 1
        case '1' =>
          opcode |= bit
          bit <<= 1
          bitCount += 1
        case ' ' =>

    if bitCount != 16 then
      throw new IllegalArgumentException("Opcode mask must contain 16 bits")

    opcode
