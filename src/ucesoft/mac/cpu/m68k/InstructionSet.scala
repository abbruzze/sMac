package ucesoft.mac.cpu.m68k

trait InstructionSet:
  def registerInstruction(opcode:Int,i:Instruction): Unit
