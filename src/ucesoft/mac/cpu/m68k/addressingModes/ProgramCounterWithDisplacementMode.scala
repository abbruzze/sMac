package ucesoft.mac.cpu.m68k.addressingModes

import ucesoft.mac.cpu.m68k.AddressingMode.DIPC
import ucesoft.mac.cpu.m68k.{AddressingMode, M6800X0, Operand, RegisterType, Size}

class ProgramCounterWithDisplacementMode(override protected val ctx: M6800X0.Context) extends Operand(ctx):
  override val mode : AddressingMode = DIPC

  private var ext,displacement = 0

  override def init(reg: Int, size: Size,disassemblingAddress:Option[Int],includeIdleBusCycles:Boolean): Unit =
    super.init(reg, size,disassemblingAddress,includeIdleBusCycles)
    address = ctx.getPC
    ext = disassemblingAddress match
      case None => ctx.fetchWord()
      case Some(address) => ctx.fetchWordForDisassembling(address + 2)
    displacement = getExtensionWordDisplacement(ext,Size.Word) // extended 32 bits
    address += displacement// + 2

  final override def get(size: Size, signExtended: Boolean = false): Int =
    val r = ctx.readMemory(address, size)
    if signExtended then
      extendSign(size, r)
    else
      r
  final override def set(value: Int, size: Size): Unit = ctx.writeMemory(address, value, size)

  override def getMnemonic(address:Int): String =
    val target = address + displacement + 2
    s"(${target.toHexString},PC)"

  override def getExtensionWords:List[Int] = List(ext & 0xFFFF)