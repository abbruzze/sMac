package ucesoft.mac.cpu.m68k.addressingModes

import ucesoft.mac.cpu.m68k.AddressingMode.AW
import ucesoft.mac.cpu.m68k.{AddressingMode, M6800X0, Operand, Size}

class AbsoluteShortMode(override protected val ctx: M6800X0.Context) extends Operand(ctx):
  override val mode : AddressingMode = AW

  private var ext1 = 0

  override def init(reg: Int, size: Size,disassemblingAddress:Option[Int],includeIdleBusCycles:Boolean): Unit =
    super.init(reg, size,disassemblingAddress)
    ext1 = disassemblingAddress match
      case None => ctx.fetchWord()
      case Some(address) => ctx.fetchWordForDisassembling(address + 2)
    address = extendSign(Size.Word,ext1)

  final override def get(size: Size, signExtended: Boolean = false): Int =
    val r = ctx.readMemory(address, size)
    if signExtended then
      extendSign(size, r)
    else
      r
  final override def set(value: Int, size: Size): Unit = ctx.writeMemory(address, value, size)
  
  override def getMnemonic(address:Int): String =
    "%04X".format(this.address)  
    s"(${this.address.toHexString}).w"

  override def getExtensionWords:List[Int] = List(ext1 & 0xFFFF)