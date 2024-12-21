package ucesoft.mac.cpu.m68k.addressingModes

import ucesoft.mac.cpu.m68k.AddressingMode.IXPC
import ucesoft.mac.cpu.m68k.{AddressingMode, M6800X0, Operand, Register, RegisterType, Size}

import scala.compiletime.uninitialized

class ProgramCounterWithIndexMode(override protected val ctx: M6800X0.Context) extends Operand(ctx):
  override val mode : AddressingMode = IXPC

  private var ext,displacement = 0
  private var ri : Register = uninitialized

  override def init(reg: Int, size: Size,disassemblingAddress:Option[Int],includeIdleBusCycles:Boolean): Unit =
    if disassemblingAddress.isEmpty && includeIdleBusCycles then
      ctx.busIdle(2)

    super.init(reg, size,disassemblingAddress,includeIdleBusCycles)
    address = ctx.getPC
    ext = disassemblingAddress match
      case None => ctx.fetchWord()
      case Some(address) => ctx.fetchWordForDisassembling(address + 2)
    displacement = getExtensionWordDisplacement(ext,Size.Byte) // extended 32 bits
    val extSize = getExtensionWordSize(ext)
    val regIndex = getExtensionWordRegister(ext)
    ri = ctx.getRegister(getExtensionWordRegisterType(ext), regIndex)
    val regValue = ri.get(extSize, signExtended = true)
    //val scale = 1 << getExtensionWordScale(ext)

    address += displacement + regValue// + 2//* scale

  final override def get(size: Size, signExtended: Boolean = false): Int =
    val r = ctx.readMemory(address, size)
    if signExtended then
      extendSign(size, r)
    else
      r
  final override def set(value: Int, size: Size): Unit = ctx.writeMemory(address, value, size)

  override def getMnemonic(address:Int): String =
    val target = address + displacement + 2
    s"(${target.toHexString},PC,${ri.mnemonic})"

  override def getExtensionWords:List[Int] = List(ext & 0xFFFF)