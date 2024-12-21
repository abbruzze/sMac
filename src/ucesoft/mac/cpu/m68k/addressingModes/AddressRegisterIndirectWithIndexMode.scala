package ucesoft.mac.cpu.m68k.addressingModes

import ucesoft.mac.cpu.m68k.AddressingMode.IX
import ucesoft.mac.cpu.m68k.{AddressingMode, M6800X0, Operand, Register, Size}

import scala.compiletime.uninitialized

class AddressRegisterIndirectWithIndexMode(override protected val ctx: M6800X0.Context) extends AddressRegisterIndirectMode(ctx):
  override val mode : AddressingMode = IX

  private var ext,displacement = 0
  private var ri : Register = uninitialized

  override def init(reg: Int, size: Size,disassemblingAddress:Option[Int],includeIdleBusCycles:Boolean): Unit =
    if disassemblingAddress.isEmpty && includeIdleBusCycles then
      ctx.busIdle(2)

    super.init(reg, size,disassemblingAddress,includeIdleBusCycles:Boolean)
    ext = disassemblingAddress match
      case None => ctx.fetchWord()
      case Some(address) => ctx.fetchWordForDisassembling(address + 2)
    //checkExtension(ext)
    displacement = getExtensionWordDisplacement(ext,Size.Byte) // extended 32 bits
    val extSize = getExtensionWordSize(ext)
    val regIndex = getExtensionWordRegister(ext)
    ri = ctx.getRegister(getExtensionWordRegisterType(ext),regIndex)
    val regValue = ri.get(extSize,signExtended = true)
    //val scale = 1 << getExtensionWordScale(ext)

    address += displacement + regValue// * scale
  
  override def getMnemonic(address:Int): String = s"(${displacement.toHexString},a$reg,${ri.mnemonic})"

  override def getExtensionWords:List[Int] = List(ext & 0xFFFF)