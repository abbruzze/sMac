package ucesoft.mac.cpu.m68k.addressingModes

import ucesoft.mac.cpu.m68k.AddressingMode.IM
import ucesoft.mac.cpu.m68k.{AddressingMode, M6800X0, Operand, Size}

class ImmediateMode(override protected val ctx: M6800X0.Context) extends Operand(ctx):
  override val mode : AddressingMode = IM

  private var immediateValue = 0
  private var ext1,ext2 = 0

  override def init(reg: Int, size: Size,disassemblingAddress:Option[Int],includeIdleBusCycles:Boolean): Unit =
    super.init(reg, size,disassemblingAddress,includeIdleBusCycles)
    ext1 = disassemblingAddress match
      case None => ctx.fetchWord()
      case Some(address) => ctx.fetchWordForDisassembling(address + 2)
    if size == Size.Long then
      ext2 = disassemblingAddress match
        case None => ctx.fetchWord()
        case Some(address) => ctx.fetchWordForDisassembling(address + 4)
      immediateValue = ext1 << 16 | ext2
    else
      immediateValue = ext1 & size.mask


  final override def get(size:Size,signExtended:Boolean = false): Int =
    val r = size match
      case Size.Byte =>
        immediateValue & 0xFF
      case Size.Word =>
        immediateValue & 0xFFFF
      case Size.Long =>
        immediateValue
        
    if signExtended then
      extendSign(size,r)
    else
      r

  final override def set(value:Int,size:Size): Unit =
    throw new IllegalArgumentException(s"Mode $mode: cannot write on immediate value")
  
  override def getMnemonic(address:Int): String = s"#${immediateValue.toHexString}"

  override def getExtensionWords:List[Int] = if size == Size.Long then List(ext1,ext2) else List(ext1)