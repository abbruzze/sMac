package ucesoft.mac.cpu.m68k.addressingModes

import ucesoft.mac.cpu.m68k.AddressingMode.DI
import ucesoft.mac.cpu.m68k.{AddressingMode, M6800X0, Operand, Size}

class AddressRegisterIndirectWithDisplacementMode(override protected val ctx: M6800X0.Context) extends AddressRegisterIndirectMode(ctx):
  override val mode : AddressingMode = DI

  private var ext,displacement = 0

  override def init(reg: Int, size: Size,disassemblingAddress:Option[Int],includeIdleBusCycles:Boolean): Unit =
    super.init(reg, size,disassemblingAddress,includeIdleBusCycles)
    ext = disassemblingAddress match
      case None => ctx.fetchWord()
      case Some(address) => ctx.fetchWordForDisassembling(address + 2)
    displacement = getExtensionWordDisplacement(ext,Size.Word) // extended 32 bits
    address += displacement
  
  override def getMnemonic(address:Int): String = s"(${displacement.toHexString},a$reg)"

  override def getExtensionWords:List[Int] = List(ext & 0xFFFF)