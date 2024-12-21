package ucesoft.mac.cpu.m68k.addressingModes

import ucesoft.mac.cpu.m68k.AddressingMode
import ucesoft.mac.cpu.m68k.AddressingMode.PI
import ucesoft.mac.cpu.m68k.{M6800X0, Operand, Size}

class AddressRegisterIndirectPostIncrementMode(override protected val ctx: M6800X0.Context) extends AddressRegisterIndirectMode(ctx):
  import ucesoft.mac.cpu.m68k.RegisterType.Address

  override val mode : AddressingMode = PI

  override def init(reg: Int, size: Size,disassemblingAddress:Option[Int],includeIdleBusCycles:Boolean): Unit =
    super.init(reg, size,disassemblingAddress,includeIdleBusCycles)
    // post increment
    if disassemblingAddress.isEmpty then
      ctx.getRegister(Address,reg).increment(size)

  override def getMnemonic(address:Int): String = s"(a$reg)+"