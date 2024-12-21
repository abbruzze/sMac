package ucesoft.mac.cpu.m68k.addressingModes

import ucesoft.mac.cpu.m68k.AddressingMode.PD
import ucesoft.mac.cpu.m68k.{AddressingMode, M6800X0, Operand, Size}

class AddressRegisterIndirectPreDecrementMode(override protected val ctx: M6800X0.Context) extends AddressRegisterIndirectMode(ctx):
  import ucesoft.mac.cpu.m68k.RegisterType.Address

  override val mode : AddressingMode = PD

  override def init(reg: Int, size: Size,disassemblingAddress:Option[Int],includeIdleBusCycles:Boolean): Unit =
    if disassemblingAddress.isEmpty && includeIdleBusCycles then
      ctx.busIdle(2)
    // pre decrement
    if disassemblingAddress.isEmpty then
      ctx.getRegister(Address, reg).decrement(size)
    super.init(reg, size,disassemblingAddress,includeIdleBusCycles)
  
  override def getMnemonic(address:Int): String = s"-(a$reg)"