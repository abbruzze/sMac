package ucesoft.mac.cpu.m68k.addressingModes

import ucesoft.mac.cpu.m68k.AddressingMode
import ucesoft.mac.cpu.m68k.AddressingMode.AI
import ucesoft.mac.cpu.m68k.{M6800X0, Operand, Size}

class AddressRegisterIndirectMode(override protected val ctx: M6800X0.Context) extends Operand(ctx):
  import ucesoft.mac.cpu.m68k.RegisterType.Address

  override val mode : AddressingMode = AI

  override def init(reg: Int, size: Size,disassemblingAddress:Option[Int],includeIdleBusCycles:Boolean): Unit =
    super.init(reg, size,disassemblingAddress)
    address = ctx.getRegister(Address,reg).get(Size.Long)

  override def get(size:Size,signExtended:Boolean = false): Int =
    val r = ctx.readMemory(address,size)
    if signExtended then
      extendSign(size,r)
    else
      r
  override def set(value:Int,size:Size): Unit = ctx.writeMemory(address,value,size)

  override def getMnemonic(address:Int): String = s"(a$reg)"