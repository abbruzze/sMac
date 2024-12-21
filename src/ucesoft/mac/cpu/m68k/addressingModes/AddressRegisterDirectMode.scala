package ucesoft.mac.cpu.m68k.addressingModes

import ucesoft.mac.cpu.m68k.AddressingMode
import ucesoft.mac.cpu.m68k.AddressingMode.AN
import ucesoft.mac.cpu.m68k.{M6800X0, Operand, Size}

class AddressRegisterDirectMode(override protected val ctx: M6800X0.Context) extends DataRegisterDirectMode(ctx):
  import ucesoft.mac.cpu.m68k.RegisterType.Address
  override val requiresBUS : Boolean = false

  override val mode : AddressingMode = AN
  final override def get(size:Size,signExtended:Boolean = false): Int = ctx.getRegister(Address,reg).get(size, signExtended)
  final override def set(value:Int,size:Size): Unit = ctx.getRegister(Address,reg).set(value,size)

  override def getMnemonic(address:Int): String = s"a$reg"