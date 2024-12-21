package ucesoft.mac.cpu.m68k.addressingModes

import ucesoft.mac.cpu.m68k.AddressingMode
import ucesoft.mac.cpu.m68k.AddressingMode.DN
import ucesoft.mac.cpu.m68k.{M6800X0, Operand, Size}

class DataRegisterDirectMode(override protected val ctx: M6800X0.Context) extends Operand(ctx):
  import ucesoft.mac.cpu.m68k.RegisterType.*
  override val requiresBUS : Boolean = false

  override val mode : AddressingMode = DN
  override def get(size:Size,signExtended:Boolean = false): Int = ctx.getRegister(Data,reg).get(size, signExtended)
  override def set(value:Int,size:Size): Unit = ctx.getRegister(Data,reg).set(value,size)
  final override def isRegisterMode = true

  override def getMnemonic(address:Int): String = s"d$reg"