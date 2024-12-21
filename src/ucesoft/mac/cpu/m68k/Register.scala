package ucesoft.mac.cpu.m68k

import Size.*
import ucesoft.mac.cpu.m68k.RegisterType.Address

enum RegisterType:
  case Data,Address,PC,SR,SP,USP,SSP

class Register(val rtype : RegisterType,val model:Model,val index:Int = -1):
  private var value = 0
          
  def reset(): Unit = {}
  
  def get(): Int = value

  protected def set(value:Int): Unit = this.value = value

  def get(size: Size,signExtended:Boolean = false): Int =
    var v = value & size.mask
    if signExtended then
      val msb = v & size.msb
      if msb > 0 then
        v |= ~size.mask

    /*if rtype == RegisterType.PC then
      v &= model.addressBUSMask*/

    v
  end get
    
  def set(v:Int,size: Size): Unit =
    /*
      Finally, when a word is written to an address
      register, the entire register is affected (the operand
      is size-extended to fill the register). Within a data
      register, only the operand is affected
    */
    if rtype == RegisterType.Address then
      value = v
      if size != Size.Long then
        val msb = v & size.msb
        if msb > 0 then
          value |= ~size.mask
    else
      value = (value & ~size.mask) | v & size.mask


  def increment(size: Size): Unit =
    rtype match
      case RegisterType.SP if size.bytes == 1 =>
        value += 2
      case _ =>
        value += size.bytes

  def decrement(size: Size): Unit =
    rtype match
      case RegisterType.SP if size.bytes == 1 =>
        value -= 2
      case _ =>
        value -= size.bytes

  def isBit(bit:Int): Boolean = (value & (1 << bit)) != 0
  def setBit(bit:Int,onoff:Int): Unit =
    if onoff == 0 then value &= ~(1 << bit) else value |= 1 << bit
  def invertBit(bit:Int): Unit = value ^= 1 << bit

  def swapAndGet(): Int =
    val tmp = value >>> 16
    value = (value & 0xFFFF) << 16 | tmp
    value

  override def toString: String =
    import RegisterType.*
    rtype match
      case Data =>
        s"d$index = ${value.toHexString.toUpperCase}"
      case Address =>
        s"a$index = ${value.toHexString.toUpperCase}"
      case PC =>
        s"pc = ${value.toHexString.toUpperCase}"
      case SR =>
        s"sr = ${value.toHexString.toUpperCase}"
      case SP | USP | SSP =>
        s"sp = ${value.toHexString.toUpperCase}"
        
  val mnemonic: String = rtype match
    case RegisterType.Data =>     s"d$index"
    case RegisterType.Address =>  s"a$index"
    case RegisterType.PC =>       s"pc"
    case RegisterType.SR =>       s"sr"
    case RegisterType.SP =>       s"sp"

end Register

object StatusRegister:
  /*
    +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
    |                  SR                       |      CCR          |
    | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
    +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
    | T | - | S | - | - | I2| I1| I0| - | - | - | X | N | Z | V | C |
    +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  */
  enum StatusFlag(val flag:Int):
    case C extends StatusFlag(1 << 0)
    case V extends StatusFlag(1 << 1)
    case Z extends StatusFlag(1 << 2)
    case N extends StatusFlag(1 << 3)
    case X extends StatusFlag(1 << 4)

class StatusRegister(override val model:Model) extends Register(RegisterType.SR,model):
  import StatusRegister.*

  private inline final val CCR_MASK = 0x00FF
  private inline final val INT_MASK = 7 << 8
  private inline final val TRACE_MASK = 0x8000
  private inline final val SUPER_MASK = 0x2000

  def isFlag(flag:StatusFlag): Boolean = (get() & flag.flag) > 0
  def getFlag(flag:StatusFlag): Int = (get() & flag.flag) >> flag.ordinal
  def clearFlag(flag:StatusFlag): Unit = set(get() & ~flag.flag)
  def setFlag(flag:StatusFlag): Unit = set(get() | flag.flag)

  def getCCR: Int = get() & CCR_MASK
  def setCCR(ccr:Int): Unit = set((get() & ~CCR_MASK) | (ccr & CCR_MASK))

  def getInterruptMask: Int = (get() & INT_MASK) >> 8
  def setInterruptMask(imask:Int): Unit = set((get() & ~INT_MASK) | (imask & 7) << 8)

  def isTrace: Boolean = (get() & TRACE_MASK) > 0
  def setTrace(setTrace:Boolean): Unit =
    if setTrace then
      set(get() | TRACE_MASK)
    else
      set(get() & ~TRACE_MASK)

  def isSupervisorMode: Boolean = (get() & SUPER_MASK) > 0
  def setSupervisorMode(setSM:Boolean): Unit =
    if setSM then
      set(get() | SUPER_MASK)
    else
      set(get() & ~SUPER_MASK)

  override def toString: String =
    val sb = new StringBuilder("sr=T/")
    sb.append(if isTrace then "1" else "0")
    sb.append("|S/")
    sb.append(if isSupervisorMode then "1" else "0")
    sb.append("|I2I1I0/")
    val is = getInterruptMask.toBinaryString
    sb.append("0" * (3 - is.length))
    sb.append(is)
    sb.append("|X/")
    sb.append(if isFlag(StatusFlag.X) then "1" else "0")
    sb.append("|N/")
    sb.append(if isFlag(StatusFlag.N) then "1" else "0")
    sb.append("|Z/")
    sb.append(if isFlag(StatusFlag.Z) then "1" else "0")
    sb.append("|V/")
    sb.append(if isFlag(StatusFlag.V) then "1" else "0")
    sb.append("|C/")
    sb.append(if isFlag(StatusFlag.C) then "1" else "0")
    sb.toString()