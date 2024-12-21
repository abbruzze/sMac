package ucesoft.mac.cpu.m68k

abstract class Operand(protected val ctx: M6800X0.Context):
  val mode : AddressingMode
  val requiresBUS : Boolean = true
  protected var reg = 0
  protected var address = 0
  protected var size : Size = scala.compiletime.uninitialized
  
  def init(reg:Int,size: Size,disassemblingAddress:Option[Int] = None,includeIdleBusCycles:Boolean = true): Unit =
    this.reg = reg
    this.size = size

  def get(size:Size,signExtended:Boolean = false): Int
  def set(value:Int,size:Size): Unit

  def isRegisterMode: Boolean = false

  def getAddress: Int = address
  
  def getExtensionWords:List[Int] = Nil
  
  def getMnemonic(address:Int): String

  protected def extendSign(size:Size,value:Int):Int =
    size match
      case Size.Byte =>
        if (value & 0x80) > 0 then
          0xFFFFFF00 | value & 0xFF
        else
          value & 0xFF
      case Size.Word =>
        if (value & 0x8000) > 0 then
          0xFFFF0000 | value & 0xFFFF
        else
          value & 0xFFFF
      case Size.Long =>
        value

  // extension word utilities
  protected def getExtensionWordDisplacement(word:Int,size:Size): Int = extendSign(size,word & size.mask)
  protected def getExtensionWordScale(word:Int): Int = (word >> 9) & 3
  protected def getExtensionWordSize(word:Int): Size = if ((word >> 11) & 1) == 0 then Size.Word else Size.Long
  protected def getExtensionWordRegister(word:Int):Int = (word >> 12) & 7
  protected def getExtensionWordRegisterType(word:Int): RegisterType = if (word & 0x8000) == 0 then RegisterType.Data else RegisterType.Address