package ucesoft.mac.cpu.m68k

object M6800X0:
  inline val DEBUG_READ_OPTION = 1 << 1

  trait EventListener:
    def rw(cpu:M6800X0,address:Int,size:Size,read:Boolean,value:Int = 0): Unit = {}
    def fetch(cpu:M6800X0,address:Int,opcode:Int,i:Instruction,busNotAvailable:Boolean = false): Unit = {}
    def interrupted(cpu:M6800X0,level:Int): Unit = {}
    def reset(cpu:M6800X0): Unit = {}
    def halt(cpu:M6800X0,halted:Boolean): Unit = {}
    def stop(cpu:M6800X0,stopped:Boolean): Unit = {}
    def exception(cpu:M6800X0,number:Int): Unit = {}

  trait BusAccessListener:
    def busAccess(address:Int,mode:BusAccessMode,cycles:Int): Unit
    
  trait ResetDeviceListener:
    def resetDevice(): Unit
    
  trait InterruptAckListener:
    def intAcknowledged(level:Int): Unit
    
  trait Context:
    def model : Model
    def adjustCycles(adjustment:Int): Unit
    def busIdle(cycles: Int): Unit
    def fetchWord(incPC:Boolean = true,clearPrefetchQueue:Boolean = false): Int
    def fetchWordForDisassembling(address: Int): Int
    def getEA(mode: Int, reg: Int, size: Size, disassemblingAddress: Option[Int] = None,includeIdleBusCycles:Boolean = true): Operand
    def getEA(addressingMode: AddressingMode, reg: Int, size: Size, disassemblingAddress: Option[Int]): Operand
    def readMemory(address: Int, size: Size,busAccess:BusAccessMode = BusAccessMode.Read): Int
    def writeMemory(address: Int, value: Int, size: Size,busAccess:BusAccessMode = BusAccessMode.Write): Unit
    def raiseException(number: Int): Unit
    def branch(pc: Int): Unit
    def getRegister(rtype:RegisterType,index:Int = -1): Register
    def getPC: Int
    def resetDevices(): Unit
    def stopCPU(): Unit
    
  case class Snapshot(regs: Map[String, Int], prefetch: List[Int]):
    override def toString: String =
      val sb = new StringBuilder()
      for (dx <- 0 to 7) {
        val v = regs(s"d$dx").toLong & 0xFFFFFFFFL
        sb.append(s"d$dx=${v.toHexString}($v) ")
      }
      for (ax <- 0 to 6) {
        val v = regs(s"a$ax").toLong & 0xFFFFFFFFL
        sb.append(s"a$ax=${v.toHexString}($v) ")
      }
      val usp = regs(s"usp").toLong & 0xFFFFFFFFL
      sb.append(s"usp=${usp.toHexString}($usp) ")
      val ssp = regs(s"ssp").toLong & 0xFFFFFFFFL
      sb.append(s"ssp=${ssp.toHexString}($ssp) ")
      val sr = regs(s"sr")
      sb.append(s"sr=${sr.toHexString}($sr) ")
      val pc = regs(s"pc").toLong & 0xFFFFFFFFL
      sb.append(s"pc=${pc.toHexString}($pc) ")
      sb.append(s"prefetch=${prefetch.map(_.toLong & 0xFFFFFFFFL).mkString("[", ",", "]")}")
      sb.toString()

  enum FunctionCode(val code:Int):
    case UserData     extends FunctionCode(1) // 001
    case UserProgram  extends FunctionCode(2) // 010
    case SuperData    extends FunctionCode(5) // 101
    case SuperProgram extends FunctionCode(6) // 110
    case InterruptAck extends FunctionCode(7) // 111

  enum BusAccessMode(val cycles:Int,val isRead:Boolean,val id:String):
    case Fetch extends BusAccessMode(4,true,"p")
    case Read extends BusAccessMode(4,true,"r")
    case Write extends BusAccessMode(4,false,"w")
    case Idle extends BusAccessMode(2,true,"n")
    case Push extends BusAccessMode(4,false,"s")
    case Pop extends BusAccessMode(4,true,"u")
    case FetchVector extends BusAccessMode(4,true,"v")
    case Reset extends BusAccessMode(4,true,"f")
    case VectorNumberAcquisition extends BusAccessMode(4,true,"i")

  case class BusTrace(address:Int,value:Int,mode:BusAccessMode,size:Size,fcode:FunctionCode):
    import BusAccessMode.*
    def toShortString: String =
      mode match
        case Idle =>
          if size == Size.Long then
            s"${mode.id}${mode.id}" else mode.id
        case _ =>
          val w = s"${Idle.id}${mode.id}"
          if size == Size.Long then s"${Idle.id}${mode.id.toUpperCase()} $w" else w

trait M6800X0:
  import M6800X0.*
  lazy val model : Model

  def getRegister(rtype:RegisterType,index:Int = -1): Register
  def reset(now:Boolean = false): Unit
  def execute(): Int
  def disassemble(address:Int): DisassembledInstruction
  def getSnapshot: Snapshot
  def setSnapshot(snap:Snapshot): Unit
  def setBusTracing(enabled:Boolean): Unit
  def getBusTracing: List[BusTrace]
  def getExceptionNumber: Option[Int]
  def setBusAccessListener(listener:BusAccessListener): Unit
  def setInterruptAckListener(listener:InterruptAckListener): Unit
  def setResetDeviceListener(resetDeviceListener: ResetDeviceListener): Unit
  def halt(asserted:Boolean): Unit
  def interrupt(ipl012:Int): Unit
  def addEventListener(el: EventListener): Unit
  def removeEventListener(el: EventListener): Unit
  def setBUSAvailable(available:Boolean): Unit
  def isBUSAvailable: Boolean
  def getLastInstructionPC : Int
  def getTotalElapsedCycles : Long
  def getLastInstructionElapsedCycles: Int
  def setDTACK(enabled:Boolean): Unit
  def addWaitCycles(waitCycles:Int): Unit
