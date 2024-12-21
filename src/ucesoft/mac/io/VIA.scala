package ucesoft.mac.io

import ucesoft.mac.MACComponent


abstract class VIA(val irqAction:Boolean => Unit) extends MACComponent {
  protected final val regs = Array.ofDim[Int](16)
  private var paLatch = 0
  private var pbLatch = 0
  private var t2ll 	= 0		// T2 low order latch
  private var PB7 	= 0		// 7th bit of PB set by Timer 1
  private var pending_t1,pending_t2,reload_t1,reload_t2 = false
  private var oneshotB,oneshotBNew,acrNew = false
  protected inline val PB 	= 0x00	// Port B
  protected inline val PA 	= 0x01	// Port A
  protected inline val DDRB 	= 0x02  // Data Direction Register B
  protected inline val DDRA 	= 0x03  // Data Direction Register A
  protected inline val T1LC 	= 0x04  // Timer 1 Low Counter
  protected inline val T1HC 	= 0x05	// Timer 1 High Counter
  protected inline val T1LL 	= 0x06	// Timer 1 Low Latch
  protected inline val T1HL 	= 0x07	// Timer 1 High Latch
  protected inline val T2LC 	= 0x08	// Timer 2 Low Counter
  protected inline val T2HC 	= 0x09	// Timer 2 High Counter
  protected inline val SR	= 0x0A	// Shift Register
  protected inline val ACR 	= 0x0B	// Auxiliary Control Register
  protected inline val PCR 	= 0x0C	// Peripheral Control Register
  protected inline val IFR 	= 0x0D	// Interrupt Flag Register
  protected inline val IER 	= 0x0E	// Interrupt Enable Register
  protected inline val PA2	= 0x0F
  
  protected inline val IRQ_CA2 	= 0x01
  protected inline val IRQ_CA1 	= 0x02
  protected inline val IRQ_SR 	= 0x04
  protected inline val IRQ_CB2 	= 0x08
  protected inline val IRQ_CB1 	= 0x10
  private inline val IRQ_TIMER_2 = 0x020
  private inline val IRQ_TIMER_1 = 0x040
  
  private inline val PA_LATCH_ENABLED = 0x01
  private inline val PB_LATCH_ENABLED = 0x02

  // handshaking lines
  protected var lastCA1,lastCB1,lastCA2,lastCB2 = false
  // SR
  protected var srStarted,srLoaded = false
  protected var srCounter,srStartDelay = 0

  override def getProperties: List[MACComponent.Property] =
    import MACComponent.Property
    List(
      Property("DDRA","%02X".format(regs(DDRA))),
      Property("PA","%02X".format(regs(PA))),
      Property("DDRB","%02X".format(regs(DDRB))),
      Property("PB","%02X".format(regs(PB))),
      Property("T1LC","%02X".format(regs(T1LC))),
      Property("T1HC","%02X".format(regs(T1HC))),
      Property("T1LL","%02X".format(regs(T1LL))),
      Property("T1HL","%02X".format(regs(T1HL))),
      Property("T2LC","%02X".format(regs(T2LC))),
      Property("T2HC","%02X".format(regs(T2HC))),
      Property("SR","%02X".format(regs(SR))),
      Property("ACR","%02X".format(regs(ACR))),
      Property("PCR","%02X".format(regs(PCR))),
      Property("IFR","%02X".format(regs(IFR))),
      Property("IER","%02X".format(regs(IER))),
    )

  override def init() : Unit = {
    initCA1()
    initCA2()
    initCB1()
    initCB2()
  }

  override protected def reset() : Unit = {
    java.util.Arrays.fill(regs,0)
    paLatch = 0
    pbLatch = 0
    t2ll = 0
    PB7 = 0
    pending_t1 = false
    pending_t2 = false
    reload_t1 = false
    reload_t2 = false
    oneshotB = false
    oneshotBNew = true // because ACR5 = 0 => oneshot
    acrNew = true
    srStarted = false
    srLoaded = false
    srCounter = 0
    srStartDelay = 0

    initCA1()
    initCA2()
    initCB1()
    initCB2()
  }
  
  final def irq_set(irq:Int) : Unit = {
    regs(IFR) |= irq //| 0x80	// enable irq bit + 7th bit
    checkIRQ()
  }
  final def irq_clr(irq:Int) : Unit = {
    regs(IFR) &= ~irq & 0x7F
    //if ((regs(IFR) & 0x7F) == 0) regs(IFR) = 0 // if no more irq are set clear 7th bit
    checkIRQ()
  }
  inline private def checkIRQ() : Unit ={
    val irq = (regs(IFR) & regs(IER)) > 0
    if (irq) regs(IFR) |= 0x80 else regs(IFR) &= 0x7F
    irqAction(irq)
  }
  
  inline private def is_set(reg:Int,bits:Int) = (regs(reg) & bits) > 0

  protected def srMode: Int = (regs(ACR) >> 2) & 7

  protected def addPB7(value:Int):Int = if (is_set(ACR,0x80)) (value & 0x7F) | PB7 else value

  /*
   * Ignores DDRA & DDRB. Subclasses are in charge for this check
   */
  def read(address: Int): Int = address & 0x0F match {
    case PA =>
      irq_clr(IRQ_CA1)
      val PCR_CA2_CTRL = (regs(PCR) >> 1) & 7
      if (PCR_CA2_CTRL != 1 && PCR_CA2_CTRL != 3) irq_clr(IRQ_CA2) // check for independent interrupt mode
      log.info("VIA Cleared IRQ_CA1: IFR=%s IER=%s",Integer.toBinaryString(regs(IFR)),Integer.toBinaryString(regs(IER)))
      if (is_set(ACR,PA_LATCH_ENABLED)) paLatch else regs(PA) // & ~regs(DDRA)
    case PA2 =>
      if (is_set(ACR,PA_LATCH_ENABLED)) paLatch else regs(PA) // & ~regs(DDRA)
    case PB =>
      irq_clr(IRQ_CB1)
      val PCR_CB2_CTRL = (regs(PCR) >> 5) & 7
      if (PCR_CB2_CTRL != 1 && PCR_CB2_CTRL != 3) irq_clr(IRQ_CB2) // check for independent interrupt mode
      val pb7 = if (is_set(ACR,0x80)) PB7 else 0
      (if (is_set(ACR,PB_LATCH_ENABLED)) pbLatch else regs(PB)) | pb7 //& (~regs(DDRB) | (if (is_set(ACR,0x80)) 0x80 else 0))
    case SR =>
      irq_clr(IRQ_SR)
      checkSR()
      regs(SR)
    case T1LC =>
      irq_clr(IRQ_TIMER_1)
      regs(T1LC)
    case T2LC =>
      irq_clr(IRQ_TIMER_2)
      regs(T2LC)
    case IER =>
      regs(IER) | 0x80
    case ofs =>
      regs(ofs)
  }
  
  /*
   * Ignores DDRA & DDRB. Subclasses are in charge for this check
   */
  def write(address: Int, value: Int): Unit = address & 0x0F match {
    case DDRA =>
      regs(DDRA) = value
      write(PA,(regs(PA) | ~value) & 0xFF)
    case DDRB =>
      regs(DDRB) = value
      write(PB,(regs(PB) | ~value) & 0xFF)
    case PA =>
      irq_clr(IRQ_CA1)
      val PCR_CA2_CTRL = (regs(PCR) >> 1) & 7
      if (PCR_CA2_CTRL != 1 && PCR_CA2_CTRL != 3) irq_clr(IRQ_CA2) // check for independent interrupt mode
      regs(PA) = value
    case PA2 =>
      regs(PA) = value
    case PB =>
      irq_clr(IRQ_CB1)
      val PCR_CB2_CTRL = (regs(PCR) >> 5) & 7
      if (PCR_CB2_CTRL != 1 && PCR_CB2_CTRL != 3) irq_clr(IRQ_CB2) // check for independent interrupt mode
      regs(PB) = value
    case T1LC =>
      regs(T1LL) = value
      log.info("VIA write to T1LC => T1LL=%02X",regs(T1LL))
    case T1HC =>
      regs(T1HL) = value
      regs(T1HC) = value
      regs(T1LC) = regs(T1LL)
      pending_t1 = true
      reload_t1 = true
      irq_clr(IRQ_TIMER_1)
      if (is_set(ACR,0x80)) PB7 = 0x00
      log.info("VIA write T1HC => T1HL=%02X T1HC=%02X T1LC=%02X PB7=%d",regs(T1HL),regs(T1HC),regs(T1LC),PB7)
    case T1LL =>
      regs(T1LL) = value
      log.info("VIA write T1LL => T1LL=%02X",regs(T1LL))
    case T1HL =>
      regs(T1HL) = value
      irq_clr(IRQ_TIMER_1)
      log.info("VIA write T1HL => T1HL=%02X",regs(T1HL))
    case T2LC =>
      t2ll = value
      log.info("VIA writing T2LC => t2ll=%02X",t2ll)
    case T2HC =>
      regs(T2HC) = value
      regs(T2LC) = t2ll
      irq_clr(IRQ_TIMER_2)
      pending_t2 = true
      reload_t2 = true
      log.info("VIA writing T2HC => T2LC=%02X T2HC=%02X",regs(T2LC),regs(T2HC))
    case SR =>
      regs(SR) = value
      irq_clr(IRQ_SR)
      checkSR()
      log.info("VIA writing SR => SR=%02X",regs(SR))
    case IFR =>
      regs(IFR) &= ~value
      log.info("VIA writing IFR => IFR=%02X",regs(IFR))
      checkIRQ()
    case IER =>
      if ((value & 0x80) > 0) regs(IER) |= value & 0x7F else regs(IER) &= ~value
      log.info("VIA writing IER => IER=%02X",regs(IER))
      checkIRQ()
    case ACR =>
      acrNew = true
      regs(ACR) = value
      oneshotBNew = !is_set(ACR,0x20)
      //if (!is_set(ACR,0x80)) PB7 = 0x00
      PB7 = if (is_set(ACR,0x80)) 0x80 else 0x00

      checkSR()
    case PCR =>
      log.info("VIA writing PCR %02X",value)
      regs(PCR) = value
      checkPCR()
    case ofs => 
      regs(ofs) = value
      log.info("VIA writing reg %02X",ofs)
  }

  protected def checkSR(): Unit = {
    if (!srStarted && srMode != 0) {
      srStarted = true
      srCounter = 0
      srStartDelay = 0
      srMode match {
        case 1 =>
          incT2(2)
        case 2 =>
          srStartDelay = 5
        case 4 =>
          incT2(2)
        case 5 =>
          incT2(2)
          srLoaded = true
        case 6 =>
          srStartDelay = 5
        case _ =>
      }
    }
  }

  inline private def incT2(value:Int): Unit = {
    regs(T2LC) += value
    if (regs(T2LC) > 0xFF) {
      regs(T2LC) &= 0xFF
      regs(T2HC) = (regs(T2HC) + 1) & 0xFF
    }
  }

  // ============== Handshaking lines ==============================================

  protected def checkPCR(): Unit = {
    //println(s"VIA($name) PCR set to ${regs(PCR)}")
    // CA2
    (regs(PCR) >> 1) & 7 match {
      case 6 /*110 Manual output mode: CA2 = low */ => CA2Out(false)
      case 7 /*111 Manual output mode: CA2 = high */ => CA2Out(true)
      case _ =>
    }
    // CB2
    (regs(PCR) >> 5) & 7 match {
      case 6 /*110 Manual output mode: CA2 = low */ => CB2Out(false)
      case 7 /*111 Manual output mode: CA2 = high */ => CB2Out(true)
      case _ =>
    }
  }

  protected def initCA1(): Unit = lastCA1 = false
  protected def initCA2(): Unit = lastCA2 = false
  protected def initCB1(): Unit = lastCB1 = false
  protected def initCB2(): Unit = lastCB2 = false

  protected def CA2Out(state:Boolean): Unit = {}
  protected def CB2Out(state:Boolean): Unit = {}
  def CA1In(state:Boolean): Unit = {
    val ca1HighToLow = (regs(PCR) & 1) == 0
    if (ca1HighToLow) {
      if (lastCA1 && !state) irq_set(IRQ_CA1)
    }
    else {
      if (!lastCA1 && state) irq_set(IRQ_CA1)
    }
    lastCA1 = state
  }
  def CB1In(state: Boolean): Unit = {
    val cb1HighToLow = (regs(PCR) & 0x10) == 0
    if (cb1HighToLow) {
      if (lastCB1 && !state)
        irq_set(IRQ_CB1)
    }
    else {
      if (!lastCB1 && state)
        irq_set(IRQ_CB1)
    }
    lastCB1 = state
  }
  def CA2In(state:Boolean): Unit = {
    (regs(PCR) >> 1) & 7 match {
      case 0 | 1 =>
        if (lastCA2 && !state)
          irq_set(IRQ_CA2)
      case 2 | 3 =>
        if (!lastCA2 && state) {
          irq_set(IRQ_CA2)
        }
      case _ =>
    }
    lastCA2 = state
  }

  def CB2In(state: Boolean): Unit = {
    (regs(PCR) >> 5) & 7 match {
      case 0 | 1 =>
        if (lastCB2 && !state) irq_set(IRQ_CB2)
      case 2 | 3 =>
        if (!lastCB2 && state) irq_set(IRQ_CB2)
      case _ =>
    }
    lastCB2 = state
  }
  // ===============================================================================
  
  def clock(cycles:Long) : Unit = {
      updateT1()
      updateT2()

      if (acrNew) {
        acrNew = false
        oneshotB = oneshotBNew
      }

      if (srStarted) {
        srMode match {
          case 2 =>
            if (srStartDelay == 0) shiftSR(in = true)
            else {
              srStartDelay -= 1
              if (srStartDelay == 0) {
                CA2In(true)
                CA2In(false)
              }
            }
          case 6 =>
            if (srStartDelay == 0) shiftSR(in = false)
            else srStartDelay -= 1
          case _ =>
        }
      }
  }

  inline private def updateT1(): Unit = {
    var counter = 0
    if (reload_t1) {
      counter = regs(T1LL) | regs(T1HL) << 8
      reload_t1 = false
    }
    else {
      counter = regs(T1LC) | regs(T1HC) << 8
      counter = (counter - 1) & 0xFFFF
      reload_t1 = counter == 0xFFFF
      val timeout_t1 = pending_t1 && reload_t1
      if (timeout_t1 && is_set(ACR,0x80)) {
        if (is_set(ACR,0x40)) PB7 ^= 0x80
        else PB7 = 0x80
      }
      if (timeout_t1) {
        irq_set(IRQ_TIMER_1)
        pending_t1 = is_set(ACR,0x40)
      }
    }
    regs(T1LC) = counter & 0xFF
    regs(T1HC) = (counter >> 8) & 0xFF
  }

  inline private def updateT2(): Unit = {
    var counter = 0
    if (reload_t2) {
      counter = t2ll | regs(T2HC) << 8

      reload_t2 = false
      regs(T2LC) = counter & 0xFF
      regs(T2HC) = (counter >> 8) & 0xFF
    }
    else if (oneshotB) { // DO NOT count pulses on PB6
      counter = regs(T2LC) | regs(T2HC) << 8
      counter = (counter - 1) & 0xFFFF
      val timeout_t2 = (pending_t2 || srMode == 2) && counter == 0xFFFF
      if (timeout_t2) {
        irq_set(IRQ_TIMER_2)
        pending_t2 = false
      }
      if (srStarted && (counter & 0xFF) == 0xFF) {
        srMode match {
          case 1 =>
            shiftSR(in = true)
            reload_t2 = true
          case 4|5 =>
            shiftSR(in = false)
            reload_t2 = true
          case _ =>
        }
      }

      regs(T2LC) = counter & 0xFF
      regs(T2HC) = (counter >> 8) & 0xFF
    }
  }

  protected def shiftSR(in:Boolean): Unit = {
    if (in) {
      srCounter += 1
      if ((srCounter & 1) == 0) {
        regs(SR) = (regs(SR) << 1 | 1) & 0xFF
        CB1In(true)
        CB2In(true)
        CB2In(false)
      }
      else CB1In(false)

      if (srCounter == 16) {
        srCounter = 0
        srStarted = false
        irq_set(IRQ_SR)
      }
    }
    else {
      srMode match {
        case 4 =>
          srCounter = (srCounter + 1) & 1
          if (srCounter == 1) {
            regs(SR) <<= 1
            if ((regs(SR) & 0x100) == 0x100) regs(SR) |= 1
            regs(SR) &= 0xFF
            CA2In((regs(SR) & 0x80) > 0)
          }
        case 5|6 =>
          srCounter += 1
          if ((srCounter & 1) == 1) {
            regs(SR) <<= 1
            if ((regs(SR) & 0x100) == 0x100) regs(SR) |= 1
            regs(SR) &= 0xFF
            CB1In(true)
          }
          else {
            CA2In((regs(SR) & 0x80) > 0)
            CB1In(false)
          }

          if (srCounter == 16) {
            srCounter = 0
            srStarted = srLoaded
            srLoaded = false
            irq_set(IRQ_SR)
          }
        case _ =>
      }
    }
  }

  /*
  protected def saveState(out:ObjectOutputStream) : Unit = {
    out.writeInt(paLatch)
    out.writeInt(pbLatch)
    out.writeInt(t2ll)
    out.writeInt(PB7)
    out.writeBoolean(active)
    out.writeObject(regs)
    out.writeBoolean(reload_t1)
    out.writeBoolean(reload_t2)
    out.writeBoolean(pending_t1)
    out.writeBoolean(pending_t1)
    out.writeBoolean(oneshotB)
    out.writeBoolean(oneshotBNew)
    out.writeBoolean(acrNew)
  }
  protected def loadState(in:ObjectInputStream) : Unit = {
    paLatch = in.readInt
    pbLatch = in.readInt
    t2ll = in.readInt
    PB7 = in.readInt
    active = in.readBoolean
    loadMemory[Int](regs,in)
    reload_t1 = in.readBoolean
    reload_t2 = in.readBoolean
    pending_t1 = in.readBoolean
    pending_t2 = in.readBoolean
    oneshotB = in.readBoolean
    oneshotBNew = in.readBoolean
    acrNew = in.readBoolean
  }
   */
}