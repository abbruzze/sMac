package ucesoft.mac.serial

import ucesoft.mac.MACComponent

import javax.swing.ImageIcon
import scala.collection.mutable.ListBuffer

/**
 * @author Alessandro Abbruzzetti
 *         Created on 14/11/2024 19:53  
 */
class Z8530(irqLow: Boolean => Unit) extends MACComponent:
  override protected val componentName = "Zilog 8530"
  override protected val icon = new ImageIcon(getClass.getResource("/resources/trace/serial.png"))
  
  private class Channel(channel:Int):
    private var data = 0x00
    private var DCD = false
    private var interruptMask = 0
    private var externalInterruptMask = 0
    private var externalInterruptStatus = 0
    
    def reset(): Unit =
      data = 0x00
      DCD = false
      interruptMask = 0
      externalInterruptMask = 0
      externalInterruptStatus = 0

    def getDCD: Boolean = DCD
    def getInterruptMask: Int = interruptMask
    def getExternalInterruptMask: Int = externalInterruptMask
    def getExternalInterruptStatus: Int = externalInterruptStatus

    def setDCD(dcd:Boolean): Unit =
      if dcd != DCD then
        DCD = dcd
        if (externalInterruptMask & 0x8) != 0 then
          externalInterruptStatus |= 0x8
    def getData: Int = data
    def setData(data:Int): Unit = this.data = data

    def hasInterrupt: Boolean = (interruptMask & 1) != 0 && externalInterruptStatus != 0
    def clearInt(): Unit =
      externalInterruptStatus = 0

    /*
    The Interrupt Enable (IE) bits control interrupt requests
    from each interrupt source on the SCC. If the IE bit is set
    to 1 for an interrupt source, that source may generate an
    interrupt request, providing all of the necessary conditions
    are met. If the IE bit is reset, no interrupt request is generated by that source. The transmit interrupt IE bit is WR1
    D1. The receive interrupt IE bits are WR1 D3 and D4. The
    external status interrupts are individually enabled in WR15
    with the master external status interrupt enable in WR1
    D0. Reminder: The MIE bit, WR9 D3, must be set for any interrupt to occur.
    */
    def write(reg:Int,value:Int): Unit =
      reg match
        case 0x0 => // WR0
          // D7 D6 D5 D4 D3 D2 D1 D0
          // D2 D1 D0 => register selection
          // D5 D4 D3
          // 0  0  0 NULL CODE
          // 0  0  1 POINT HIGH
          // 0  1  0 RESET EXT/STAT INTERRUPTS
          // 0  1  1 SEND ABORT
          // 1  0  0 ENABLE INT ON NEXT Rx CHARACTER
          // 1  0  1 RESET TxINT PENDING
          // 1  1  0 ERROR RESET
          // 1  1  1 RESET HIGHEST IUS
          // D7 D6
          // 0  0 NULL CODE
          // 0  1 RESET Rx CRC CHECKER
          // 1  0 RESET Tx CRC GENERATOR
          // 1  1 RESET Tx UNDERRUN/EOM LATCH
          (value >> 3) & 7 match
            case 0b010 =>
              log.info("Z8530 WR0 write: reset ext/stat interrupts")
              externalInterruptStatus = 0
            case _ =>
              log.info("Z8530 WR0 write: value %02X",value)
        case 0x1 => // WR1
          // b0: 1 external interrupt is enabled
          // b1: 1 transmit buffer empty interrupt is enabled
          // b2: 1 parity error is a special condition
          // b3/b4:
            // 00 = disable receive interrupt
            // 01 = interrupt on first character or special condition
            // 10 = interrupt on all characters and special conditions
            // 11 = interrupt only upon special conditions.
          // b5: 1 wait/request is relative to read buffer
          // b6: Wait/request output is for
            // 0 wait: floating when inactive, low if CPU is attempting to transfer data the SCC isn't yet ready for
            // 1 request: high if inactive, low if SCC is ready to transfer data
          // b7: 0 Wait/Request output is inactive, 1 output is informative
          log.info("Z8530 WR1 write: interrupt mask %02X",value)
          interruptMask = value
        case 0x2 => // WR2
          log.info("Z8530 interrupt vector %02X",value)
        case 0x3 => // WR3
          // b7,b6:
          //	00 = 5 receive bits per character
          //	01 = 7 bits
          //	10 = 6 bits
          //	11 = 8 bits
          //
          // b5 = 1 => DCD and CTS outputs are set automatically; 0 => they're inputs to read register 0.
          //							(DCD is ignored in local loopback; CTS is ignored in both auto echo and local loopback).
          // b4: enter hunt mode (if set to 1, presumably?)
          // b3 = 1 => enable receiver CRC generation; 0 => don't.
          // b2: address search mode (SDLC)
          // b1: sync character load inhibit.
          // b0: Rx enable.
          log.info("Z8530 WR3 write %02X", value)
        case 0x4 => // WR4
          /*
            b0: PARITY ENABLED
            b1: PARITY EVEN/ODD
            b2/b3:
              00: SYNC MODES ENABLED
              01: 1 STOP BIT/CHARACTER
              10: 1 1/2 STOP BIT/CHARACTER
              11: 2 STOP BIT/CHARACTER
            b4/b5:
              00: 8 BIT SYNC CHARACTER
              01: 16 BIT SYNC CHARACTER
              10: SDLC MODE
              11: EXTERNAL SYNC MODE
            b6/b7:
              00: X1 CLOCK MODE
              01: X16 CLOCK MODE
              10: X32 CLOCK MODE
              11: X64 CLOCK MODE
           */
          log.info("Z8530 WR4 write %02X", value)
        case 0x5 => // WR5
          /*
           b7: DTR
           b6/b5:
            00 = Tx 5 bits (or less) per character
            01 = Tx 7 bits per character
            10 = Tx 6 bits per character
            11 = Tx 8 bits per character
           b4: send break.
           b3: Tx enable.
           b2: SDLC (if 0) / CRC-16 (if 1)
           b1: RTS
           b0: Tx CRC enable.
          */
          log.info("Z8530 WR5 write %02X", value)
        case 0x6 => // WR6
          log.info("Z8530 WR6 write %02X", value)
        case 0x7 => // WR7
          log.info("Z8530 WR7 write %02X", value)
        case 0x9 => // WR9
          log.info("Z8530 WR9 write %02X", value)
        case 0xA => // WR10
          log.info("Z8530 WR10 write %02X", value)
        case 0xB => // WR11
          log.info("Z8530 WR11 write %02X", value)
        case 0xC => // WR12
          log.info("Z8530 WR12 write %02X", value)
        case 0xD => // WR13
          log.info("Z8530 WR13 write %02X", value)
        case 0xE => // WR14
          log.info("Z8530 WR14 write %02X", value)
        case 0xF => // WR15
          // External interrupt mask:
          // b0: 0
          // b1: Zero count.
          // b2: 0
          // b3: DCD.
          // b4: Sync/hunt.
          // b5: CTS.
          // b6: Tx underrun/EOM.
          // b7: Break/abort.
          log.info("Z8530 external interrupt mask %02X", value)
          externalInterruptMask = value
        case _ =>
          log.warning("Z8530 write to unrecognized register %0X",reg)
    def read(reg:Int): Int =
      reg match
        case 0x0 => // RR0
          // b0: Rx character available.
          // b1: zero count.
          // b2: Tx buffer empty.
          // b3: DCD.
          // b4: sync/hunt.
          // b5: CTS.
          // b6: Tx underrun/EOM.
          // b7: break/abort.

          log.info("Z8530 reading DCD(%d) %s",channel,DCD)
          if DCD then 0x8 else 0x0
        case 0x1 => // RR1
          // b0: all sent.
          // b1: residue code 0.
          // b2: residue code 1.
          // b3: residue code 2.
          // b4: parity error.
          // b5: Rx overrun error.
          // b6: CRC/framing error.
          // b7: end of frame (SDLC).
          0x1
        case 0x2 => // RR2
          // Interrupt vector — modified by status information in B channel.
          0x0
        case 0x3 => // RR3
          // B channel: all bits are 0.
          // A channel:
          // b0: Channel B ext/status IP.
          // b1: Channel B Tx IP.
          // b2: Channel B Rx IP.
          // b3: Channel A ext/status IP.
          // b4: Channel A Tx IP.
          // b5: Channel A Rx IP.
          // b6, b7: 0.
          if channel == 1 then 0x0
          else
            var rr3 = 0
            if externalInterruptStatus != 0 then rr3 |= 0x8
            if channels(1).externalInterruptStatus != 0 then rr3 |= 1
            rr3
        case 0xA => // RR10
          // b0: 0
          // b1: On loop.
          // b2: 0
          // b3: 0
          // b4: Loop sending.
          // b5: 0
          // b6: Two clocks missing.
          // b7: One clock missing.
          0x0
        case 0xC => // RR12
          // Lower byte of time constant.
          0x0
        case 0xD => // R13
          // Upper byte of time constant.
          0x0
        case 0xF => // R15
          // External interrupt status:
          // b0: 0
          // b1: Zero count.
          // b2: 0
          // b3: DCD.
          // b4: Sync/hunt.
          // b5: CTS.
          // b6: Tx underrun/EOM.
          // b7: Break/abort.
          externalInterruptStatus
        case _ =>
          log.warning("Z8530 reading from unspecified register: %02X",reg)
          0x0
  end Channel

  private final val channels = Array(new Channel(0), new Channel(1))
  private var interruptVector = 0 // WR2 shared between channels
  private var masterInterruptControl = 0 // WR9 shared between channels
  private var registerAddress = 0

  override protected def reset(): Unit = 
    super.reset()
    interruptVector = 0
    masterInterruptControl = 0
    registerAddress = 0
    channels.foreach(_.reset())

  override def getProperties: List[MACComponent.Property] =
    import MACComponent.Property
    val prop = new ListBuffer[Property]
    prop += Property("Master Interrupt control","%02X".format(masterInterruptControl))
    prop += Property("Register address","%02X".format(registerAddress))
    for c <- 0 to 1 do
      val channel = if c == 0 then "A" else "B"
      prop += Property(s"$channel DCD",channels(c).getDCD.toString)
      prop += Property(s"$channel Interrupt mask", channels(c).getInterruptMask.toString)
      prop += Property(s"$channel External interrupt mask", channels(c).getExternalInterruptMask.toString)
      prop += Property(s"$channel External interrupt status", channels(c).getExternalInterruptStatus.toString)

    prop.toList

  private def hasInterrupt: Boolean =
    (masterInterruptControl & 0x8) != 0 && (channels(0).hasInterrupt || channels(1).hasInterrupt)

  private def checkIRQ(): Unit = irqLow(hasInterrupt)

  def setDCD(channel:Int,dcd:Boolean): Unit =
    channels(channel).setDCD(dcd)
    checkIRQ()
  def readData(channel:Int): Int = channels(channel).getData
  def writeData(channel:Int,data:Int): Unit = channels(channel).setData(data)

  def writeControl(channel:Int,value:Int): Unit =
    registerAddress match
      case 2 =>
        interruptVector = value
        log.info("Z8530 writing interrupt vector to %02X",value)
      case 9 =>
        /*
        The Master Interrupt Enable (MIE) bit, WR9 D3, must be
        set to enable the SCC to generate interrupts. The MIE bit should be set after initializing the SCC registers and enabling the individual interrupt enables.
        The SCC requests an interrupt by asserting the /INT pin Low from its opendrain state only upon detection that one of the enabled interrupt conditions has been detected.
        */
        masterInterruptControl = value
        log.info("Z8530 writing master interrupt control to %02X",value)
      case _ =>
        channels(channel).write(registerAddress,value)

    if registerAddress != 0 then // after each access the register address must be cleared
      registerAddress = 0
    else
      registerAddress = value & 7
      if ((value >> 3) & 7) == 1 then
        registerAddress |= 0x8
      log.info("Z8530 writing control register setting register address to %02X",registerAddress)

    checkIRQ()

  def readControl(channel:Int): Int =
    val read = registerAddress match
      case 2 =>
        log.info("Z8530 reading interrupt vector %02X",interruptVector)
        if channel == 0 then interruptVector
        else
          var r2 = interruptVector
          val extStatusChangeA = if channels(0).hasInterrupt then 0b101 else 0b000
          val extStatusChangeB = if channels(1).hasInterrupt then 0b001 else 0b000
          val extStatusChange = if extStatusChangeA != 0 then extStatusChangeA else extStatusChangeB
          if (masterInterruptControl & 0x10) == 0 then // V3 V2 V1
            r2 &= 0b11110001
            r2 |= extStatusChange << 1 //(extStatusChangeA | extStatusChangeB) << 1
          else // V6 V5 V4
            r2 &= 0b10001111
            r2 |= extStatusChange << 4 //(extStatusChangeA | extStatusChangeB) << 4
          r2
      case _ =>
        channels(channel).read(registerAddress)

    registerAddress = 0
    read

