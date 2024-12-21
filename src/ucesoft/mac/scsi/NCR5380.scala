package ucesoft.mac.scsi

import ucesoft.mac.{MACComponent, MessageBus}

import javax.swing.ImageIcon
import scala.collection.mutable.ListBuffer
import scala.compiletime.uninitialized

/**
 * @author Alessandro Abbruzzetti
 *         Created on 04/12/2024 15:10
 *
 *         The address of each register is computed as follows:
 *         $580drn
 *         where r represents the register number (from 0 through 7),
 *         n determines whether it a read or write operation
 *         (0 for reads, or 1 for writes), and
 *         d determines whether the DACK signal to the NCR 5380 is asserted.
 *         (0 for not asserted, 1 is for asserted)
 *
 *         Here's an example of the address expressed in binary:
 *         0101 1000 0000 00d0 0rrr 000n
 *
 *         Note: Asserting the DACK signal applies only to write operations to the output data
 *         register and read operations from the input data register.
 *
 *         Registers:
 *         A2 A1 A0   R/W   Register name
 *         -------------------------------------
 *          0  0  0   R     Current SCSI Data
 *          0  0  0   W     Output Data
 *          0  0  1   R/W   Initiator Command
 *          0  1  0   R/W   Mode
 *          0  1  1   R/W   Target Command
 *          1  0  0   R     Current SCSI Bus Status
 *          1  0  0   W     Select enable
 *          1  0  1   R     Bus and status
 *          1  0  1   W     Start DMA send
 *          1  1  0   R     Input Data
 *          1  1  0   W     Start DMA target receive
 *          1  1  1   R     Reset Parity Interrupts
 *          1  1  1   W     Start DMA Initiator Receive
 *
 *         Memory
 *         Location	  NCR 5380 Internal Register
 *         -----------------------------------------
 *         $580201		Output Data Register with DACK
 *         $580260		Current SCSI Data with DACK
 *         $580001		Output Data Register
 *         $580011		Initiator Command Register
 *         $580021		Mode Register
 *         $580031		Target Command Register
 *         $580041		Select Enable Register
 *         $580051		Start DMA Send
 *         $580061		Start DMA Target Receive
 *         $580071		Start DMA Initiator Receive
 *         $580000		Current SCSI Data
 *         $580010		Initiator Command Register
 *         $580020		Mode Registor
 *         $580030		Target Command Register
 *         $580040		Current SCSI Bus Status
 *         $580050		Bus and Status Register
 *         $580060		Input Data Register
 *         $580070		Reset Parity/Interrupt
 */
class NCR5380 extends MACComponent:
  protected override val componentName = "SCSI Controller"
  override protected val icon = new ImageIcon(getClass.getResource("/resources/trace/scsi.png"))

  private enum Lines:
    final val bit = 1 << ordinal
    case I_O  // (INPUT/OUTPUT). A signal driven by a target that controls the direction of data movement on the DATA BUS with respect to an initiator.
              // True indicates input to the initiator. This signal is also used to distinguish between SELECTION and RESELECTION phases.
    case C_D  // (CONTROL/DATA). A signal driven by a target that indicates whether CONTROL or DATA information is on the DATA BUS. True indicates CONTROL.
    case MSG  // (MESSAGE). A signal driven by a target during the MESSAGE phase.

    case ACK  // (ACKNOWLEDGE). A signal driven by an initiator to indicate an acknowledgment for a REQ/ACK data transfer handshake.
    case ATN  // (ATTENTION). A signal driven by an initiator to indicate the ATTENTION condition.
    case BSY  // (BUSY). An "OR-tied" signal that indicates that the bus is being used.
    case REQ  // (REQUEST). A signal driven by a target to indicate a request for a REQ/ACK data transfer handshake.
    case RST  // (RESET). An "OR-tied" signal that indicates the RESET condition.
    case SEL  // (SELECT). A signal used by an initiator to select a target or by a target to reselect an initiator.

  private enum Mode:
    final val bit = 1 << ordinal
    case ARBITRATE  // The ARBITRATE bit is set (1) to start the arbitration process. Prior to setting this bit the Output Data
                    // Register should contain the proper SCSI device ID value. Only one data bit should be active for SCSI bus
                    // arbitration. The NCR 5380 will wait for a bus free condition before entering the arbitration phase. The
                    // results of the arbitration phase may be determined by reading the status bits LA and AlP
    case DMA_MODE   // The DMA MODE bit is normally used to enable a DMA transfer and must be set (1) prior to writing ports 5 through 7. Ports 5 through 7 are used to start DMA transfers.
    case MON_BUSY   // The MONITOR BUSY bit, when true (1), causes an interrupt to be generated for an unexpected loss of BSY (pin 13).
    case EN_EOP_INT // The ENABLE EOP INTERRUPT, when set (1), causes an interrupt to occur when an EOP (End of Process) signal (pin 27) is received from the DMA controller logic
    case EN_PA_INT  // The ENABLE PARITY INTERRUPT bit, when set (1), will cause an interrupt (IRQ) to occur if a parity error is detected
    case EN_PA_CHECK// The ENABLE PARITY CHECKING bit determines whether parity errors will be ignored or saved in the parity error latch
    case TARGET_MODE// The TARGETMODE bit allows the NCR 5380 to operate as either an SCSI bus initiator, bit reset (0), or as an SCSI.bus target device, bit set (1). In order for the
                    // signals ATN (pin 15) and ACK (pin 14) to be asserted on the SCSI bus, the TARGETMODE bit must be reset (0). In order for the signals C/O, I/O, MSG and REQ to
                    // be asserted on the SCSI bus, the TARGETMODE bit must be set (1).
    case BLK_MD_DMA // The BLOCK MODE DMA bit controls the characteristics of the DMA DRQ-DACK handshake.

  private enum BusPhase:
    case BusFree      // The BUS FREE phase is used to indicate that no SCSI device is actively using the SCSI bus and that it is available for subsequent users.
    case Arbitration  // The ARBITRATION phase allows one SCSI device to gain control of the SCSI bus so that it can assume the role of an initiator or target.
    case Selection    // The SELECTION phase allows an initiator to select a target for the purpose of initiating some target function (e.g., READ or WRITE command).
    case Reselection  // RESELECTION is an optional phase that allows a target to reconnect to an initiator for the purpose of continuing some operation that was previously started by the initiator
                      // but was suspended by the target, (i.e., the target disconnected by allowing a BUS FREE phase to occur before the operation was complete).
    case Command      // The COMMAND phase allows the target to request command information from the initiator.
                      // The target shall assert the C/D signal and negate the I/O and MSG signals during the REQ/ACK handshake(s) of this phase.
    case DataIn       // The DATA IN phase allows the target to request that data be sent to the initiator from the target.
                      // The target shall assert the I/O signal and negate the C/D and MSG signals during the REQ/ACK handshake(s) of this phase.
    case DataOut      // The DATA OUT phase allows the target to request that data be sent from the initiator to the target.
                      // The target shall negate the C/D, I/O, and MSG signals during the REQ/ACK handshake(s) of this phase.
    case Status       // The STATUS phase allows the target to request that status information be sent from the target to the initiator.
                      // The target shall assert C/D and I/O and negate the MSG signal during the REQ/ACK handshake of this phase.
    case MessageIn    // The MESSAGE IN phase allows the target to request that message(s) be sent to the initiator from the target.
                      // The target shall assert C/D, I/O, and MSG during the REQ/ACK handshake(s) of this phase.
    case MessageOut   // The MESSAGE OUT phase allows the target to request that message(s) be sent from the initiator to the target. The target may invoke this phase at its convenience in response to the ATTENTION condition (see 5.2.1) created by the initiator.
                      // The target shall assert C/D and MSG and negate I/O during the REQ/ACK handshake(s) of this phase. The target shall handshake byte(s) in this phase until ATN goes false, unless an error occurs

  import BusPhase.*
  import Lines.*
  import Mode.*

  private inline val STATUS_GOOD = 0
  private inline val MESSAGE_COMPLETE = 0

  private val PARITY_TABLE = buildParityTable()

  private var scsiLines = 0 // ACK,ATN,BSY,C_D,I_O,MSG,REQ,RST,SEL
  private var scsiData = 0  // DB0 - DB7

  private var initiatorCommandReg = 0
  private var latchedInputReg = 0xFF
  private var targetCommandReg = 0
  private var targetBSYBit = false // BSY is "OR-tied" signal

  private var aip = false // arbitration in progress

  // mode
  private var mode = 0
  //
  private var assertDataBus = false
  private var irq = false

  // phase
  private var phase = BusFree

  // targets
  private val targets = Array.ofDim[SCSITarget](7)
  private var currentTarget : SCSITarget = uninitialized

  // dma
  private var dmaMode = false
  private var dmaReq = false

  // select enable
  private var selectEnable = 0

  // buffers
  private val commandBuffer = new ListBuffer[Int]
  private var commandByteLen = 0
  private var dataInBuffer : Array[Byte] = Array()
  private var dataInPos = 0
  private var dataOutBuffer : Array[Byte] = Array()
  private var dataOutPos = 0
  private var dataOutHandler : Array[Byte] => Int = uninitialized

  // listener
  private var scsiListener : SCSIListener = uninitialized

  // irq
  private var irqHandler: Boolean => Unit = uninitialized

  inline private def isScsiSet(bits:Int): Boolean = (scsiLines & bits) == bits
  inline private def setScsi(bits:Int): Unit = scsiLines |= bits
  inline private def clearScsi(bits:Int): Unit = scsiLines &= ~bits

  override def onMessage(msg: MessageBus.Message): Unit =
    msg match
      case MessageBus.Shutdown(_,_) =>
        for s <- targets do 
          if s != null then
            println("SCSI disconnecting target %d".format(s.id))
            s.disconnect()
      case _ =>
  end onMessage

  override protected def reset(): Unit = 
    super.reset()
    setPhase(BusFree)
    selectEnable = 0
    dmaMode = false
    dmaReq = false
    checkIRQ(newIrq = false)
    mode = 0
    assertDataBus = false
    aip = false
    targetCommandReg = 0
    initiatorCommandReg = 0
    scsiData = 0

  def setIRQHandler(handler:Boolean => Unit): Unit =
    irqHandler = handler

  def setSCSIListener(l:SCSIListener): Unit =
    scsiListener = l

  override def getProperties: List[MACComponent.Property] =
    import MACComponent.Property
    List(
      Property("SCSI lines","%02X".format(scsiLines | (if targetBSYBit then BSY.bit else 0))),
      Property("SCSI data","%02X".format(scsiData)),
      Property("Phase",phase.toString),
      Property("Last command buffer",commandBuffer.map(b => "%02X".format(b)).mkString(" ")),
      Property("Data in",s"$dataInPos/${dataInBuffer.length}"),
      Property("Data out",s"$dataOutPos/${dataOutBuffer.length}"),
      Property("Current selected scsi ID",Option(currentTarget).map(_.id.toString).getOrElse("none")),
      Property("Dma mode",dmaMode.toString),
      Property("Dma req",dmaReq.toString),
      Property("Mode","%02X".format(mode))
    )

  private def buildParityTable(): Array[Byte] =
    def checkParity(_byte:Int): Byte =
      var byte = _byte
      var parity = 0
      while byte != 0 do
        parity ^= (byte & 1)
        byte >>= 1
      parity.toByte
    (0 to 255).map(checkParity).toArray

  def setTarget(target:SCSITarget): Unit =
    targets(target.id) = target
    log.info("SCSI added new target %d",target.id)
  def removeTarget(id:Int): Unit =
    targets(id) = null
    log.info("SCSI removed target %d",id)

  def read(address:Int): Int =
    val dack = (address & 0b10_0000_0000) != 0
    val reg = (address >> 4) & 7
    val w = (address & 1) == 1

    if dack then
      dmaReq = false

    if w then
      log.warning("SCSI reading with write mode set")
      return 0

    reg match
      case 0b000 => // Current SCSI Data
        //if phase == Status then println(s"SCSI reading STATUS $scsiData") else println(s"SCSI reading data $scsiData")
        if phase == DataIn && dmaMode then
          if nextDataIn(assertReq = false) then dmaReq = true
        scsiData
      case 0b001 => // Initiator Command
        /*
        7 RST
        6 AIP (Arbitration in progress)
        5 LA (Lost arbitration)
        4 ACK
        3 BSY
        2 SEL
        1 ATN
        0 DATA BUS
        */
        (initiatorCommandReg & ~0x60) |
        (if aip then 0x40 else 0x00)
      case 0b010 => // Mode
        mode
      case 0b011 => // Target Command
        targetCommandReg
      case 0b100 => // Current SCSI Bus Status
        /*
        7 RST
        6 BSY
        5 REQ
        4 MSG
        3 C/D
        2 I/O
        1 SEL
        0 DBP
        */
        var st = 0
        if isScsiSet(RST.bit) then st |= 0x80
        if isScsiSet(BSY.bit) || targetBSYBit then st |= 0x40
        if isScsiSet(REQ.bit) then st |= 0x20
        if isScsiSet(MSG.bit) then st |= 0x10
        if isScsiSet(C_D.bit) then st |= 0x08
        if isScsiSet(I_O.bit) then st |= 0x04
        if isScsiSet(SEL.bit) then st |= 0x02
        if PARITY_TABLE(scsiData) == 1 then st |= 0x01
        log.info("SCSI reading bus status %02X",st)
        st
      case 0b101 => // Bus and status
        /*
        7 End of DMA
        6 DMA request
        5 Parity error
        4 Interrupt request active
        3 Phase match
        2 Busy error
        1 ATN
        0 ACK
        */
        var st = 0
        if dmaReq then st |= 0x40
        if irq then st |= 0x10
        if (scsiLines & 7) == targetCommandReg then st |= 0x8 // Phase match
        st
      case 0b110 => // Input Data
        latchedInputReg
      case 0b111 => // Reset Parity Interrupts
        checkIRQ(newIrq = false)
        0
      case _ =>
        log.warning("SCSI reading unhandled register %d",reg)
        0
  end read

  def write(address:Int,value:Int): Unit =
    val dack = (address & 0b10_0000_0000) != 0
    val reg = (address >> 4) & 7
    val w = (address & 1) == 1

    if !w then
      log.warning("SCSI writing with write mode not set")
      return

    if dack then dmaReq = false

    reg match
      case 0b000 => // Output Data
        scsiData = value & 0xFF
        if dmaMode && phase == DataOut then
          nextDataOut(assertReq = false,value.toByte)
          dmaReq = true
        checkSelectEnableReg()
        log.info("SCSI writing output reg %02X",value)
        //println("SCSI writing output reg %02X".format(value))
        // TODO DMA ack
      case 0b001 => // Initiator Command
        val oldIC = initiatorCommandReg
        initiatorCommandReg = value
        log.info("SCSI write to Initiator Command Reg %02X",value)
        //println("SCSI write to Initiator Command Reg %02X".format(value))
        /*
        7 RST
        6 TEST MODE
        5 DIFF ENBL
        4 ACK
        3 BSY
        2 SEL
        1 ATN
        0 DATA BUS
        */
        /*7*/
        if ((initiatorCommandReg ^ oldIC) & 0x80) != 0 then
          if (value & 0x80) != 0 then setScsi(RST.bit) else clearScsi(RST.bit)
          checkReset()
        /*6 ignored: This bit may be written during a test environment to disable all output drivers */
        /*5 ignored: This bit is not used in the NCR 5380 and is only meaningful in the NCR 5381 */
        /*4*/
        if (mode & TARGET_MODE.bit) == 0 then
          if isScsiSet(ACK.bit) ^ ((value & 0x10) != 0) then
            ackChanged((value & 0x10) != 0)
        /*3*/
        if isScsiSet(BSY.bit) ^ ((value & 0x8) != 0) then
          bsyChanged((value & 0x8) != 0)
        /*2*/
        if isScsiSet(SEL.bit) ^ ((value & 0x4) != 0) then
          selChanged((value & 0x4) != 0)
        /*1*/
        if (mode & TARGET_MODE.bit) == 0 then
          if isScsiSet(ATN.bit) ^ ((value & 0x2) != 0) then
            atnChanged((value & 0x2) != 0)
        /*0*/
        if (mode & TARGET_MODE.bit) == 0 then
          assertDataBus = (value & 1) != 0

        checkControlLines()
      case 0b010 => // Mode
        /*
        7 Block mode DMA
        6 Target mode
        5 Enable parity check
        4 Enable parity interrupt
        3 Enable EOP interrupt
        2 Monitor busy
        1 DMA mode
        0 Arbitrate
        */
        val oldMode = mode
        mode = value
        if (oldMode ^ mode) != 0 then modeChanged(oldMode)
      case 0b011 => // Target Command
        /*
        3 Assert REQ
        2 Assert MSG
        1 Assert C/D
        0 Assert I/O
        */
        targetCommandReg = value
        //println(s"SCSI target command register is $value")
      case 0b100 => // Select enable
        selectEnable = value
      case 0b101 => // Start DMA send
        //println("Start DMA send")
        if phase == DataIn || phase == DataOut then
          dmaReq = true
      case 0b110 => // Start DMA target receive
        //println("Start DMA target receive")
      case 0b111 => // Start DMA Initiator Receive
        //println("Start DMA Initiator Receive")
        if phase == DataIn || phase == DataOut then
          dmaReq = true
  end write

  private def checkReset(): Unit =
    if isScsiSet(RST.bit) then
      println("SCSI RESETTING")
      // TODO
      setPhase(BusFree)
      checkIRQ(newIrq = false)
      dmaReq = false

  private def modeChanged(oldMode:Int): Unit =
    log.info("SCSI mode changed: %02X",mode)
    if (oldMode & 1) == 0 && (mode & 1) == 1/* && phase == BusFree*/ then // ARBITRATE 0 -> 1
      aip = true
      phase = Arbitration
      //println("SCSI Arbitration in progress ...")
    else if (oldMode & 1) == 1 && (mode & 1) == 0 then // ARBITRATE 1 -> 0
      aip = false

    val oldDmaMode = (oldMode & 2) != 0
    dmaMode = (mode & 2) != 0
    if !dmaMode then dmaReq = false
    //println("SCSI mode changed: %02X dmaMode=%b".format(mode,dmaMode))
//    if dmaMode then
//      println("DMA mode is on")
    if oldDmaMode && !dmaMode && phase == DataIn then
      // DMA stopped
      setPhase(Status,STATUS_GOOD)
  end modeChanged

  private def ackChanged(ack:Boolean): Unit =
    if ack then setScsi(ACK.bit) else clearScsi(ACK.bit)
    phase match
      case Command =>
        if ack then
          clearScsi(REQ.bit)
        else
          //println("SCSI Command byte: %02X".format(scsiData))
          if commandBuffer.isEmpty then
            currentTarget.identifyCommandLen(scsiData) match
              case Some(len) =>
                commandBuffer += scsiData
                commandByteLen = len - 1
                setScsi(REQ.bit)
              case None =>
                //println("SCSI unsupported command: %02X".format(scsiData))
                log.error("SCSI unsupported command: %02X",scsiData)
          else
            commandBuffer += scsiData
            commandByteLen -= 1
            if commandByteLen == 0 then
              val cmd = commandBuffer.toArray
              commandBuffer.clear()
              executeCommand(cmd)
            else
              setScsi(REQ.bit)
      case Status =>
        if ack then
          clearScsi(REQ.bit)
        else
          setPhase(MessageIn)
      case MessageIn =>
        if ack then
          clearScsi(REQ.bit)
        else
          setPhase(BusFree)
      case DataIn =>
        if ack then
          clearScsi(REQ.bit)
        else
          nextDataIn(assertReq = true)
      case DataOut =>
        if ack then
          clearScsi(REQ.bit)
        else
          nextDataOut(assertReq = true,scsiData.toByte)
      case _ =>
  end ackChanged

  private def nextDataIn(assertReq:Boolean): Boolean =
    if dataInPos == dataInBuffer.length then
      setPhase(Status, STATUS_GOOD)
      //println(s"SCSI finished reading DataIn($dataInPos/${dataInBuffer.length})=$scsiData")
      false
    else
      if assertReq then setScsi(REQ.bit)
      scsiData = dataInBuffer(dataInPos).toInt & 0xFF
      dataInPos += 1
      //println(s"SCSI reading DataIn($dataInPos/${dataInBuffer.length})=$scsiData")
      true
  end nextDataIn

  private def nextDataOut(assertReq:Boolean,value:Byte): Boolean =
    dataOutBuffer(dataOutPos) = value
    dataOutPos += 1
    //println(s"Data out $dataOutPos/${dataOutBuffer.length}")
    if dataOutPos == dataOutBuffer.length then
      //println(s"SCSI finished writing DataOut($dataOutPos/${dataOutBuffer.length})")
      val status = dataOutHandler(dataOutBuffer)
      setPhase(Status, status)
      false
    else
      if assertReq then setScsi(REQ.bit)
      true
  end nextDataOut

  /*
    I/O C/D MSG   Bus phase
    ------------------------
    0   0   0     Data out
    0   1   0     Command
    0   1   1     Message out
    1   0   0     Data in
    1   1   0     Status
    1   1   1     Message in
  */
  private def setPhase(phase: BusPhase,statusOrLen:Int = -1,data:Array[Byte] = null): Unit =
    dmaReq = false

    phase match
      case Command =>
        setScsi(BSY.bit | C_D.bit | REQ.bit)
        targetBSYBit = true
        clearScsi(I_O.bit | MSG.bit)
        this.phase = Command
      case Status =>
        // set status bits
        setScsi(REQ.bit | C_D.bit | I_O.bit)
        clearScsi(MSG.bit)
        scsiData = statusOrLen
        this.phase = Status
      case DataIn =>
        setScsi(REQ.bit | I_O.bit)
        clearScsi(C_D.bit | MSG.bit)
        dataInBuffer = data
        dataInPos = 0
        this.phase = DataIn
      case DataOut =>
        setScsi(REQ.bit)
        clearScsi(C_D.bit | MSG.bit | I_O.bit)
        dataOutBuffer = Array.ofDim[Byte](statusOrLen)
        dataOutPos = 0
        this.phase = DataOut
      case MessageIn =>
        setScsi(REQ.bit | C_D.bit | I_O.bit | MSG.bit)
        scsiData = MESSAGE_COMPLETE
        this.phase = MessageIn
      case BusFree =>
        targetBSYBit = false
        scsiLines = 0
        this.phase = BusFree
        if scsiListener != null then
          scsiListener.noTargetSelected()
      case _ =>
        println(s"SCSI phase not managed: $phase")
        log.error("SCSI phase not managed: %s",phase)
  end setPhase

  private def executeCommand(cmd:Array[Int]): Unit =
    currentTarget.executeCommand(cmd) match
      case Some(SCSITargetResponse.Status(st)) =>
        setPhase(Status,st)
      case Some(SCSITargetResponse.DataIn(data)) =>
        setPhase(DataIn,0,data)
      case Some(SCSITargetResponse.DataOut(len,handler)) =>
        dataOutHandler = handler
        setPhase(DataOut,len)
      case None =>
  end executeCommand

  private def bsyChanged(bsy: Boolean): Unit =
    if bsy || targetBSYBit then setScsi(BSY.bit) else clearScsi(BSY.bit)
    checkSelectEnableReg()
  private def selChanged(sel: Boolean): Unit =
    if sel then setScsi(SEL.bit) else clearScsi(SEL.bit)
    checkSelectEnableReg()
  private def atnChanged(atn: Boolean): Unit =
    if atn then setScsi(ATN.bit) else clearScsi(ATN.bit)

  private def checkControlLines(): Unit =
    phase match
      case Arbitration =>
        if isScsiSet(BSY.bit | SEL.bit) then
          phase = Selection
          //println(s"SCSI going to Selection phase: selected ID $scsiData")
          checkSelection()
      case Selection =>
        checkSelection()
      case _ =>

  private def dataLinesToTarget: Option[SCSITarget] =
    val id = scsiData & 0x7F match
      case 0x40 => 6
      case 0x20 => 5
      case 0x10 => 4
      case 0x08 => 3
      case 0x04 => 2
      case 0x02 => 1
      case 0x01 => 0
    Option(targets(id))

  private def checkSelection(): Unit =
    if (scsiLines & (BSY.bit | I_O.bit)) == 0 then
      dataLinesToTarget match
        case Some(target) =>
          currentTarget = target
          if scsiListener != null then
            scsiListener.targetSelected(target.id)
          //println(s"SCSI Selection of $scsiData: ${target.id}")
          setPhase(Command)
        case None =>
          //println(s"SCSI Selection of non-existent target ${scsiData & 0x7F}")
          setPhase(BusFree)

  private def checkSelectEnableReg(): Unit =
    if selectEnable == (scsiData & 0x7F) && isScsiSet(BSY.bit | SEL.bit) then
      checkIRQ(newIrq = true)

  private def checkIRQ(newIrq:Boolean): Unit =
    if newIrq ^ irq then
      if irqHandler != null then
        irqHandler(newIrq)
      irq = newIrq
