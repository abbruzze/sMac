package ucesoft.mac.storage

import ucesoft.mac.{MACComponent, MacModel}

import scala.collection.mutable

/**
 * @author Alessandro Abbruzzetti
 *         Created on 08/01/2025 15:13
 *
 * Based on:
 *  1. SWIM_Chip_Users_Ref_198801
 *  2. SonyMFM.a, SonyEqu.a, Sony.a : https://github.com/elliotnunn/supermario/tree/master
 *
 */
class SWIM extends IWM:
  override protected val componentName = "SWIM"

  private enum REGISTER:
    case
      DATA,         // R/W A3..A0 = x000 [action = 1]
      CORRECTION,   // R   A3..A0 = 1000 [action = 0]
      MARK,         // R/W A3..A0 = x001
      CRC,          // W   A3..A0 = 0010 [action = 1]
      IWM_CONF,     // W   A3..A0 = 0010 [action = 0]
      PRAM,         // R/W A3..A0 = x011
      PHASE,        // R/W A3..A0 = x100
      SETUP,        // R/W A3..A0 = x101
      MODE,         // W A3..A0 = 011x
      STATUS,       // R A3..A0 = 1110
      ERROR,        // R A3..A0 = 1010
      HANDSHAKE     // R A3..A0 = 1111

  private enum STATE:
    case Idle, Reading, Writing

  // Error register useful bits
  private inline val ERROR_UNDERRUN = 0x1
  private inline val ERROR_MARK_BYTE_READ = 0x2
  private inline val ERROR_OVERRUN = 0x4
  // Mode register useful bits
  private inline val MODE_CLEAR_FIFO = 0x01
  private inline val MODE_ENABLE1 = 0x02
  private inline val MODE_ENABLE2 = 0x04
  private inline val MODE_ACTION = 0x08
  private inline val MODE_RW = 0x10
  private inline val MODE_HDSEL = 0x20
  private inline val MODE_ISM_IWM_SELECT = 0x40
  private inline val MODE_MOTORON = 0x80
  // Handshake register useful bit
  private inline val HANDSHAKE_MARK_BYTE = 0x01
  private inline val HANDSHAKE_CRC_ERROR = 0x02
  private inline val HANDSHAKE_RDDATA = 0x04
  private inline val HANDSHAKE_SENSE = 0x08
  private inline val HANDSHAKE_MOTORON = 0x10
  private inline val HANDSHAKE_ERROR = 0x20
  private inline val HANDSHAKE_FIFO_2EMPTY = 0x40
  private inline val HANDSHAKE_FIFO_1OR2EMPTY = 0x80
  // Setup register useful bit
  private inline val SETUP_MOTORON_TIMER = 0x80
  private inline val SETUP_GCR = 0x04
  private inline val SETUP_FCLK = 0x08

  private inline val MARK_BYTE_MASK = 0x100

  private inline val FDHD_EJECT_CYCLES = 1 // eject immediately ?

  private var errorRegisterLockedForWriting = false

  private final val swimReg = Array.ofDim[Int](REGISTER.values.length)
  private final val fifo = new mutable.Queue[Int]()

  private final val pram = Array.ofDim[Int](16)
  private var pramAddress = 0

  private var iwmModeStateCounter = 0

  private var mfmMode = true
  private var mfmBitCycles = 0
  private var mfmBitLengthCycles = 0
  private var mfmInSync = false
  private var mfmMarkCounter = 0
  private var mfmBitCount = 0

  private var driveSelected = 0

  private var crc = 0xFFFF
  private var writeCrcCounter = 0

  private var state = STATE.Idle
  private var swimCycles = 0 // see setModel

  private var savedDriveRegisterSelection = 0

  import STATE.*
  import REGISTER.*

  override protected def reset(): Unit =
    super.reset()
    errorRegisterLockedForWriting = false
    fifo.clear()
    java.util.Arrays.fill(pram,0)
    pramAddress = 0
    iwmModeStateCounter = 0
    mfmMode = true
    mfmBitCycles = 0
    mfmBitLengthCycles = 0
    mfmInSync = false
    mfmMarkCounter = 0
    mfmBitCount = 0
    writeCrcCounter = 0
    state = STATE.Idle
  end reset

  override protected def setModel(model: MacModel): Unit =
    super.setModel(model)
    swimCycles = if macModel.ordinal >= MacModel.SEFDHD.ordinal then 2 else 1 // 2 = 16Mhz, 1 = 8Mhz
    log.info("SWIM set clock multiplier to %d",swimCycles)
  end setModel

  override def getProperties: List[MACComponent.Property] =
    val sp = super.getProperties
    if macModel.ordinal < MacModel.SEFDHD.ordinal then sp
    else
      var tp = "-"
      if getSelectedDrive != null && getSelectedDrive.isFloppyIn then
        val tr = getSelectedDrive.getTrack
        val t = getSelectedDrive.getFloppy.get.getTrack(getHead,tr)
        tp = s"${t.getPos}/${t.getBitSize}"
      List(
        MACComponent.Property("IWM mode",iwmMode.toString),
        MACComponent.Property("ACTION",ACTION.toString),
        MACComponent.Property("State",state.toString),
        MACComponent.Property("MFM mode",mfmMode.toString),
        MACComponent.Property("PRAM",pram.map(b => "%02X".format(b)).mkString(",")),
        MACComponent.Property("Track position",tp),
      ) ::: sp
  end getProperties

  private inline def ACTION : Boolean = (swimReg(MODE.ordinal) & 0x8) != 0
  private inline def isReadMode: Boolean = (swimReg(MODE.ordinal) & MODE_RW) == 0
  private inline def iwmMode: Boolean = (swimReg(MODE.ordinal) & MODE_ISM_IWM_SELECT) == 0

  // ========================= CRC =============================================
  private def updateCRC(byte:Int): Unit = crc = MFM.crc(byte,crc)
  private def resetCRC(): Unit =
    crc = MFM.CRC_RESET_VALUE
  // ====================== FIFO ===============================================
  private inline def isFIFOFull: Boolean = fifo.size > 1 // takes into account also crc appended bytes
  private inline def isFIFOEmpty: Boolean = fifo.isEmpty
  private inline def isFIFOHeadMarkByte: Boolean = (fifo.headOption.getOrElse(-1) & MARK_BYTE_MASK) != 0

  private def fifoEnqueue(b:Int,markByte:Boolean,crc:Boolean = false): Unit =
    if !crc then
      updateCRC(b)
    //println(s"Enqueuing $b mark=$markByte [CRC=$crc]")
    if crc || !isFIFOFull then
      fifo.enqueue(if markByte then b | MARK_BYTE_MASK else b)
    else
      writeErrorBits(ERROR_UNDERRUN)
      println("FIFO is FULL!!")

  private def fifoDequeue(): Int =
    if !isFIFOEmpty then
      fifo.dequeue()
    else
      println("FIFO is EMPTY!!")
      0
  private def clearFifo(): Unit = fifo.clear()
  // ====================== IWM changes ========================================
  private def saveDriveRegisterSelection(): Unit =
    val drs = driveRegisterSelection
    driveRegisterSelection = savedDriveRegisterSelection
    savedDriveRegisterSelection = drs
  /*
    The ISM/IWM bit selects which register set will be used. To select the ISM set, you must
    write to the GCR mode register four times in a row with this bit set to "1 ", "0", "1","1 ",
    respectively.
  */
  override protected def iwmModeRegisterWritten(mode:Int): Unit =
    val ismIwmSelect = ((mode & 0x40) >> 6) & 1
    iwmModeStateCounter match
      case 0 => if ismIwmSelect == 1 then iwmModeStateCounter += 1 else iwmModeStateCounter = 0
      case 1 => if ismIwmSelect == 0 then iwmModeStateCounter += 1 else iwmModeStateCounter = 0
      case 2 => if ismIwmSelect == 1 then iwmModeStateCounter += 1 else iwmModeStateCounter = 0
      case 3 =>
        if ismIwmSelect == 1 then
          log.info("SWIM: ISM mode set")
          println("ISM mode set")
          swimReg(MODE.ordinal) |= MODE_ISM_IWM_SELECT
          saveDriveRegisterSelection()

        iwmModeStateCounter = 0
  end iwmModeRegisterWritten

  override protected def getSelectedDrive: MacDrive =
    if iwmMode then
      super.getSelectedDrive
    else if driveSelected != -1 then drives(driveSelected) else null
  end getSelectedDrive
  
  // ===========================================================================
  private def initAction(): Unit =
    mfmInSync = false
    mfmBitCount = 0
    readShiftRegister = 0
    writeShiftRegisterBits = 0
    writeShiftRegister = 0
    writeCrcCounter = 0
    // TODO check
    mfmBitLengthCycles = macModel.clockRateMhz / 500_000 // 500 Kbit/s => 16 cycles per bit
    mfmBitCycles = 0
    diskListener.onFloppyModeChanged(getSelectedDrive.driveIndex,writing = !isReadMode)
    println(s"ACTION!: $mfmBitLengthCycles")
  end initAction

  /**
   * When ACTION is set, this register reads data from and writes data to the FIFO. If a mark byte
   * is read from this location, an error will occur (see Error register, bit 1). If there is still valid data to be
   * read when ACTION is not set, it can be read from the Mark register.
   * When ACTION is not set, two consecutive reads from this location will provide error correction
   * information (see the section on error correction).
   */
  private def readDataRegister: Int =
    //println("Read data register")
    if isFIFOEmpty then
      writeErrorBits(ERROR_OVERRUN)
      0xFF
    else
      val byte = fifoDequeue()
      if (byte & MARK_BYTE_MASK) != 0 then
        writeErrorBits(ERROR_MARK_BYTE_READ)
      byte & 0xFF
  end readDataRegister

  private def writeDataRegister(value:Int): Unit =
    //println(s"Write data register: $value")
    if isFIFOFull then
      writeErrorBits(ERROR_OVERRUN)
    else
      fifoEnqueue(value,markByte = false)
  end writeDataRegister

  private def readCorrectionRegister: Int =
    swimReg(CORRECTION.ordinal)

  private def readMarkRegister: Int =
    //println("Read mark register")
    if isFIFOEmpty then
      writeErrorBits(ERROR_OVERRUN)
      0xFF
    else
      val byte = fifoDequeue()
      byte & 0xFF
  end readMarkRegister

  private def writeMarkRegister(value:Int): Unit =
    //println(s"Write mark register: $value")
    if isFIFOFull then
      writeErrorBits(ERROR_OVERRUN)
    else
      fifoEnqueue(value, markByte = true)
      if value == MFM.DATA_MARK then
        writeCrcCounter += 1
        if writeCrcCounter == 3 then // start of an address or data block that needs CRC calculation
          crc = MFM.CRC_MARK // crc of A1 x 3
          writeCrcCounter = 0
          //println(s"CRC reset to $crc")
      else
        writeCrcCounter = 0
  end writeMarkRegister

  private def writeCRCRegister(): Unit =
    // will cause two bytes from the internal CRC generator to be written out instead of a regular data byte
    //println(s"Write CRC: $crc = [${(crc >> 8) & 0xFF} ${crc & 0xFF}]")
    fifoEnqueue((crc >> 8) & 0xFF,markByte = false,crc = true)
    fifoEnqueue(crc & 0xFF,markByte = false,crc = true)
  end writeCRCRegister

  private def writeIWMConfRegister(value:Int): Unit =
    swimReg(IWM_CONF.ordinal) = value
    println(s"IWM configuration register = $value")
  private def readPRAM(): Int =
    //println(s"Reading pram [$pramAddress] = ${pram(pramAddress)}")
    val value = pram(pramAddress)
    pramAddress = (pramAddress + 1) & 0xF
    value
  private def writePRAM(value:Int): Unit =
    //println(s"Writing PRAM [$pramAddress] = $value")
    pram(pramAddress) = value
    pramAddress = (pramAddress + 1) & 0xF

  /**
   * The Phase register controls the direction and state of the four phase lines. Bits 4-7 control the
   * direction of each phase line. Clearing a bit causes the line to be an input, while setting a bit
   * makes the line an output. Bits 0-3 reflect the state of the individual phase lines. If a phase line
   * is configured as an output, setting its corresponding state bit high or low sets the output level on
   * that pin high or low; if it's configured as an input, reading the bit shows the current level of the
   * signal connected to that pin.
   */
  private def readPhase: Int =
    var phase = driveRegisterSelection & 0xF0
    if (driveRegisterSelection & CA0_MASK) != 0 then phase |= 1 << 0
    if (driveRegisterSelection & CA1_MASK) != 0 then phase |= 1 << 1
    if (driveRegisterSelection & CA2_MASK) != 0 then phase |= 1 << 2
    if lstrb then phase |= 1 << 3
    phase

  private def writePhase(value:Int): Unit =
    driveRegisterSelection = (driveRegisterSelection & 0x0F) | (value & 0xF0)
    val outBits = value >> 4
    if (outBits & 1) != 0 then
      if (value & 1) != 0 then driveRegisterSelection |= CA0_MASK else driveRegisterSelection &= ~CA0_MASK
    if (outBits & 2) != 0 then
      if (value & 2) != 0 then driveRegisterSelection |= CA1_MASK else driveRegisterSelection &= ~CA1_MASK
    if (outBits & 4) != 0 then
      if (value & 4) != 0 then driveRegisterSelection |= CA2_MASK else driveRegisterSelection &= ~CA2_MASK
    if (outBits & 8) != 0 then
      val oldLstrb = lstrb
      lstrb = (value & 0x8) != 0
      if oldLstrb && !lstrb then
        sendDiskCommand()
  private def readSetup: Int =
    swimReg(SETUP.ordinal)
  private def writeSetup(value:Int): Unit =
    swimReg(SETUP.ordinal) = value
//    mfmMode = (value & SETUP_GCR) == 0
//    if !mfmMode then
//      println("GCR mode enabled (SETUP)")
//    else
//      println("MFM mode enabled (SETUP)")
//    println(s"FCLK/2 bit = ${(value & SETUP_FCLK) != 0}")
  end writeSetup

  private def writeMode(value:Int, modeZero:Boolean): Unit =
    //println(s"Writing mode: $value modeZero=$modeZero")
    if modeZero then
      // The pram counter is set to zero after any access is made to the Mode 0 register (register 6) or the chip is reset.
      pramAddress = 0
    val oldMode = swimReg(MODE.ordinal)
    if modeZero then
      swimReg(MODE.ordinal) &= ~value
    else
      swimReg(MODE.ordinal) |= value

    /* 0  Toggling the clear FIFO bit high then low clears the FIFO to begin a read or write operation,
          and initializes the CRC generator with its starting value. Since this value is different for
          reading or writing, the read/write mode bit must be set to the appropriate state before toggling
          the Clear AFO bit.
    */
    if (oldMode & MODE_CLEAR_FIFO) != 0 && (swimReg(MODE.ordinal) & MODE_CLEAR_FIFO) == 0 then
      clearFifo()
      resetCRC()
    // 1 Setting this bit along with bit 7 (MotorOn) will enable drive 1.
    // 2 Setting this bit along with bit 7 (MotorOn) will enable drive 2.
    if (swimReg(MODE.ordinal) & (MODE_ENABLE1 | MODE_ENABLE2 | MODE_MOTORON)) != 0 then
      if (swimReg(MODE.ordinal) & MODE_MOTORON) != 0 then
        (swimReg(MODE.ordinal) >> 1) & 3 match
          case 0 =>
            //println("NO drive selected!")
            driveSelected = -1
          case 1 =>
            driveSelected = 0
          case 2 =>
            driveSelected = 1
          case 3 =>
            println("BOTH drives selected!")
      else
        driveSelected = -1
    /* 3 Setting the ACTION bit to "1" starts a read or write operation. It should be set only after
         everything else has been set up. When writing, at least one byte of data should be written to
         the FIFO before this bit is set to prevent an underrun when the chip goes to fetch a byte from
         the [empty] FIFO. This bit will be cleared if an error occurs while in write mode, but not in
         read mode.
         NOTE: after setting ACTION on a read operation, the first byte that will be returned will be a mark byte (as defined in the section
         on MFM encoding). The search for the mark byte is invisible to the software since it is handled entirely by the SWIM chip.
    */
    if (oldMode & MODE_ACTION) == 0 && (swimReg(MODE.ordinal) & MODE_ACTION) != 0 then
      state = if isReadMode then Reading else Writing
      initAction()
      println(s"ACTION set. State = $state")
    else if (oldMode & MODE_ACTION) != 0 && (swimReg(MODE.ordinal) & MODE_ACTION) == 0 then
      state = Idle
      //println("ACTION cleared. State = Idle")
    // 4 This bit determines whether an operation will be a read (0) or write (1) operation.
    // 5 Sets the state of the HDSEL pin if the Q3* /HDSEL bit in the Setup register is set to "1 ".
    // Not used
    // 6 Clearing this bit switches to the IWM register set As long as this bit remains a "1" the ISM register set will stay selected.
    if (swimReg(MODE.ordinal) & MODE_ISM_IWM_SELECT) == 0 then
      log.info("SWIM: IWM mode set")
      println("IWM mode set")
      saveDriveRegisterSelection()
    // 7 Enables/disables the /ENBLl and /ENBL2 drive enables (assuming bit 1 or 2 is set).
    //   This bit must be set prior to setting ACTION and must not be cleared until after ACTION is cleared.
  end writeMode

  private def readStatus: Int =
    //println("Reading status ...")
    swimReg(MODE.ordinal)

  private def writeErrorBits(errorBits:Int): Unit =
    //println(s"Error bits set: $errorBits")
    if !errorRegisterLockedForWriting then
      errorRegisterLockedForWriting = true
      swimReg(ERROR.ordinal) |= errorBits
  /**
   * This register shows what kind of error has occurred. When any of the bits is set, the Error bit in the
   * Handshake register will also be set. Once one error bit is set, no other bits can be set until the register is
   * cleared. The register is cleared by either reading it or resetting the chip. This register must be cleared
   * before beginning a read or write operation.
   */
  private def readError: Int =
    //println("Reading error ...")
    val error = swimReg(ERROR.ordinal)
    swimReg(ERROR.ordinal) = 0
    errorRegisterLockedForWriting = false
    error
  private def readHandshake: Int =
    //println("Reading handshake ...")
    var hs = 0
    // 0 If set to 1 it indicates that the next byte to be read is a mark byte (i.e., has a dropped clock pulse).
    if isFIFOHeadMarkByte then hs |= HANDSHAKE_MARK_BYTE
    // 1 The CRC error bit is cleared to zero if the CRC generated on the bytes up to and including the
    //   byte about to be read is zero (meaning all the bytes are correct). It's set to 1 if the internal
    //   CRC is currently non-zero. The bit is usually checked when the second CRC byte is about to
    //   be read from the FIFO.
    if crc != 0 then hs |= HANDSHAKE_CRC_ERROR
    // 2 This bit returns the current state of the RDDATA input from the drive.
    // TODO ?
    val drive = getSelectedDrive
    if drive != null then
      if drive.getBitOnHead(getHead, moveAhead = false) then hs |= HANDSHAKE_RDDATA
    // 3 This bit returns the current state of the SENSE input.
    if drive != null then
      //println(s"Reading handshake sense ${driveRegisterSelection & 0xF}")
      if readDiskSense(drive) then hs |= HANDSHAKE_SENSE
    // 4 This bit is set to 1 if either the MotorOn bit in the mode register is a 1 or the timer is timing out
    if (swimReg(MODE.ordinal) & MODE_MOTORON) != 0 then hs |= HANDSHAKE_MOTORON
    // 5 If this bit is set, it indicates that one of the bits in the Error register is set. The bit is cleared by reading the Error register or when the chip is reset.
    if swimReg(ERROR.ordinal) != 0 then hs |= HANDSHAKE_ERROR
    // 6 In read mode, this bit indicates that the FIFO contains 2 bytes to be read. In write mode, it indicates that 2 bytes can be written to the FIFO.
    if isReadMode then
      if isFIFOFull then hs |= HANDSHAKE_FIFO_2EMPTY
    else
      if isFIFOEmpty then hs |= HANDSHAKE_FIFO_2EMPTY
    // 7 In read mode, this bit indicates that the FIFO contains at least 1 byte to be read. In write mode, it indicates that at least 1 byte can be written to the FIFO.
    if isReadMode then
      if !isFIFOEmpty then hs |= HANDSHAKE_FIFO_1OR2EMPTY
    else
      if !isFIFOFull then hs |= HANDSHAKE_FIFO_1OR2EMPTY
    hs

  final override def read(address:Int): Int =
    if iwmMode then
      super.read(address)
    else
      rwRegister(address >> 9,isRead = true)

  final override def write(address: Int, value: Int): Unit =
    if iwmMode then
      super.write(address, value)
    else
      rwRegister(address >> 9,isRead = false,value)

  private def rwRegister(a3a2a1a0: Int, isRead: Boolean, value: Int = -1): Int =
    if isRead then
      a3a2a1a0 & 0b0111 match
        case 0b000 => // read DATA/CORRECTION
          if ACTION then
            readDataRegister // ACTION = 1
          else
            readCorrectionRegister
        case 0b001 => // read MARK
          readMarkRegister
        case 0b011 => // read PRAM
          readPRAM()
        case 0b100 => // read PHASE
          readPhase
        case 0b101 => // read SETUP
          readSetup
        case 0b110 => // read STATUS
          readStatus
        case 0b010 => // read ERROR
          readError
        case 0b111 => // read HANDSHAKE
          readHandshake
        case r =>
          println(s"Reading unknown register: $r")
          0
    else
      a3a2a1a0 & 0b0111 match
        case 0b000 => // write DATA
          writeDataRegister(value)
        case 0b001 => // write MARK
          writeMarkRegister(value)
        case 0b010 => // write CRC/IWM Conf
          if ACTION then
            writeCRCRegister() // ACTION = 1
          else
            writeIWMConfRegister(value)
        case 0b011 => // write PRAM
          writePRAM(value)
        case 0b100 => // write PHASE
          writePhase(value)
        case 0b101 => // write SETUP
          writeSetup(value)
        case addr@(0b110 | 0b111) => // write MODE
            writeMode(value, (addr & 1) == 0)
        case r =>
          println(s"Writing unknown register: $r")
      0
    end if
  end rwRegister

  // ==================== Drive sense =================================
  override protected def readDiskSense(drive: MacDrive): Boolean =
    driveRegisterSelection & 0x0F match
      case 0b0000 /*/DIRTN*/ => // Head step directory: 0 = Up, 1 = Down
        !drive.isSteppingUp
      case 0b0001 /*/CISTN*/ => // Disk in place: 0 = disk in drive, 1 = no disk
        !drive.isFloppyIn
      case 0b0010 /*/STEP*/ => // Disk head stepping: 0 = head stepping, 1 = head not stepping
        !drive.isStepping
      case 0b0011 /*/WRTPRT*/ => // Disk write protect: 0 = disk write protected, 1 = not write protected
        drive.getFloppy match
          case Some(f) => !f.isWriteProtected
          case None => false
      case 0b0100 /*/MOTORON*/ => // Disk motor running: 0 = running, 1 = off
        !drive.isMotorOn
      case 0b0101 /*/TKO*/ => // Head at track 0: 0 = track 0, 1 = other track
        drive.getTrack > 0
      case 0b0110 /*EJECT*/ => // Disk ejecting: 0 = not ejecting, 1 = ejecting
        ejecting > 0
      case 0b0111 /*/TACH*/ =>
        drive.getTACH
      case 0b1000 /*RDDATA0*/ => // bit on lower head or if on index a pulse
        if macModel.ordinal < MacModel.SEFDHD.ordinal || isReadMode then // on SWIM seems to report INDEX pulse on writing
          drive.getBitOnHead(0, moveAhead = false)
        else
          drive.isOnIndexHole
      case 0b1001 /*RDDATA1*/ => // bit on upper head or if on index a pulse
        if macModel.ordinal < MacModel.SEFDHD.ordinal || isReadMode then
          drive.getBitOnHead(1, moveAhead = false)
        else
          drive.isOnIndexHole
      case 0b1010 /*SUPERDRIVE*/ =>
        macModel.ordinal >= MacModel.SEFDHD.ordinal
      case 0b1011 /*MFM ?*/ =>
        mfmMode // TODO
      case 0b1100 /*SIDES*/ => // Single/double sided drive: 0 = single, 1 = double
        doubleSide
      case 0b1101 /*/READY*/ => //TODO Disk ready: 0 = ready, 1 = not ready
        false
      case 0b1110 /*/INSTALLED*/ => //TODO Drive installed: 0 = installed, 1 = not installed
        !drive.present
      case 0b1111 /*/PRESENT*/ => //TODO PRESENT/HD
        if macModel.ordinal >= MacModel.SEFDHD.ordinal then
          !(drive.isFloppyIn && drive.getFloppy.get.getEncoding.hd)
        else
          !drive.present
      case _ =>
        log.warning("Reading unknown SWIM sense bit: %2X", driveRegisterSelection)
        println(s"Reading unknown SWIM sense bit: ${driveRegisterSelection & 15}")
        true
  end readDiskSense
  // ===================== Drive command ==================================
  override def sendDiskCommand(): Unit =
    //println(s"Disk command: ${driveRegisterSelection & 0x0F}")
    val drive = getSelectedDrive
    if drive != null then
      driveRegisterSelection & 0x0F match
        case 0b0000 /*TRACKUP*/ => drive.setStepDirection(stepUP = true)
        case 0b1000 /*TRACKDN*/ => drive.setStepDirection(stepUP = false)
        case 0b0010 /*TRACKSTEP*/ => drive.stepHead()
        case 0b0011 /*MFM MODE*/ =>
          mfmMode = true
          println("MFM mode on")
        case 0b0100 /*MOTORON*/ => drive.setMotorOn(on = true)
        case 0b1011 /*GCR MODE*/ =>
          mfmMode = false
          println("GCR mode on (command)")
        case 0b1100 /*MOTOROFF*/ =>
          if drive.isMotorOn then
            // check mode Motor-off timer bit
            if (swimReg(SETUP.ordinal) & SETUP_MOTORON_TIMER) != 0 then
              motorIsTurningOff = MOTOR_OFF_1_SEC_CYCLES
            else
              motorIsTurningOff = 1 // 1 cycle
            println(s"Motor is turning off: $motorIsTurningOff")
        case 0b1110 /*EJECT*/ =>
          ejecting = FDHD_EJECT_CYCLES
          ejectingDriveIndex = drive.driveIndex
        case c =>
          println(s"Unrecognized disk command: $c")
  end sendDiskCommand
  // ===================== Cycle ==========================================
  final override def cycle(): Unit =
    var cycle = swimCycles
    val drive = getSelectedDrive
    while drive != null && cycle > 0 do
      if iwmMode then
        super.cycle()
      else
        // motor off
        if motorIsTurningOff > 0 then
          motorIsTurningOff -= 1
          if motorIsTurningOff == 0 then
            drive.setMotorOn(on = false)
        // ejection
        if ejecting > 0 then
          ejecting -= 1
          if ejecting == 0 then
            ejectDisk()

        drive.cycle()
        if drive.isMotorOn then
          mfmBitCycles += 1
          if mfmBitCycles == mfmBitLengthCycles then // go to next bit
            mfmBitCycles = 0

            val head = getHead
            state match
              case Idle =>
                drive.getBitOnHead(head,moveAhead = true)
              case Reading =>
                reading(drive,head)
              case Writing =>
                writing(drive,head)
          end if
      cycle -= 1
    end while
  end cycle

  private def writing(drive:MacDrive,head:Int): Unit =
    if writeShiftRegisterBits == 0 then
      if isFIFOEmpty then
        writeErrorBits(ERROR_UNDERRUN)
      else
        val toBeWritten = fifoDequeue()
        if mfmMode then
          writeShiftRegister = MFM.byte2MFM(toBeWritten & 0xFF,isMark = (toBeWritten & MARK_BYTE_MASK) != 0)
          writeShiftRegisterBits = 16
        else
          writeShiftRegister = toBeWritten
          writeShiftRegisterBits = 8

    val bitToWrite = if mfmMode then
      (writeShiftRegister & 0x8000) == 0x8000
    else
      (writeShiftRegister & 0x80) == 0x80

    writeShiftRegister <<= 1
    drive.setBitOnHeadAndMoveAhead(head, bitToWrite)
    writeShiftRegisterBits -= 1
  end writing

  private def reading(drive:MacDrive,head:Int): Unit =
    val bit = drive.getBitOnHead(head,moveAhead = true)
    readShiftRegister <<= 1
    mfmBitCount += 1
    if bit then
      readShiftRegister |= 1

    if mfmMode then
      // =================== MFM MODE =====================================
      if !mfmInSync && (readShiftRegister & 0xFFFF) == MFM.MARK then
        readShiftRegister = 0
        mfmInSync = true
        mfmBitCount = 0
        mfmMarkCounter = 1

        fifoEnqueue(MFM.mfm2Byte(MFM.MARK),markByte = true)
      else if mfmInSync && mfmBitCount == 16 then
        val mfmBits = readShiftRegister & 0xFFFF
        readShiftRegister = 0
        mfmBitCount = 0

        val markByte = mfmBits == MFM.MARK || mfmBits == MFM.I_MARK
        val byte = MFM.mfm2Byte(mfmBits) // decode a MFM byte

        fifoEnqueue(byte,markByte = markByte)
        if markByte then
          mfmMarkCounter += 1
        else
          if sectorByteCounter > 0 then // waiting for a sector id byte ?
            sectorByteCounter -= 1
            if sectorByteCounter == 0 then
              diskListener.onSectorOnHead(drive.driveIndex,byte)
          if mfmMarkCounter == MFM.MARK_BYTE_SIZE && byte == MFM.ADDRESS_MARK_NEXT_BYTE then
            sectorByteCounter = MFM.SECTOR_BYTE_POS
          mfmMarkCounter = 0
      end if
    else
      // =================== GCR MODE =====================================
      // read a bit under head and go to next one
      val bit = drive.getBitOnHead(head,moveAhead = true)
      readShiftRegister <<= 1
      if bit then
        readShiftRegister |= 1
        
      if (readShiftRegister & 0x80) == 0x80 then
        val byte = readShiftRegister & 0xFF
        checkGCRSector(byte)
        fifoEnqueue(byte,markByte = false)
        readShiftRegister = 0
  end reading