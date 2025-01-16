package ucesoft.mac.storage

import scala.collection.mutable

/**
 * @author Alessandro Abbruzzetti
 *         Created on 08/01/2025 15:13
 *
 * Based on:
 *  1. SWIM_Chip_Users_Ref_198801
 *  2. SonyMFM.a, SonyEqu.a : https://github.com/elliotnunn/supermario/tree/master
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

  private inline val MARK_BYTE_MASK = 0x100

  private inline val FDHD_EJECT_CYCLES = 1 // eject immediately ?

  private var errorRegisterLockedForWriting = false

  private final val swimReg = Array.ofDim[Int](REGISTER.values.length)
  private final val fifo = new mutable.Queue[Int]()

  private final val pram = Array.ofDim[Int](16)
  private var pramAddress = 0

  private var iwmMode = true
  private var iwmModeStateCounter = 0

  private var mfmMode = true

  private var crc = 0xFFFF
  private var crcWritePending = false // will cause two bytes from the internal CRC generator to be written out instead of a regular data byte

  import REGISTER.*

  private inline def ACTION : Boolean = (swimReg(MODE.ordinal) & 0x8) != 0
  private inline def isReadMode: Boolean = (swimReg(MODE.ordinal) & MODE_RW) == 0

  // ========================= CRC =============================================
  private def updateCRC(byte:Int): Unit = crc = MFM.crc(byte,crc)
  private def resetCRC(): Unit =
    crc = 0xFFFF
    // TODO different value for reading or writing ??
  // ====================== FIFO ===============================================
  private inline def isFIFOFull: Boolean = fifo.size == 2
  private inline def isFIFOEmpty: Boolean = fifo.isEmpty
  private inline def isFIFOHeadMarkByte: Boolean = (fifo.headOption.getOrElse(-1) & MARK_BYTE_MASK) != 0

  private def fifoEnqueue(b:Int,markByte:Boolean): Unit =
    if !isFIFOFull then
      fifo.enqueue(if markByte then b | MARK_BYTE_MASK else b)
    else
      println("FIFO is FULL!!")
  private def fifoDequeue(): Int =
    if !isFIFOEmpty then
      fifo.dequeue()
    else
      println("FIFO is EMPTY!!")
      0
  private def clearFifo(): Unit = fifo.clear()
  // ====================== IWM changes ========================================
  /*
    The ISM/IWM bit selects which register set will be used. To select the ISM set, you must
    write to the GCR mode register four times in a row with this bit set to "1 ", "0", "1","1 ",
    respectively.
  */
  override protected def iwmModeRegisterWritten(mode:Int): Unit =
    val ismIwmSelect = ((mode & 0x40) >> 5) & 1
    iwmModeStateCounter match
      case 0 => if ismIwmSelect == 1 then iwmModeStateCounter += 1 else iwmModeStateCounter = 0
      case 1 => if ismIwmSelect == 0 then iwmModeStateCounter += 1 else iwmModeStateCounter = 0
      case 2 => if ismIwmSelect == 1 then iwmModeStateCounter += 1 else iwmModeStateCounter = 0
      case 3 =>
        if ismIwmSelect == 1 then
          log.info("SWIM: ISM mode set")
          println("ISM mode set")
          iwmMode = false
        iwmModeStateCounter = 0
  end iwmModeRegisterWritten

  override protected def getSelectedDrive: MacDrive =
    if iwmMode then
      super.getSelectedDrive
    else if (swimReg(MODE.ordinal) & (MODE_ENABLE1 | MODE_MOTORON)) == (MODE_ENABLE1 | MODE_MOTORON) then drives(0)
    else if (swimReg(MODE.ordinal) & (MODE_ENABLE2 | MODE_MOTORON)) == (MODE_ENABLE2 | MODE_MOTORON) then drives(1)
    else
      println(s"getSelectedDrive cannot determine the selected drive mode=${swimReg(MODE.ordinal)}")
      drives(0)
  end getSelectedDrive
  // ===========================================================================
  private def initAction(): Unit = ???

  private def readDataRegister: Int = ???
  private def writeDataRegister(value:Int): Unit = ???
  private def readCorrectionRegister: Int = ???
  private def readMarkRegister: Int = ???
  private def writeMarkRegister(value:Int): Unit = ???
  private def writeCRCRegister(): Unit =
    crcWritePending = true
  private def writeIWMConfRegister(value:Int): Unit = ???
  private def readPRAM(): Int =
    val value = pram(pramAddress)
    pramAddress = (pramAddress + 1) % 16
    value
  private def writePRAM(value:Int): Unit =
    pram(pramAddress) = value
    pramAddress = (pramAddress + 1) % 16

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
    if (outBits & 8) != 0 && !lstrb then
      lstrb = (value & 0x8) != 0
      if !lstrb then
        sendFDHDDiskCommand()
  private def readSetup: Int =
    swimReg(SETUP.ordinal)
  private def writeSetup(value:Int): Unit = ???
  private def writeMode(value:Int, modeZero:Boolean): Unit =
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
    /* 3 Setting the ACTION bit to "1" starts a read or write operation. It should be set only after
         everything else has been set up. When writing, at least one byte of data should be written to
         the FIFO before this bit is set to prevent an underrun when the chip goes to fetch a byte from
         the [empty] FIFO. This bit will be cleared if an error occurs while in write mode, but not in
         read mode.
         NOTE: after setting ACTION on a read operation, the first byte that will be returned will be a mark byte (as defined in the section
         on MFM encoding). The search for the mark byte is invisible to the software since it is handled entirely by the SWIM chip.
    */
    if (swimReg(MODE.ordinal) & MODE_ACTION) != 0 then
      initAction()
    // 4 This bit determines whether an operation will be a read (0) or write ( 1) operation.
    diskListener.onFloppyModeChanged(getSelectedDrive.driveIndex,writing = (swimReg(MODE.ordinal) & MODE_RW) != 0)
    // 5 Sets the state of the HDSEL pin if the Q3* /HDSEL bit in the Setup register is set to "1 ".
    // Not used
    // 6 Clearing this bit switches to the IWM register set As long as this bit remains a "1" the ISM register set will stay selected.
    if (swimReg(MODE.ordinal) & MODE_ISM_IWM_SELECT) == 0 then
      log.info("SWIM: ISM mode set")
      println("ISM mode set")
      iwmMode = false
    // 7 Enables/disables the /ENBLl and /ENBL2 drive enables (assuming bit 1 or 2 is set).
    //   This bit must be set prior to setting ACTION and must not be cleared until after ACTION is cleared.
  end writeMode

  private def readStatus: Int =
    swimReg(MODE.ordinal)

  private def writeErrorBits(errorBits:Int): Unit =
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
    val error = swimReg(ERROR.ordinal)
    swimReg(ERROR.ordinal) = 0
    errorRegisterLockedForWriting = false
    error
  private def readHandshake: Int =
    var hs = 0
    // 0 If set to 1 it indicates that the next byte to be read is a mark byte (i.e., has a dropped clock pulse).
    if isFIFOHeadMarkByte then hs |= HANDSHAKE_MARK_BYTE
    // 1 The CRC error bit is cleared to zero if the CRC generated on the bytes up to and including the
    //   byte about to be read is zero (meaning all the bytes are correct). It's set to 1 if the internal
    //   CRC is currently non-zero. The bit is usually checked when the second CRC byte is about to
    //   be read from the FIFO.
    if crc != 0 then hs |= HANDSHAKE_CRC_ERROR
    // 2 This bit returns the current state of the RDDATA input from the drive.
    // TODO
    // 3 This bit returns the current state of the SENSE input.
    if readSWIMSense(getSelectedDrive) then hs |= HANDSHAKE_SENSE
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

  private def rwRegister(a3a2a1a0:Int,isRead:Boolean,value:Int = -1): Option[Int] =
    a3a2a1a0 & 0b1111 match
      case 0b1_000 => // read DATA/CORRECTION
        if isRead then
          if ACTION then
            Some(readDataRegister) // ACTION = 1
          else
            Some(readCorrectionRegister)
        else
          println("Reading data/correction register with write operation")
          None
      case 0b0_000 => // write DATA
        if !isRead then
          writeDataRegister(value)
        else
          println("Writing data register with read operation")
        None
      case 0b1_001 => // read MARK
        if isRead then
          Some(readMarkRegister)
        else
          println("Reading mark register with write operation")
          None
      case 0b0_001 => // write MARK
        if !isRead then
          writeMarkRegister(value)
        else
          println("Writing mark register with read operation")
        None
      case 0b0_010 => // write CRC/IWM Conf
        if !isRead then
          if ACTION then
            writeCRCRegister() // ACTION = 1
          else
            writeIWMConfRegister(value)
        else
          println("Writing crc/iwmconf register with read operation")
        None
      case 0b1_011 => // read PRAM
        if isRead then
          Some(readPRAM())
        else
          println("Reading pram with write operation")
          None
      case 0b0_011 => // write PRAM
        if !isRead then
          writePRAM(value)
        else
          println("Writing pram register with read operation")
        None
      case 0b1_100 => // read PHASE
        if isRead then
          Some(readPhase)
        else
          println("Reading phase register with write operation")
          None
      case 0b0_100 => // write PHASE
        if !isRead then
          writePhase(value)
        else
          println("Writing phase register with read operation")
        None
      case 0b1_101 => // read SETUP
        if isRead then
          Some(readSetup)
        else
          println("Reading setup register with write operation")
          None
      case 0b0_101 => // write SETUP
        if !isRead then
          writeSetup(value)
        else
          println("Writing setup register with read operation")
        None
      case addr@(0b0_110 | 0b0_111) => // write MODE
        if !isRead then
          writeMode(value,(addr & 1) == 0)
        else
          println("Writing mode register with read operation")
        None
      case 0b1_110 => // read STATUS
        if isRead then
          Some(readStatus)
        else
          println("Reading status register with write operation")
          None
      case 0b1_010 => // read ERROR
        if isRead then
          Some(readError)
        else
          println("Reading error register with write operation")
          None
      case 0b1_111 => // read HANDSHAKE
        if isRead then
          Some(readHandshake)
        else
          println("Reading error register with write operation")
          None
      case addr =>
        println(s"Accessing unknown register: $addr")
        None
  end rwRegister

  // ==================== Drive sense =================================
  private def readSWIMSense(drive: MacDrive): Boolean =
    driveRegisterSelection match
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
      case 0b0111 /*/TACH*/ => // on SWIM seems to report INDEX pulse
        drive.isOnIndexHole
      case 0b1000 /*RDDATA0*/ => // bit on lower head or if on index a pulse
        drive.getBitOnHead(0, moveAhead = false) || drive.isOnIndexHole
      case 0b1001 /*RDDATA1*/ => // bit on upper head or if on index a pulse
        drive.getBitOnHead(1, moveAhead = false) || drive.isOnIndexHole
      case 0b1010 /*SUPERDRIVE*/ =>
        true
      case 0b1100 /*SIDES*/ => // Single/double sided drive: 0 = single, 1 = double
        true
      case 0b1101 /*/READY*/ => //TODO Disk ready: 0 = ready, 1 = not ready
        false
      case 0b1110 /*/INSTALLED*/ => //TODO Drive installed: 0 = installed, 1 = not installed
        !drive.present
      case 0b1111 /*PRESENT*/ => //TODO PRESENT/HD
        false
      case _ =>
        log.warning("Reading unknown SWIM sense bit: %2X", driveRegisterSelection)
        true
  end readSWIMSense
  // ===================== Drive command ==================================
  private def sendFDHDDiskCommand(): Unit =
    val drive = getSelectedDrive
    driveRegisterSelection match
      case 0b0000 /*TRACKUP*/ => drive.setStepDirection(stepUP = true)
      case 0b1000 /*TRACKDN*/ => drive.setStepDirection(stepUP = false)
      case 0b0010 /*TRACKSTEP*/ => drive.stepHead()
      case 0b0011 /*MFM MODE*/ =>
        mfmMode = true
        println("MFM mode on")
      case 0b0100 /*MOTORON*/ => drive.setMotorOn(on = true)
      case 0b1011 /*GCR MODE*/ =>
        mfmMode = false
        println("GCR mode on")
      case 0b1100 /*MOTOROFF*/ =>
        if drive.isMotorOn then
          // check mode Motor-off timer bit
          if (swimReg(SETUP.ordinal) & SETUP_MOTORON_TIMER) != 0 then
            motorIsTurningOff = MOTOR_OFF_1_SEC_CYCLES
          else
            motorIsTurningOff = 1 // 1 cycle
      case 0b1110 /*EJECT*/ =>
        ejecting = FDHD_EJECT_CYCLES
        ejectingDriveIndex = drive.driveIndex