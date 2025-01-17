package ucesoft.mac.storage

import ucesoft.mac.MacModel.MAC128K
import ucesoft.mac.storage.DiskController.DiskControllerListener
import ucesoft.mac.{MACComponent, MacModel, MessageBus}

import javax.swing.ImageIcon

/*
 * @author Alessandro Abbruzzetti
 *         Created on 05/11/2024 18:59
 *
 *         Line	    Enable address	Disable address	  Purpose
 *         ===================================================================================================
 *         CA0	    0xDFE3FF	      0xDFE1FF	        Drive register selection
 *         CA1	    0xDFE7FF	      0xDFE5FF	        Drive register selection
 *         CA2	    0xDFEBFF	      0xDFE9FF	        Drive register selection
 *         LSTRB	  0xDFEFFF	      0xDFEDFF	        Drive register write strobe
 *         ENABLE	  0xDFF3FF	      0xDFF1FF	        Drive enable, required for many drive commands.
 *         SELECT	  0xDFF7FF	      0xDFF5FF	        Drive select
 *                                                    Selects internal (off) or external (on) disk drive.
 *                                                    Note that the Mac 128K/512K can only address one drive, due to the PWM speed control.
 *         Q6	      0xDFFBFF	      0xDFF9FF	        Selects which IWM register to access
 *         Q7	      0xDFFFFF	      0xDFFDFF	        Selects which IWM register to access
 *         HEADSEL	Through VIA	    Through VIA	      Two purposes: selects the drive head to read/write data from either side of the disk (in double-sided drives) and plays part in drive register selection.
 */
class IWM extends DiskController:
  override protected val componentName = "IWM"
  override protected val icon = new ImageIcon(getClass.getResource("/resources/trace/iwm.png"))

  protected var MOTOR_OFF_1_SEC_CYCLES = 0 // see setModel
  private var EJECT_LSTRB_ACTIVE_CYCLES = 0 // see setModel
  // driveRegisterSelection's bits
  inline private val SEL_MASK = 0b0001
  inline protected val CA0_MASK = 0b0010
  inline protected val CA1_MASK = 0b0100
  inline protected val CA2_MASK = 0b1000
  // iwmRegisterSelection's bits
  inline private val Q7_MASK = 0b001
  inline private val Q6_MASK = 0b010
  inline private val EN_MASK = 0b100
  // handshake register's bits
  inline private val HS_READY_FOR_DATA_MASK = 0b10000000
  inline private val HS_BUFFER_UNDERRUN_MASK = 0b01000000
  // mode Motor-off timer bit
  inline private val MODE_MOTOR_OFF_TIMER_MASK = 0b00000100

  protected var driveRegisterSelection = 0 // CA2 CA1 CA0 SEL
  private var iwmRegisterSelection = 0   // ENABLE Q6 Q7
  protected var lstrb = false
  private var enable = false
  private var externalDrive = false
  private var internalDriveSE = false
  private var headsel = false
  private var mode = 0
  // number of cycles to eject a disk
  protected var ejecting = 0
  // drive index of the ejecting floppy
  protected var ejectingDriveIndex = 0
  // number of cycles to turn off the motor
  protected var motorIsTurningOff = 0

  // current bit cycles
  private var bitCycles = 0
  // number of cycles per bit
  private var bitLengthCycles = 16
  // byte read
  private var dataRegister = 0
  // byte reading
  private var readShiftRegister = 0
  // byte to write
  private var writeDataRegister = 0
  // true = writeDataRegister filled with a byte to write
  private var writeDataRegisterFilled = false
  // writing byte
  private var writeShiftRegister = 0
  // bits to write
  private var writeShiftRegisterBits = 0
  // used to recognize a sector number
  private var sectorShiftRegister = 0
  private var sectorByteCounter = 0
  
  private var lastModeWasWriting = false

  private var doubleSide = false

  private var eventListeners: List[DiskControllerListener] = Nil

  protected val diskListener = new DiskControllerListener:
    override def onHeadSelected(driveIndex:Int,head:Int): Unit =
      eventListeners.foreach(_.onHeadSelected(driveIndex,head))
    override def onTrackChanged(driveIndex: TrackPos, track: TrackPos): Unit =
      eventListeners.foreach(_.onTrackChanged(driveIndex, track))
    override def onSectorOnHead(driveIndex: TrackPos, sector: TrackPos): Unit =
      eventListeners.foreach(_.onSectorOnHead(driveIndex, sector))
    override def onMotorChanged(driveIndex: TrackPos, on: Boolean): Unit =
      eventListeners.foreach(_.onMotorChanged(driveIndex, on))
    override def onFloppyInserted(driveIndex: Int, image: DiskImage): Unit =
      eventListeners.foreach(_.onFloppyInserted(driveIndex, image))
    override def onFloppyEjected(driveIndex: Int): Unit =
      eventListeners.foreach(_.onFloppyEjected(driveIndex))
    override def onFloppyModeChanged(driveIndex: TrackPos, writing: Boolean): Unit =
      eventListeners.foreach(_.onFloppyModeChanged(driveIndex,writing))

  protected var drives : Array[MacDrive] = Array()

  setModel(MAC128K)

  override def onMessage(msg: MessageBus.Message): Unit =
    msg match
      case MessageBus.Shutdown(_,_)=>
        drives.foreach(_.ejectFloppy())
      case _ =>

  override protected def reset(): Unit = 
    super.reset()
    driveRegisterSelection = 0
    iwmRegisterSelection = 0
    lstrb = false
    externalDrive = false
    internalDriveSE = false
    headsel = false
    mode = 0
    ejecting = 0
    motorIsTurningOff = 0
    bitCycles = 0
    dataRegister = 0
    readShiftRegister = 0
    writeDataRegister = 0
    writeDataRegisterFilled = false
    writeShiftRegister = 0
    writeShiftRegisterBits = 0
    lastModeWasWriting = false
    getAndResetByteAccessed

  override protected def setModel(model: MacModel): Unit =
    super.setModel(model)
    val clockSpeed = model.clockRateMhz
    MOTOR_OFF_1_SEC_CYCLES = clockSpeed // 1 sec
    EJECT_LSTRB_ACTIVE_CYCLES = clockSpeed / 1000 * 550 // < 750 ms (max time for lstrb)

    doubleSide = model.floppySettings.doubleDensity
    val oldDrives = drives
    drives = Array.ofDim[MacDrive](3)
    for i <- drives.indices do
      drives(i) = new MacDrive(driveIndex = i,clockSpeed, doubleSide = doubleSide, present = i < model.floppySettings.drivesNumber,trackChangeListener = diskListener)
      drives(i).setComponentModel(model)
      if i < oldDrives.length && oldDrives(i).getFloppy.isDefined then
        drives(i).insertFloppy(oldDrives(i).getFloppy.get)

    log.info("IWM set model to %s: configuring %d %s drives",model.toString,model.floppySettings.drivesNumber,if doubleSide then "double side" else "single side")

  override def getProperties: List[MACComponent.Property] =
    import MACComponent.Property
    def b2i(b:Int):0|1 = if b != 0 then 1 else 0
    val common = List(
      Property("Drive register selection",s"CA2=${b2i(driveRegisterSelection & CA2_MASK)} CA1=${b2i(driveRegisterSelection & CA1_MASK)} CA0=${b2i(driveRegisterSelection & CA0_MASK)} SEL=${b2i(driveRegisterSelection & SEL_MASK)}"),
      Property("IWM register selection",s"ENABLE=${b2i(iwmRegisterSelection & EN_MASK)} Q6=${b2i(iwmRegisterSelection & Q6_MASK)} Q7=${b2i(iwmRegisterSelection & Q7_MASK)}"),
      Property("External drive",externalDrive.toString),
      Property("LSTRB",lstrb.toString),
      Property("Mode","%02X".format(mode)),
      Property("Selected drive",getSelectedDrive.driveIndex.toString),
      Property("Motor on",getSelectedDrive.isMotorOn.toString)
    )
    val drivesProp = for d <- drives.toList yield
      List(
        Property(s"Drive #${d.driveIndex} floppy",d.getFloppy.map(_.diskName).getOrElse("-")),
        Property(s"Drive #${d.driveIndex} track",d.getTrack.toString),
        Property(s"Drive #${d.driveIndex} floppy modified", d.getFloppy.exists(_.isModified).toString)
      )

    common ::: drivesProp.flatten

  override def addDiskControllerListener(l:DiskControllerListener): Unit = eventListeners ::= l
  override def removeDiskControllerListener(l:DiskControllerListener): Unit = eventListeners = eventListeners.filterNot(_ == l)

  override def setInternalDriveSE(set:Boolean): Unit =
    internalDriveSE = set

  override def setHeadSelLine(set:Boolean): Unit =
    headsel = set
    diskListener.onHeadSelected(getSelectedDrive.driveIndex,if set then 1 else 0)
    if set then
      driveRegisterSelection |= SEL_MASK
    else
      driveRegisterSelection &= ~SEL_MASK

  inline private def getHead: Int =
    if !doubleSide || getSelectedDrive.getFloppySides == 1 || !headsel then 0 else 1

  protected def getSelectedDrive: MacDrive =
    if externalDrive then
      drives(1)
    else if internalDriveSE then
      drives(2)
    else
      drives(0)

  /*
    A23 A22 A21 A20 ...
     1   1   0   X
    {A12, A11, A10} select one of 8 bits to access
    A9 indicates if the selected bit must be put on (1) or off (0)
   */
  private def access(address:Int): Unit =
    val bitSet = (address & 0x200) != 0
    (address >> 10) & 7 match
      case 0 => if bitSet then driveRegisterSelection |= CA0_MASK else driveRegisterSelection &= ~CA0_MASK
      case 1 => if bitSet then driveRegisterSelection |= CA1_MASK else driveRegisterSelection &= ~CA1_MASK
      case 2 => if bitSet then driveRegisterSelection |= CA2_MASK else driveRegisterSelection &= ~CA2_MASK
      case 3 =>
        lstrb = bitSet
        if !lstrb && ejecting > 0 then
          ejecting = 0 // Disk ejecting: Note that the LSTRB signal must remain active for 750ms for the eject to occur
        if lstrb /*&& enable*/ then
          sendDiskCommand()
      case 4 =>
        enable = bitSet
        if enable then iwmRegisterSelection |= EN_MASK else iwmRegisterSelection &= ~EN_MASK
      case 5 => externalDrive = bitSet
      case 6 => if bitSet then iwmRegisterSelection |= Q6_MASK else iwmRegisterSelection &= ~Q6_MASK
      case 7 => if bitSet then iwmRegisterSelection |= Q7_MASK else iwmRegisterSelection &= ~Q7_MASK
    //println("IWM access: driveRegisterSelection=%d iwmRegisterSelection=%d".format(driveRegisterSelection,iwmRegisterSelection))
  end access

  /*
    CA2	CA1	CA0	HEADSEL	Command	  Description
    =====================================================================================
    off	off	off	off	    TRACKUP	  Sets head movement direction to higher tracks
    on	off	off	off	    TRACKDN	  Sets head movement direction to lower tracks
    off	off	on	off	    TRACKSTEP	Moves the drive head one track in the selected direction. A step may take up to 30 ms for the head to settle. If the destination track is in a different speed group, it can take up to 150 ms for the motor to reach the proper speed.
    off	on	off	off	    MOTORON	  Enables the spindle motor. Note that the spindle motor will not actually run unless there is a disk in the drive. The motor will be running at the proper speed within 400 ms.
    on	on	off	off	    MOTOROFF	Stops the spindle motor.
    on	on	on	off	    EJECT	    Ejects the disk. Note that the LSTRB signal must remain active for 750ms for the eject to occur.*

    *: the Macintosh Plus briefly asserts eject during boot for unknown reasons.
       This does not cause an actual eject on hardware, but re-emphasizes the need to observe the 750ms in an emulator.
   */
  private def sendDiskCommand(): Unit =
    val drive = getSelectedDrive
    driveRegisterSelection match
      case 0b0000 /*TRACKUP*/ => drive.setStepDirection(stepUP = true)
      case 0b1000 /*TRACKDN*/ => drive.setStepDirection(stepUP = false)
      case 0b0010 /*TRACKSTEP*/ => drive.stepHead()
      case 0b0100 /*MOTORON*/ => drive.setMotorOn(on = true)
      case 0b1100 /*MOTOROFF*/ =>
        if drive.isMotorOn then
          // check mode Motor-off timer bit
          if (mode & MODE_MOTOR_OFF_TIMER_MASK) == 0 then
            motorIsTurningOff = MOTOR_OFF_1_SEC_CYCLES
          else
            motorIsTurningOff = 1 // 1 cycle
      case 0b1110 /*EJECT*/ =>
        ejecting = EJECT_LSTRB_ACTIVE_CYCLES
        ejectingDriveIndex = drive.driveIndex
  end sendDiskCommand

  /*
      CA2 CA1 CA0 SEL  STAT35   Function
      --- --- --- ---  ------   --------
      off off off off   $00     Step direction.
                                  0 = head set to step inward
                                    (toward higher-numbered tracks)
                                  1 = head set to step outward
                                    (toward lower-numbered tracks)
      off off off on    $02	  Disk in place.
                                  0 = disk in drive
                                  1 = drive is empty.
      off off on  off   $04	  Disk is stepping.
                                  0 = head is stepping between tracks
                                  1 = head is not stepping.
      off off on  on    $06	  Disk locked.
                                  0 = disk is write protected
                                  1 = disk is write-enabled.
      off on  off off   $08	  Motor on.
                                  0 = spindle motor is spinning
                                  1 = motor is off
      off on  off on    $0A	  Track 0.
                                  0 = head is at track 0
                                  1 = head is at some other track
                                  This bit becomes valid beginning 12 msec
                                  after the step that places the head at
                                  track 0.
      off on  on  off   $0C	 *Disk switched?
                                  0 = user ejected disk by pressing
                                    the eject button
                                  1 = disk not ejected.
      off on  on  on    $0E	  Tachometer.  60 pulses per disk revolution
      on  off off off   $01	  Instantaneous data from lower head.  Reading
                              this bit configures the drive to do I/O with
                              the lower head.
      on  off off on    $03	  Instantaneous data from upper head.  Reading
                              this bit configures the drive to do I/O with
                              the upper head.
      on  on  off off   $09	  Number of sides.
                                0 = single-sided drive
                                1 = double-sided drive
      on  on  off on    $0B	 *Disk ready for reading?
                                0 = ready
                                1 = not ready
                              I am not too sure about this bit.  The
                              firmware waits for this bit to go low
                              before trying to read a sector address
                              field.
      on  on  on  on    $0F	 Drive installed.
                                0 = drive is connected
                                1 = no drive is connected
   */
  private def readSense(drive:MacDrive): Boolean =
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
      case 0b0111 /*/TACH*/ => // Tachometer: 60 pulses/revolution
        drive.getTACH
      case 0b1000 /*RDDATA0*/ => // bit on lower head
        drive.getBitOnHead(0,moveAhead = false)
      case 0b1001 /*RDDATA1*/ => // bit on upper head
        drive.getBitOnHead(1,moveAhead = false)
      case 0b1100 /*SIDES*/ => // Single/double sided drive: 0 = single, 1 = double
        doubleSide
      case 0b1101 /*/READY*/ => //TODO Disk ready: 0 = ready, 1 = not ready
        false
      case 0b1110 /*/INSTALLED*/ => //TODO Drive installed: 0 = installed, 1 = not installed
        !drive.present
      case 0b1111 /*PRESENT*/ => //TODO PRESENT/HD
        !drive.present
      case _ =>
        log.warning("Reading unknown sense bit: %2X",driveRegisterSelection)
        true
  end readSense

  /*
    ENABLE	Q6	Q7	Read/Write	Register addressed
      off   off off     R         Read 0xFF
      on	  off	off	    R	        Data register (reading track)
      X 	  on	off	    R	        Status register
      X 	  off	on	    R	        Handshake register
   */
  private def readIWMRegister: Int =
    if iwmRegisterSelection == 0 then // Read 0xFF
      0xFF
    else if iwmRegisterSelection == EN_MASK then // Data register (reading track)
      val dr = dataRegister & 0xFF
      incProbeBytes()
      dataRegister = 0 // reading data register clear it
      dr
    else if (iwmRegisterSelection & (Q6_MASK | Q7_MASK)) == Q6_MASK then // Status register
      /*
        STATUS REGISTER
        BIT       DESCRIPTION
        =======================================
        7         SENSE. The content of the drive register that was inquired
        6         Reserved
        5         ENABLE line status. 0 = enable off, 1 = enable on
        4-0       Same as bit 4-0 of the mode register
       */
      var status = mode & 0x1F
      if readSense(getSelectedDrive) then status |= 0x80
      if enable then status |= 0x20
      log.info("IWM reading status register: %02X",status)
      status
    else if (iwmRegisterSelection & (Q6_MASK | Q7_MASK)) == Q7_MASK then // Handshake register
      /*
          7   6   5   4   3   2   1   0
        +---+---+---+---+---+---+---+---+
        | B | U | R | R | R | R | R | R |
        +---+---+---+---+---+---+---+---+

        Bit	Function
        ---	--------
         B	Register Ready
            0 = IWM is busy
            1 = IWM is ready for data
         U	Under-run
            0 = write under-run has occurred (the program took
                too long to write the next byte)
            1 = no under-run
         R	Reserved.
       */
      var handshakeRegister = 0
      // Ready for data: this flag is set as soon as the data register content is moved to the shift register,
      // signalling that the write data register needs to be filled with the next byte.
      if !writeDataRegisterFilled then handshakeRegister |= 0x80
      // Underrun: if the shift register is empty and there is no new byte waiting in the data register,
      // writing stops and the underrun flag is set.
      if !(!writeDataRegisterFilled && writeShiftRegisterBits == 0) then handshakeRegister |= 0x40
      handshakeRegister
    else
      log.warning("IWM reading an unknown register: %2X",iwmRegisterSelection)
      0
  end readIWMRegister

  /*
    ENABLE	Q6	Q7	Read/Write	Register addressed
      on	  on	on	    W	        Data register (writing track)
      off	  on	on	    W	        Mode register
   */
  private def writeIWMRegister(value:Int): Unit =
    if iwmRegisterSelection == (EN_MASK | Q6_MASK | Q7_MASK) then // Data register (writing track)
      if writeDataRegisterFilled then
        log.warning("IWM write data register written when was already filled")
      writeDataRegister = value & 0xFF
      incProbeBytes()
      //println("Writing %02X".format(writeDataRegister))
      writeDataRegisterFilled = true
    else if iwmRegisterSelection == (Q6_MASK | Q7_MASK) then // Mode register
      /*
          7   6   5   4   3   2   1   0
        +---+---+---+---+---+---+---+---+
        | R | R | R | S | C | M | H | L |
        +---+---+---+---+---+---+---+---+

      	Bit	Function
        ---	--------
         R	Reserved
         S	Clock speed:
            0 = 7 MHz
            1 = 8 MHz
          Should always be 0.
         C	Bit cell time:
            0 = 4 usec/bit (for 5.25 drives)
            1 = 2 usec/bit (for 3.5 drives)
         M	Motor-off timer:
            0 = leave drive on for 1 sec after program turns
                it off
            1 = no delay
          Should be 0 for 5.25 and 1 for 3.5.
         H	Handshake protocol:
            0 = synchronous (software must supply proper
                timing for writing data)
            1 = asynchronous (IWM supplies timing)
          Should be 0 for 5.25 and 1 for 3.5.
         L	Latch mode:
            0 = read-data stays valid for about 7 usec
            1 = read-data stays valid for full byte time
          Should be 0 for 5.25 and 1 for 3.5.
       */
      mode = value
      // check bit length in clock cycles
      bitLengthCycles = mode & 0x18 match
        case 0x00 => 28 // slow mode, 7Mhz
        case 0x08 => 14 // fast mode, 7Mhz
        case 0x10 => 32 // slow mode, 8Mhz
        case 0x18 => 16 // fast mode, 8Mhz
      log.info("IWM set mode to: mode=%2X bitLen=%d",mode,bitLengthCycles)
    else
      log.warning("IWM writing to unknown register: %2X",iwmRegisterSelection)

    iwmModeRegisterWritten(mode)
  end writeIWMRegister

  protected def iwmModeRegisterWritten(mode:Int): Unit = {}

  private def ejectDisk(): Unit =
    log.info("Disk %d ejecting ...",ejectingDriveIndex)
    getSelectedDrive.ejectFloppy()
    diskListener.onFloppyEjected(ejectingDriveIndex)
    diskListener.onFloppyEjected(ejectingDriveIndex)

  override def insertFloppy(driveIndex:Int,floppy:DiskImage): Boolean =
    if driveIndex > drives.length || !drives(driveIndex).present then
      throw new IllegalArgumentException(s"Drive #$driveIndex not present or configured: can't insert a floppy")
    if drives(driveIndex).insertFloppy(floppy) then
      diskListener.onFloppyInserted(driveIndex,floppy)
      log.info("Inserted floppy %s into drive #%d",floppy.diskName,driveIndex)
      true
    else
      false

  override def updatePWMSample(pwm:Int): Unit =
    getSelectedDrive.updatePWMDutyCycle(pwm)

  inline private def isWriting: Boolean = writeDataRegisterFilled || writeShiftRegisterBits > 0

  override def cycle(): Unit =
    val drive = getSelectedDrive
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
      bitCycles += 1
      if bitCycles == bitLengthCycles then // go to next bit
        bitCycles = 0

        val head = getHead
        if isWriting then
          if !lastModeWasWriting then
            diskListener.onFloppyModeChanged(drive.driveIndex,writing = true)
            
          lastModeWasWriting = true
          if writeDataRegisterFilled && writeShiftRegisterBits == 0 then
            writeDataRegisterFilled = false
            writeShiftRegister = writeDataRegister
            writeShiftRegisterBits = 8

          val bitToWrite = (writeShiftRegister & 0x80) == 0x80
          writeShiftRegister <<= 1
          drive.setBitOnHeadAndMoveAhead(head,bitToWrite)
          writeShiftRegisterBits -= 1
          readShiftRegister = 0
        else
          if lastModeWasWriting then
            diskListener.onFloppyModeChanged(drive.driveIndex, writing = false)

          lastModeWasWriting = false
          // read a bit under head and go to next one
          val bit = drive.getBitOnHead(head,moveAhead = true)
          readShiftRegister <<= 1
          if bit then
            readShiftRegister |= 1

          // check if a proper byte has been read
          if (readShiftRegister & 0x80) == 0x80 then
            dataRegister = readShiftRegister & 0xFF
            checkSector(dataRegister)
            readShiftRegister = 0
        end if
  end cycle

  private def checkSector(byte: Int): Unit =
    if sectorByteCounter > 0 then
      sectorByteCounter -= 1
      if sectorByteCounter == 0 then
        GCR.DEC.get(byte) match
          case Some(sector) =>
            diskListener.onSectorOnHead(getSelectedDrive.driveIndex,sector)
          case None =>
            log.warning("Cannot decode sector value: %d",byte)
    else
      sectorShiftRegister <<= 8
      sectorShiftRegister |= byte & 0xFF
      if (sectorShiftRegister & GCR.ADDRESS_MARK_MASK) == GCR.ADDRESS_MARK_BYTES then
        sectorByteCounter = GCR.SECTOR_BYTE_POS_IN_ADDRESS_MARK

  // READ / WRITE ACCESS
  override def read(address:Int): Int =
    log.info("IWM read address %06X",address)
    if (address & 1) == 0 then
      log.warning("Reading IWM from an even address: %06X",address)
      return 0xFF

    access(address)
    readIWMRegister
  override def write(address:Int,value:Int): Unit =
    log.info("IWM write %02X to address %06X",value,address)
    if (address & 1) == 0 then
      log.warning("Writing IWM to an even address: %06X", address)

    access(address)
    writeIWMRegister(value)
