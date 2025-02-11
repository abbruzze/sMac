package ucesoft.mac.rtc

import ucesoft.mac.{ConfigContext, MACComponent, MacModel, MessageBus}

import java.io.File
import java.time.LocalDateTime
import java.time.temporal.ChronoUnit
import javax.swing.ImageIcon

/**
 * @author Alessandro Abbruzzetti
 *         Created on 15/11/2024 15:32  
 */
class RTC extends MACComponent:
  override protected val componentName = "RTC"
  override protected val icon = new ImageIcon(getClass.getResource("/resources/trace/clock.png"))
  
  private var pram = Array.ofDim[Int](20)
  // number of seconds since midnight, January 1, 1904
  private var seconds : Long = initSeconds()
  // To access the clock chip, you must first enable its serial function. To do this, set the serial enable
  // line (rTCEnb) to 0. Keep the serial enable line low during the entire transaction; if you set it to 1,
  // you'll abort the transfer.
  private var chipEnabled = true
  private var cmd = 0
  private var dataOut = 0
  private var currentDataOutBit = 0
  private var dataIn = 0
  private var dataInBitCount = 0
  private var chipClk = false
  private var wp = false
  private var extendedMSB = 0
  private var extendedCMD = false
  private var extendedAddress = 0

  private var loadedFromFile = ""

  private final val DEFAULT_PRAM_VALUES = Array(
  // 00   01   02   03   04   05   06   07   08   09   0A   0B   0C   0D   0E   0F
    0x00,0x80,0x4F,0x00,0x00,0x00,0x00,0x00,0x05,0x58,0x01,0x60,0x42,0x75,0x67,0x73,
    0xA8,0x00,0x01,0x22,0xCC,0x0A,0xCC,0x0A,0xC8,0xFB,0x9C,0x890,0x00,0x02,0xA3,0x01
  )

  private enum State:
    case CMD, CMDEXT, WRITE, READ

  private var state = State.CMD

  override def onMessage(msg: MessageBus.Message): Unit =
    msg match
      case MessageBus.Shutdown(_,ConfigContext(homeDir,_,_)) =>
        MessageBus.send(MessageBus.ShuttingdownItem(this,"Saving pram ..."))
        val pramDir = new File(homeDir,"pram")
        val pramFile = new File(pramDir,s"pram$macModel.bin")
        // Don't know why: if pram(19) == 0x21 (and seems to happen when the user request a shutdown) the boot remains stuck
        if pram(19) == 0x21 then pram(19) = 0x22
        java.nio.file.Files.write(pramFile.toPath,pram.map(_.toByte))
      case MessageBus.Configuration(_,ConfigContext(homeDir,_,_)) =>
        val pramDir = new File(homeDir,"pram")
        if !pramDir.exists() then
          pramDir.mkdirs()
        val pramFile = new File(pramDir,s"pram$macModel.bin")
        if pramFile.exists() then
          pram = java.nio.file.Files.readAllBytes(pramFile.toPath).map(_.toInt & 0xFF)
          loadedFromFile = pramFile.getAbsolutePath
          log.info("RTC PRAM loaded from %s",pramFile)
        else
          log.warning("RTC PRAM file %s not found, set default values",pramFile)
          System.arraycopy(DEFAULT_PRAM_VALUES,0,pram,0,math.min(DEFAULT_PRAM_VALUES.length,pram.length))
      case _ =>
  end onMessage

  override protected def reset(): Unit =
    super.reset()
    chipEnabled = true
    dataOut = 0
    dataIn = 0
    dataInBitCount = 0
    state = State.CMD
    extendedCMD = false
    chipClk = false
    wp = false

  override protected def setModel(model:MacModel): Unit =
    super.setModel(model)
    val size = if model.pramExtended then 256 else 20
    pram = Array.ofDim[Int](size)
    log.info("RTC pram size set to %d",size)

  override def getProperties: List[MACComponent.Property] =
    import MACComponent.Property
    List(
      Property("Loaded from",loadedFromFile),
      Property("State",state.toString),
      Property("Enabled",(!chipEnabled).toString),
      Property("Command","%02X".format(cmd)),
      Property("Data out",Integer.toBinaryString(dataOut)),
      Property("Data in",Integer.toBinaryString(dataIn)),
      Property("Bit count",dataInBitCount.toString),
      Property("Write protect",wp.toString),
      Property("PRAM",pram.map(v => "%02X".format(v)).mkString("{",",","}")),
      Property("PRAM size",pram.length.toString)
    )

  private def initSeconds(): Long =
    val now = LocalDateTime.now()
    val zero = LocalDateTime.of(1904,1,1,0,0,0)
    ChronoUnit.SECONDS.between(zero,now)

  def oneSecondTrigger(): Unit = seconds += 1

  /*
    A command can be either a write request or a read request. After the eight bits of a write request,
    the clock will expect the next eight bits across the serial data line to be your data for storage into
    one of the internal registers of the clock. After receiving the eight bits of a read request, the clock
    will respond by putting eight bits of its data on the serial data line. Commands and data are
    transferred serially in eight-bit groups over the serial data line, with the high-order bit first and the
    low-order bit last.
    To send a command to the clock, first set the rTCData bit of VIA data direction register B
    (vBase+vDirB) so that the real-time clock's serial data line will be used for output to the clock.
    Next, set the rTCClk bit of vBase+vBufB to 0, then set the rTCData bit to the value of the first
    (high-order) bit of your data byte. Then raise (set to 1) the data-clock bit (rTCClk). Then lower
    the data-clock, set the serial data line to the next bit, and raise the data-clock line again. After the
    last bit of your command has been sent in this way, you can either continue by sending your data
    byte in the same way (if your command was a write request) or switch to receiving a data byte
    from the clock (if your command was a read request).
    To receive a byte of data from the clock, you must first send a command that's a read request.
    After you've clocked out the last bit of the command, clear the rTCData bit of the data direction
    register so that the real-time clock's serial data line can be used for input from the clock; then
    lower the data-clock bit (rTCClk) and read the first (high-order) bit of the clock's data byte on the
    serial data line. Then raise the data-clock, lower it again, and read the next bit of data. Continue
    this until all eight bits are read, then raise the serial enable line (rTCEnb }, disabling the data
    transfer.

    Command format:
    7 6 5 4 3 2 1 0
    Z X X X X X 1 0   Z=1 read, Z=0 write, XXXXX=command

      Address	  Contents		      Address	  Contents
     ===================================================
      0x00	    Seconds 0 (LSB)		0x10	    PRAM High 0
      0x01	    Seconds 1		      0x11	    PRAM High 1
      0x02	    Seconds 2		      0x12	    PRAM High 2
      0x03	    Seconds 3 (MSB)		0x13	    PRAM High 3
      0x04	    Seconds 0 (LSB)		0x14	    PRAM High 4
      0x05	    Seconds 1		      0x15	    PRAM High 5
      0x06	    Seconds 2		      0x16	    PRAM High 6
      0x07	    Seconds 3 (MSB)		0x17	    PRAM High 7
      0x08	    PRAM Low 0		    0x18	    PRAM High 8
      0x09	    PRAM Low 1		    0x19	    PRAM High 9
      0x0A	    PRAM Low 2		    0x1A	    PRAM High 10
      0x0B	    PRAM Low 3		    0x1B	    PRAM High 11
      0x0C	    Test		          0x1C	    PRAM High 12
      0x0D	    Write-Protect		  0x1D	    PRAM High 13
      0x0E	    Extended Cmd		  0x1E	    PRAM High 14
      0x0F	    Extended Cmd		  0x1F	    PRAM High 15

      Extended command format:
      First byte
      7 6 5 4 3 2 1 0
      Z 0 1 1 1 Y Y Y
      Second byte:
      7 6 5 4 3 2 1 0
      - X X X X X - -

      ram address = YYYXXXXX
  */
  def setLines(enabled:Boolean,clk:Boolean,data:Boolean): Unit =
    if enabled then
      chipEnabled = true
      log.info("RTC disabled")
      return

    if chipEnabled then // reset
      chipEnabled = false
      dataOut = 0
      dataIn = 0
      dataInBitCount = 0
      state = State.CMD
      extendedCMD = false

    // on clk raising edge
    if !chipClk && clk then
      dataInBitCount += 1

      state match
        case State.READ =>
          currentDataOutBit = if (dataOut & 0x80) == 0x80 then 1 else 0
          dataOut <<= 1
          if dataInBitCount == 8 then
            state = State.CMD
            dataInBitCount = 0
        case _ =>
          dataIn <<= 1
          if data then
            dataIn |= 1

          if dataInBitCount == 8 then // received 8 bits command or data
            dataInBitCount = 0
            state match
              case State.CMDEXT =>
                extendedAddress = (extendedMSB & 7) << 5 | (dataIn >> 2) & 0x1F
                if (extendedMSB & 0x80) != 0 then // extended read
                  state = State.CMD
                  extended(read = true,extendedAddress)
                else
                  state = State.WRITE
              case State.CMD =>
                extendedCMD = false
                cmd = dataIn & 0xFF
                // check extended command first
                if (cmd & 0b01111000) == 0b00111000 then
                  extendedMSB = cmd & 0x87
                  state = State.CMDEXT
                  extendedCMD = true
                else
                // the last two bits of a command byte must always be 01
                // A 1 in the high-order bit makes your command a read request; a 0 in the high-order bit makes your command a write request
                if (dataIn & 0x80) == 0x80 then
                  state = State.READ
                  dataOut = read(cmd)
                else
                  state = State.WRITE
              case State.WRITE =>
                state = State.CMD
                if extendedCMD then
                  extended(read = false,extendedAddress,dataIn)
                else
                  write(cmd,dataIn)
              case _ => /*never happen*/
            dataIn = 0
    chipClk = clk
  end setLines

  def data: Int = currentDataOutBit

  private def extended(read:Boolean,address:Int,value:Int = 0): Unit =
    if pram.length < 256 then
      log.warning("RTC extended command but pram size is %d. Ignored.",pram.length)
      return

    if read then
      log.info("RTC reading extended address %02X",address)
      dataOut = pram(address)
    else
      log.info("RTC writing extended address %02X with %02X",address,value)
      if !wp then
        pram(address) = value

  private def write(cmd:Int,value:Int): Unit =
    log.info(s"RTC writing into %2X %2X wp=%s",cmd,value,wp)
    (cmd >> 2) & 0x1F match
      case 0|4 =>
        if !wp then seconds = (seconds & 0xFFFFFF00) | (value & 0xFF)
      case 1|5 =>
        if !wp then seconds = (seconds & 0xFFFF00FF) | (value & 0xFF) << 8
      case 2|6 =>
        if !wp then seconds = (seconds & 0xFF00FFFF) | (value & 0xFF) << 16
      case 3|7 =>
        if !wp then seconds = (seconds & 0x00FFFFFF) | (value & 0xFF) << 24
      case 12 => // test register
      case 13 =>
        wp = (value & 0x80) == 0x80
        log.info("RTC write protect set to %s",wp)
      case x@(8|9|10|11) =>
        if !wp then pram(x) = value & 0xFF
      case x if x >= 16 && x < 32 && x < pram.length =>
          if !wp then
            pram(x) = value & 0xFF
      case _ =>
          log.warning("RTC: writing to undefined write register: %d", cmd)

  private def read(cmd:Int): Int =
    val r = (cmd >> 2) & 0x1F match
      case 0|4 => (seconds & 0xFF).toInt
      case 1|5 => ((seconds >> 8) & 0xFF).toInt
      case 2|6 => ((seconds >> 16) & 0xFF).toInt
      case 3|7 => ((seconds >> 24) & 0xFF).toInt
      case x@(8|9|10|11) =>
        pram(x)
      case x if x >= 16 && x < 32 && x < pram.length =>
        pram(x)
      case _ =>
        log.warning("RTC: reading from undefined read register: %d",cmd)
        0xFF
    log.info("RTC reading from %2X %2X",cmd,r)
    r