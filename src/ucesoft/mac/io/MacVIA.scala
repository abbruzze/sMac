package ucesoft.mac.io

import ucesoft.mac.MacModel
import ucesoft.mac.MacModel.MAC128K
import ucesoft.mac.adb.ADBTransceiver
import ucesoft.mac.audio.Audio
import ucesoft.mac.keyboard.MacKeyboard
import ucesoft.mac.mouse.QuadMouse
import ucesoft.mac.rtc.RTC
import ucesoft.mac.storage.IWM
import ucesoft.mac.video.MacVideo

import javax.swing.ImageIcon

/**
 * @author Alessandro Abbruzzetti
 *         Created on 18/11/2024 17:12
 *
 *
 * PORT A:
 * ------------------------------------------------------------------------
 * PA0-2    OUT       Sound volume (PA0 LSB, PA2 MSB)
 * PA3      OUT       Alternate sound buffer (0=sound buffer on page 2)
 *                    From SE vSync: 1 = synchronous modem support,channelA
 * PA4      OUT       Overlay (1 = ROM in low-memory)
 *                    From SE vDriveSel: 0 = upper internal floppy disk drive,1 = lower
 * PA5      OUT       SEL, Disk Head Select
 * PA6      OUT       Alternate screen buffer (0 = Video Page 2)
 * PA7      IN        SCC wait/request
 *
 * Port A Data Direction Byte: 0x7F
 *
 * PORT B:
 * ------------------------------------------------------------------------
 * PB0      I/O       RTC.DATA (Clock Data)
 * PB1      OUT       RTC.CLK (Clock Data Timer)
 * PB2      OUT       /RTC (Clock Enable)
 * PB3      IN        /MOUSE SW (0 if switch is pressed)
 *                    From SE: vFDBint: 0 = ADB interrupt
 * PB4      IN        MOUSE X2
 *          OUT       From SE: vFDeskl: ADB state input 0 (ST)
 * PB5      IN        MOUSE Y2
 *          OUT       From SE: vFDesk2: ADB state input 1 (ST)
 * PB6      IN        H4 (Horizontal Blanking)
 *          OUT       From SE: vH4: 0 = SCSI IRQ Interrupt enable
 * PB7      OUT       Sound reset (1=sound off)
 *
 * Port B Data Direction Byte: when data is coming in from clock: 0x86, when data is going out to clock: 0x87
 *
 * CONTROL REGISTERS:
 * ------------------------------------------------------------------------
 * CB2      I/O       KBD.DATA (Keyboard Data) (PCR bits 765)
 * CB1      IN        KBD.SCLK (Clock for Keyboard Data) (PCR bits 4)
 * CA2      IN        1SEC CLKOUT (1 Sec. Clock Interrupt) (PCR bits 321)
 * CA1      IN        /VSYNC (Video Vertical Blanking) (PCR bits 0)
 *
 * Interrupts:
 * ------------------------------------------------------------------------
 * 7        /IRQ      any enabled VIA interrupt
 * 6        Timer 1   Sound reset
 * 5        Timer 2
 * 4        CB1       KBD.SCLK (Clock for Keyboard Data)
 * 3        CB2       KBD.DATA (Keyboard Data)
 * 2        SR        Eight bits of KBD.DATA Shifted
 * 1        CA1       /VSYNC (Video Vertical Blanking)
 * 0        CA2       1SEC CLKOUT (1 Sec. Clock Interrupt)
 */
class MacVIA(override val irqAction:Boolean => Unit,
             audio:Audio,
             video:MacVideo,
             mouse:QuadMouse,
             rtc:RTC,
             iwm:IWM,
             keyboard: MacKeyboard,
             adb:ADBTransceiver,
             overlay:Boolean => Unit) extends VIA(irqAction):
  override protected val componentName = "VIA 6522"
  override protected val icon = new ImageIcon(getClass.getResource("/resources/trace/via.png"))
  private inline val KEYBOARD_CMD_OUT_TIME = (180 + 220) * 8 // us
  private inline val KEYBOARD_RESPONSE_TIME = (160 + 170) * 8 // us
  private var KEYBOARD_CMD_EXE_WAIT_CYCLES = 0 // see setModel
  private inline val ADB_RESPONSE_CYCLES = 2350 // about 3ms (raw estimation that seems to work)
  private var ADB_AUTOPOLLING_CYCLES = 0 // see setModel

  private var hblank = 0
  private var keyboardCommandWaitCycles = 0
  private var scsiIRQEnabled = false
  private var adbAckCycles = 0
  private var adbWroteSR = false
  private var adbAutoPollingCycles = 0

  setModel(MAC128K)

  override protected def reset(): Unit =
    super.reset()
    hblank = 0
    keyboardCommandWaitCycles = 0
    scsiIRQEnabled = false
    adbWroteSR = false
    adbAutoPollingCycles = 0
  end reset

  def isSCSIIRQEnabled: Boolean = scsiIRQEnabled

  def setHorizontalBlanking(on:Boolean): Unit = hblank = if on then 1 else 0

  override protected def setModel(model: MacModel): Unit =
    super.setModel(model)
    KEYBOARD_CMD_EXE_WAIT_CYCLES = (model.clockRateMhz / 1_000_000.0 * (KEYBOARD_CMD_OUT_TIME + KEYBOARD_RESPONSE_TIME)).toInt
    ADB_AUTOPOLLING_CYCLES = model.clockRateMhz / 1000 * 5 // 5 ms

  override def read(address: Int): Int =
    log.info("VIA reading register %d",address & 0xF)
    address & 0x0F match
      case PA|PA2 =>
        super.read(address) | 0x80 // TODO SCC wait/request
      case PB =>
        super.read(address)
        /*
         * PB0      I/O       RTC.DATA (Clock Data)
         * PB1      OUT       RTC.CLK (Clock Data Timer)
         * PB2      OUT       /RTC (Clock Enable)
         * PB3      IN        /MOUSE SW (0 if switch is pressed)
         *                    From SE: vFDBint: 0 = ADB interrupt
         * PB4      IN        MOUSE X2
         *          OUT       From SE: vFDeskl: ADB state input 0 (ST)
         * PB5      IN        MOUSE Y2
         *          OUT       From SE: vFDesk2: ADB state input 1 (ST)
         * PB6      IN        H4 (Horizontal Blanking)
         *          OUT       From SE: vH4: 0 = SCSI IRQ Interrupt enable
         * PB7      OUT       Sound reset (1=sound off)
        */
        val belowSE = macModel.ordinal < MacModel.SE.ordinal
        var pb = 0
        /*PB0*/if (regs(DDRB) & 1) == 0 then pb |= rtc.data else pb |= regs(PB) & 0x1
        /*PB1*/pb |= regs(PB) & 0x2
        /*PB2*/pb |= regs(PB) & 0x4
        /*PB3*/
        if belowSE then
          if (regs(DDRB) & 0x8) == 0 then pb |= (if !mouse.isButtonPressed then 0x8 else 0) else pb |= regs(PB) & 0x8
        else
          if (regs(DDRB) & 0x8) == 0 then pb |= (if adb.isInterrupt then 0x0 else 0x8) else pb |= regs(PB) & 0x8
        /*PB4*/
        if belowSE then
          if (regs(DDRB) & 0x10) == 0 then pb |= mouse.X2 << 4 else pb |= regs(PB) & 0x10
        else
          pb |= regs(PB) & 0x10
        /*PB5*/
        if belowSE then
          if (regs(DDRB) & 0x20) == 0 then pb |= mouse.Y2 << 5 else pb |= regs(PB) & 0x20
        else
          pb |= regs(PB) & 0x20
        /*PB6*/
        if belowSE then
          if (regs(DDRB) & 0x40) == 0 then pb |= hblank << 6 else pb |= regs(PB) & 0x40
        else
          pb |= regs(PB) & 0x40
        /*PB7*/pb |= addPB7(regs(PB) & 0x80)
        pb
      case SR =>
        irq_clr(IRQ_SR)
        //println(s"VIA reading SR => SR=${regs(SR)}")
        regs(SR)
      case _ =>
        super.read(address)
  end read

  override def write(address: Int, value: Int): Unit =
    log.info("VIA writing register %d with %d",address & 0xF,value)
    address & 0x0F match
      case PA|PA2 =>
        super.write(address, value)
        /*
         * PA0-2    OUT       Sound volume (PA0 LSB, PA2 MSB)
         * PA3      OUT       Alternate sound buffer (0=sound buffer on page 2)
         *                    From SE vSync: 1 = synchronous modem support,channelA
         * PA4      OUT       Overlay (1 = ROM in low-memory)
         *                    From SE vDriveSel: 0 = upper internal floppy disk drive,1 = lower
         * PA5      OUT       SEL, Disk Head Select
         * PA6      OUT       Alternate screen buffer (0 = Video Page 2)
         * PA7      IN        SCC wait/request
        */
        val belowSE = macModel.ordinal < MacModel.SE.ordinal
        /*PA0-2*/audio.setVolumeLevel(value & 7)
        /*PA3  */if belowSE && (regs(DDRA) & 0x8) != 0 then audio.setAlternateAudioBuffer((value & 0x8) == 0)
        /*PA4  */
        if belowSE then
          if (regs(DDRA) & 0x10) != 0 then overlay((value & 0x10) != 0)
        else
          if (regs(DDRA) & 0x10) != 0 && macModel == MacModel.SE then iwm.setInternalDriveSE((value & 0x10) != 0)
        /*PA5  */if (regs(DDRA) & 0x20) != 0 then iwm.setHeadSelLine((value & 0x20) != 0)
        /*PA6  */if (regs(DDRA) & 0x40) != 0 then video.selectVideoBuffer((value & 0x40) == 0)
      case PB =>
        val lastPB = regs(PB)
        super.write(address, value)
        /*
         * PB0      I/O       RTC.DATA (Clock Data)
         * PB1      OUT       RTC.CLK (Clock Data Timer)
         * PB2      OUT       /RTC (Clock Enable)
         * PB3      IN        /MOUSE SW (0 if switch is pressed)
         *                    From SE: vFDBint: 0 = ADB interrupt
         * PB4      IN        MOUSE X2
         *          OUT       From SE: vFDeskl: ADB state input 0 (ST)
         * PB5      IN        MOUSE Y2
         *          OUT       From SE: vFDesk2: ADB state input 1 (ST)
         * PB6      IN        H4 (Horizontal Blanking)
         *          OUT       From SE: vH4: 0 = SCSI IRQ Interrupt enable
         * PB7      OUT       Sound reset (1=sound off)
        */
        /*PB0-2*/
        val fromSE = macModel.ordinal >= MacModel.SE.ordinal
        val data = if (regs(DDRB) & 1) == 1 then value & 1 else lastPB & 1
        val clk = if (regs(DDRB) & 2) == 2 then value & 2 else lastPB & 2
        val clkEn = if (regs(DDRB) & 4) == 4 then value & 4 else lastPB & 4
        rtc.setLines(enabled = clkEn != 0,clk = clk != 0,data = data != 0)
        /*PB4-5*/
        if fromSE then
          if (regs(DDRB) & 0x30) == 0x30 then
            val cmd = (value >> 4) & 3
            adb.setState(cmd) match
              case None =>
              case Some(data) =>
                // a byte was returned by transceiver: send an INT to emulate the VIA's acquisition time
                regs(SR) = data
                irq_set(IRQ_SR)
        /*PB6  */
        if fromSE then
          if (regs(DDRB) & 0x40) != 0 then
            scsiIRQEnabled = (value & 0x40) == 0
            if scsiIRQEnabled then println("SCSI IRQ enabled")
        /*PB7  */if (regs(DDRB) & 0x80) != 0 then audio.turn(on = (addPB7(value) & 0x80) == 0)
      case SR =>
        val fromSE = macModel.ordinal >= MacModel.SE.ordinal
        regs(SR) = value
        irq_clr(IRQ_SR)
        val mode = srMode
        log.info("VIA writing SR => SR=%02X SR-MODE=%02X",regs(SR),mode)
        if mode == 7 then // shift out under control of external pulse
          if !fromSE then
            val waitCycles = KEYBOARD_CMD_EXE_WAIT_CYCLES
            log.info("VIA sending keyboard command: %02X after %d cycles",value,waitCycles)
            if keyboardCommandWaitCycles > 0 then
              log.warning("VIA keyboardCommandWaitCycles is not 0: %d",keyboardCommandWaitCycles)
            keyboardCommandWaitCycles = waitCycles
          else
            // a byte has been written to SR to be clocked out to ADB: will send an INT to emulate the transceiver's acquisition time
            adbAckCycles = ADB_RESPONSE_CYCLES
      case _ =>
        super.write(address,value)
  end write

  override def clock(cycles: Long): Unit =
    super.clock(cycles)

    // =============== ADB ======================
    adbAutoPollingCycles += 1
    if adbAutoPollingCycles == ADB_AUTOPOLLING_CYCLES then
      adbAutoPollingCycles = 0
      if srMode == 3 && macModel.ordinal >= MacModel.SE.ordinal && adb.isServiceRequestPending then
        regs(SR) = 0xFF
        irq_set(IRQ_SR)

    if adbAckCycles > 0 then
      adbAckCycles -= 1
      if adbAckCycles == 0 then
        // a VIA transfer was finished, sending byte to transceiver
        adb.clockInByte(regs(SR))
        irq_set(IRQ_SR)
    // ===========================================

    if keyboardCommandWaitCycles > 0 then
      val fromSE = macModel.ordinal >= MacModel.SE.ordinal
      keyboardCommandWaitCycles -= 1
      if keyboardCommandWaitCycles == KEYBOARD_CMD_EXE_WAIT_CYCLES >> 1 then
        irq_set(IRQ_SR) // cmd command has been sent
      else if keyboardCommandWaitCycles == 0 then
        irq_set(IRQ_SR) // a byte from keyboard has been received
        regs(SR) = keyboard.command(regs(SR))