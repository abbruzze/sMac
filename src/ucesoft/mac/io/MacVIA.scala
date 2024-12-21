package ucesoft.mac.io

import ucesoft.mac.MacModel
import ucesoft.mac.MacModel.MAC128K
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
 * PA4      OUT       Overlay (1 = ROM in low-memory)
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
 * PB4      IN        MOUSE X2
 * PB5      IN        MOUSE Y2
 * PB6      IN        H4 (Horizontal Blanking)
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
             overlay:Boolean => Unit) extends VIA(irqAction):
  override protected val componentName = "VIA 6522"
  override protected val icon = new ImageIcon(getClass.getResource("/resources/trace/via.png"))
  private inline val KEYBOARD_CMD_OUT_TIME = (180 + 220) * 8 // us
  private inline val KEYBOARD_RESPONSE_TIME = (160 + 170) * 8 // us
  private var KEYBOARD_CMD_EXE_WAIT_CYCLES = 0 // see setModel

  private var hblank = 0

  private var keyboardCommandWaitCycles = 0

  setModel(MAC128K)

  override protected def reset(): Unit =
    super.reset()
    hblank = 0
    keyboardCommandWaitCycles = 0

  def setHorizontalBlanking(on:Boolean): Unit = hblank = if on then 1 else 0

  override protected def setModel(model: MacModel): Unit =
    super.setModel(model)
    KEYBOARD_CMD_EXE_WAIT_CYCLES = (model.clockRateMhz / 1_000_000.0 * (KEYBOARD_CMD_OUT_TIME + KEYBOARD_RESPONSE_TIME)).toInt

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
         * PB4      IN        MOUSE X2
         * PB5      IN        MOUSE Y2
         * PB6      IN        H4 (Horizontal Blanking)
         * PB7      OUT       Sound reset (1=sound off)
        */
        var pb = 0
        /*PB0*/if (regs(DDRB) & 1) == 0 then pb |= rtc.data else pb |= regs(PB) & 0x1
        /*PB1*/pb |= regs(PB) & 0x2
        /*PB2*/pb |= regs(PB) & 0x4
        /*PB3*/if (regs(DDRB) & 0x8) == 0 then pb |= (if !mouse.isButtonPressed then 0x8 else 0) else pb |= regs(PB) & 0x8
        /*PB4*/if (regs(DDRB) & 0x10) == 0 then pb |= mouse.X2 << 4 else pb |= regs(PB) & 0x10
        /*PB5*/if (regs(DDRB) & 0x20) == 0 then pb |= mouse.Y2 << 5 else pb |= regs(PB) & 0x20
        /*PB6*/if (regs(DDRB) & 0x40) == 0 then pb |= hblank << 6 else pb |= regs(PB) & 0x40
        /*PB7*/pb |= addPB7(regs(PB) & 0x80)
        pb
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
         * PA4      OUT       Overlay (1 = ROM in low-memory)
         * PA5      OUT       SEL, Disk Head Select
         * PA6      OUT       Alternate screen buffer (0 = Video Page 2)
         * PA7      IN        SCC wait/request
        */
        /*PA0-2*/audio.setVolumeLevel(value & 7)
        /*PA3  */if (regs(DDRA) & 0x8) != 0 then audio.setAlternateAudioBuffer((value & 0x8) == 0)
        /*PA4  */if (regs(DDRA) & 0x10) != 0 then overlay((value & 0x10) != 0)
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
         * PB4      IN        MOUSE X2
         * PB5      IN        MOUSE Y2
         * PB6      IN        H4 (Horizontal Blanking)
         * PB7      OUT       Sound reset (1=sound off)
        */
        /*PB0-2*/
        val data = if (regs(DDRB) & 1) == 1 then value & 1 else lastPB & 1
        val clk = if (regs(DDRB) & 2) == 2 then value & 2 else lastPB & 2
        val clkEn = if (regs(DDRB) & 4) == 4 then value & 4 else lastPB & 4
        rtc.setLines(enabled = clkEn != 0,clk = clk != 0,data = data != 0)
        /*PB7  */if (regs(DDRB) & 0x80) != 0 then audio.turn(on = (addPB7(value) & 0x80) == 0)
      case SR =>
        regs(SR) = value
        irq_clr(IRQ_SR)
        val mode = srMode
        log.info("VIA writing SR => SR=%02X SR-MODE=%02X",regs(SR),mode)
        if mode == 7 then // shift out under control of external pulse
          log.info("VIA sending keyboard command: %02X after %d cycles",value,KEYBOARD_CMD_EXE_WAIT_CYCLES)
          if keyboardCommandWaitCycles > 0 then
            log.warning("VIA keyboardCommandWaitCycles is not 0: %d",keyboardCommandWaitCycles)
          keyboardCommandWaitCycles = KEYBOARD_CMD_EXE_WAIT_CYCLES
      case _ =>
        super.write(address,value)
  end write

  override def clock(cycles: Long): Unit =
    super.clock(cycles)

    if keyboardCommandWaitCycles > 0 then
      keyboardCommandWaitCycles -= 1
      if keyboardCommandWaitCycles == (KEYBOARD_CMD_EXE_WAIT_CYCLES >> 1) then // TODO
        irq_set(IRQ_SR) // cmd command has been sent
      else if keyboardCommandWaitCycles == 0 then
        irq_set(IRQ_SR) // a byte from keyboard has been received
        regs(SR) = keyboard.command(regs(SR))