package ucesoft.mac.mmu

import ucesoft.mac.MacModel.PLUS
import ucesoft.mac.cpu.m68k.{M68000, Memory, Size}
import ucesoft.mac.io.MacVIA
import ucesoft.mac.scsi.NCR5380
import ucesoft.mac.serial.Z8530
import ucesoft.mac.storage.IWM
import ucesoft.mac.{MACComponent, MacModel}

import java.util
import scala.annotation.switch

/**
 * @author Alessandro Abbruzzetti
 *         Created on 20/11/2024 12:00  
 */
class MMU(val scc:Z8530,
          val iwm:IWM,
          val via:MacVIA,
          val ncr5380:NCR5380) extends MACComponent with Memory:
  override protected val componentName = "MMU"

  private var ram : Array[Int] = Array()
  private var ramMask = 0
  private var rom : Array[Int] = Array()
  private var romMask = 0
  private var overlay = true
  private var scsi = false
  
  setModel(macModel)
  
  def getRAM: Array[Int] = ram
  def getROM : Array[Int] = rom

  def setOverlay(on:Boolean): Unit =
    overlay = on
    log.info("MMU set overlay %s",overlay)

  def setROM(rom:Array[Int]): Unit =
    this.rom = rom
    romMask = rom.length - 1

  override protected def reset(): Unit = 
    super.reset()
    overlay = true

  override protected def hardReset(): Unit = 
    super.hardReset()
    util.Arrays.fill(ram, 0)

  override protected def setModel(model:MacModel): Unit =
    super.setModel(model)
    ram = configureRAM()
    ramMask = ram.length - 1
    scsi = model.scsi
    log.info("MMU configured %dK of ram for model %s, scsi = %b",model.totalRAMInBytes / 1024,model,scsi)

  private def configureRAM(): Array[Int] =
    Array.ofDim[Int](macModel.totalRAMInBytes)

  inline private def get4MBlock(address:Int): Int = (address >> 22) & 3
  
  def patchRAM(address:Int,value:Int,size: Size): Unit =
    writeTo(ram,address,value,ramMask,size)

  override def read(address: Int, size: Size, readOptions: Int): Int =
    (get4MBlock(address) : @switch) match
      case 0 => readB0(address, size, readOptions)
      case 1 => readB1(address, size, readOptions)
      case 2 => readB2(address, size, readOptions)
      case 3 => readB3(address, size, readOptions)

  override def write(address: Int, value: Int, size: Size, writeOptions: Int): Unit =
    (get4MBlock(address): @switch) match
      case 0 => writeB0(address,value, size, writeOptions)
      case 1 => writeB1(address,value, size, writeOptions)
      case 2 => writeB2(address,value, size, writeOptions)
      case 3 => writeB3(address,value, size, writeOptions)

  // BLOCK 0: RAM or ROM ====================================================================
  private def readB0(address:Int,size:Size,readOptions:Int): Int =
    if overlay then
      readFrom(rom,address,romMask,size)
    else
      readFrom(ram,address,ramMask,size)
  private def writeB0(address:Int,value:Int,size:Size,writeOptions:Int): Unit =
    if overlay then
      // writing to rom
      log.info("MMU writing B0 %06X to ROM",address)
    else
      writeTo(ram,address,value,ramMask,size)
  // BLOCK 1: RAM or ROM ====================================================================
  private def readB1(address: Int, size: Size, readOptions: Int): Int =
    if overlay then
      // Mac SE
      //One of the first instructions n the Reset handler is a jump to a location in the range
      //normally assigned to ROM($400 000 through $43E 800).
      //The first time the BBU receives an address in this range,it switches to the normal address map.
      //In this address map,RAM is located at $000000 through $3FF FFF and ROM is located at $400 000 through $43 FFFF
      // TODO for Mac SE
      if address >= 0x60_0000 then
        readFrom(ram,address,ramMask,size)
      else if (address & 0b0101_1000_0000_0000_0000_0000) == 0b0101_1000_0000_0000_0000_0000 then // 0101 1000 0000 00d0 0rrr 000n => 0x580drn
        //println("Reading SCSI %06X".format(address))
        ncr5380.read(address)
      else readFrom(rom, address, romMask, size)
    else if (address & 0b0101_1000_0000_0000_0000_0000) == 0b0101_1000_0000_0000_0000_0000 then // 0101 1000 0000 00d0 0rrr 000n => 0x580drn
      //println("Reading SCSI %06X".format(address))
      ncr5380.read(address)
    else if address >= 0x44_0000 && address < 0x50_0000 && macModel == PLUS then
      // Plus with SCSI has no repeated ROM images above 0x440000 as
      // indication of SCSI controller present.
      0xFF
    else
      readFrom(rom, address, romMask, size)
  private def writeB1(address:Int,value:Int,size:Size,writeOptions:Int): Unit =
    if overlay then
      if address >= 0x60_000 then
        writeTo(ram,address - 0x60_0000,value,ramMask,size)
      else if (address & 0b0101_1000_0000_0000_0000_0000) == 0b0101_1000_0000_0000_0000_0000 then // 0101 1000 0000 00d0 0rrr 000n => 0x580drn
        //println("Writing SCSI %06X".format(address))
        ncr5380.write(address,value)
      else readFrom(rom, address, romMask, size)
    else if (address & 0b0101_1000_0000_0000_0000_0000) == 0b0101_1000_0000_0000_0000_0000 then // 0101 1000 0000 00d0 0rrr 000n => 0x580drn
      //println("Writing SCSI %06X".format(address))
      ncr5380.write(address,value)
    else // writing to rom
      log.warning("MMU writing B1 %06X to ROM",address)
  // BLOCK 2: SCC ===========================================================================
  private def readB2(address: Int, size: Size, readOptions: Int): Int =
    import Size.*
    if address < 0xA0_0000 then
      size match
        case Byte =>
          if (address & 1) == 0 then // The SCC uses the upper byte of the data bus, so use A0 = 0 for reading the SCC
              val channel = ((address & 2) >> 1) ^ 1 // channel A=0 or B=1
              // check if is data or control register
              val dataRegister = (address & 4) != 0
              if dataRegister then
                scc.readData(channel)
              else
                scc.readControl(channel)
          else // A byte access to any SCC READ address with A0 = 1 resets the SCC
            scc.resetComponent()
            log.info("MMU reading %06X and reset SCC...",address)
            0xFF
        case Word =>
          log.info("MMU reading word %06X adjusts phase ...",address)
          adjustPhase()
          0xFF
        case Long =>
          log.warning("MMU reading %06X: unknown SCC access",address)
          0xFF
    else
      log.warning("MMU reading from %06X: invalid SCC read address",address)
      0xFF
  end readB2
  private def writeB2(address:Int,value:Int,size:Size,writeOptions:Int): Unit =
    import Size.*
    if address >= 0xA0_0000 && address < 0xC0_0000 then
      size match
        case Byte =>
          if (address & 1) == 1 then
            val channel = ((address & 2) >> 1) ^ 1 // channel A=0 or B=1
            // check if is data or control register
            val dataRegister = (address & 4) != 0
            if dataRegister then
              scc.writeData(channel,value)
            else
              scc.writeControl(channel,value)
          else
            log.info("MMU writing %06X SCC: ignored",address)
        case Word =>
          log.info("MMU writing word %06X adjusts phase ...",address)
          adjustPhase()
        case Long =>
          log.warning("MMU writing %06X: unknown SCC access",address)
    else
      log.warning("MMU writing %06X: invalid SCC write address", address)
  end writeB2
  // BLOCK 3: IWM & VIA =====================================================================
  private def readB3(address: Int, size: Size, readOptions: Int): Int =
    import Size.*
    if size == Byte then
      (address >> 20) & 0xF match
        // IWM: A23 A22 A21 A20
        //       1   1   0   X
        case 0b1100|0b1101 =>
          // A12 A11 A10 A09 select IWM register
          iwm.read(address)
        // VIA A23 A22 A21 A20
        //      1   1   1   0
        case 0b1110 =>
          // A12 A11 A10 A09 select VIA register
          via.read((address >> 9) & 0xF)
        case _ =>
          if address >= 0xF0_0000 && address < 0xF8_0000 then
            log.info("MMU reading %06X: reading phase",address)
            phaseRead()
          else if address < 0xFF_FFF0 then
            log.warning("MMU reading %06X: unknown IWM/VIA address",address)
            0xFF
          else // interrupt handling: read from FF_FFF0
            0xFF
    else if address >= 0xF0_0000 && address < 0xF8_0000 then
      log.info("MMU reading %06X: reading phase with size %s",address,size)
      phaseRead()
    else
      //log.warning("MMU reading %06X B3 with size %s",address,size)
      0xFF
  end readB3
  private def writeB3(address:Int,value:Int,size:Size,writeOptions:Int): Unit =
    import Size.*
    if size == Byte then
      (address >> 20) & 0xF match
        // IWM: A23 A22 A21 A20
        //       1   1   0   X
        case 0b1100|0b1101 =>
          // A12 A11 A10 A09 select IWM register
          iwm.write(address,value)
        // VIA A23 A22 A21 A20
        //      1   1   1   0
        case 0b1110 =>
          // A12 A11 A10 A09 select VIA register
          via.write((address >> 9) & 0xF,value)
        case _ =>
          log.warning("MMU writing %06X: unknown IWM/VIA address",address)
    else
      log.warning("MMU writing %06X B3 with size %s",address,size)
  end writeB3
  // ========================================================================================
  private def phaseRead(): Int = 0xFF // TODO
  private def adjustPhase(): Unit = {/*TODO*/}

  inline private def readFrom(mem:Array[Int],address:Int,mask:Int,size:Size): Int =
    import Size.*
    val adr = address & mask
    size match
      case Byte =>
        mem(adr) & 0xFF
      case Word =>
        (mem(adr) << 8 | mem(adr + 1)) & 0xFFFF
      case Long =>
        mem(adr) << 24 | mem(adr + 1) << 16 | mem(adr + 2) << 8 | mem(adr + 3)

  inline private def writeTo(mem:Array[Int],address:Int,value:Int,mask:Int,size:Size): Unit =
    import Size.*
    val adr = address & mask
    size match
      case Byte =>
        mem(adr) = value & 0xFF
      case Word =>
        mem(adr) = (value >> 8) & 0xFF
        mem(adr + 1) = value & 0xFF
      case Long =>
        mem(adr) = value >>> 24
        mem(adr + 1) = (value >> 16) & 0xFF
        mem(adr + 2) = (value >> 8) & 0xFF
        mem(adr + 3) = value & 0xFF