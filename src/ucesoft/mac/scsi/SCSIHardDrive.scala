package ucesoft.mac.scsi

import java.io.{File, FileInputStream, FileNotFoundException}
import java.nio.MappedByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.{Files, StandardOpenOption}
import javax.swing.ImageIcon
/**
 * @author Alessandro Abbruzzetti
 *         Created on 07/12/2024 12:34  
 */
class SCSIHardDrive(override val id:Int,val file:String) extends DirectAccessSCSITarget(id):
  override val scsiOn = new ImageIcon(getClass.getResource("/resources/scsi_on.png"))
  override val scsiOff = new ImageIcon(getClass.getResource("/resources/scsi_off.png"))
  override val name : String = file
  override protected val BLOCK_SIZE = 512

  // first 0x60 blocks of a formatted SCSI HD
  private var APM_BLOCKS : Array[Byte] = Array()
  private var apmBlockOffset = 0
  
  private val storage = mapFile()

  // Based on Inside Macintosh: Devices, chapter 3
  // Adds an APM (Driver Descriptor Record + Apple HFS + Partition Map + Driver) taken from a template that is a snapshot of the first 0x60 blocks of a formatted HD
  private def initAPM(totalBlockSize:Int): Unit =
    APM_BLOCKS = getClass.getResourceAsStream("/resources/apm_template.bin").readAllBytes()
    apmBlockOffset = 0x60
    val newTotalBlock = totalBlockSize + 0x60
    val availableTotalBlock = totalBlockSize - 0x60
    // Block 0: Driver Descriptor Record
    // sbBlkCount: LongInt
    APM_BLOCKS(4) = (newTotalBlock >> 24).toByte
    APM_BLOCKS(5) = ((newTotalBlock >> 16) & 0xFF).toByte
    APM_BLOCKS(6) = ((newTotalBlock >> 8) & 0xFF).toByte
    APM_BLOCKS(7) = (newTotalBlock & 0xFF).toByte
    // Block 1: Apple_HFS PM
    APM_BLOCKS(0x200 + 0xC) = 0x00
    APM_BLOCKS(0x200 + 0xD) = 0x00
    APM_BLOCKS(0x200 + 0xE) = (availableTotalBlock >> 8).toByte
    APM_BLOCKS(0x200 + 0xF) = (availableTotalBlock & 0xFF).toByte

  private def isEmptyDisk(in:FileInputStream): Boolean =
    val buffer = Array.ofDim[Byte](1024)
    var isEmpty = true
    var read = in.read(buffer)
    while read != -1 && isEmpty do
      for i <- buffer.indices do
        if buffer(i) != 0 then
          isEmpty = false
          read = -1
    isEmpty


  private def mapFile(): MappedByteBuffer =
    val storageFile = new File(file)
    if !storageFile.exists() then
      throw new FileNotFoundException(file)
    if (storageFile.length() % BLOCK_SIZE) != 0 then
      println(s"File $file's length is not a multiple of block size $BLOCK_SIZE'")

    // check if APM (Apple Partition Map) is present
    val in = new FileInputStream(storageFile)
    val ddrMagic = in.read << 24 | in.read << 16 | in.read << 8 | in.read

    if ddrMagic != 0x45_52_02_00 then // is it a valid disk with partitions ?
      if ddrMagic == 0 then // no, is it an empty disk ?
        if !isEmptyDisk(in) then
          initAPM(storageFile.length().toInt)
      else
        initAPM(storageFile.length().toInt)

    in.close()

    val channel = Files.newByteChannel(storageFile.toPath,StandardOpenOption.READ,StandardOpenOption.WRITE).asInstanceOf[FileChannel]
    channel.map(FileChannel.MapMode.READ_WRITE,0,channel.size())

  override protected def readBlocks(block:Int,size:Int): Option[Array[Byte]] =
    if block >= apmBlockOffset then
      val pos = (block - apmBlockOffset) * BLOCK_SIZE
      if pos + size * BLOCK_SIZE > storage.capacity() then
        return None

      val data = Array.ofDim[Byte](size * BLOCK_SIZE)
      storage.get(pos,data,0,data.length)
      Some(data)
    else if block + size < apmBlockOffset then
      val data = Array.ofDim[Byte](size * BLOCK_SIZE)
      System.arraycopy(APM_BLOCKS,block * BLOCK_SIZE,data,0,data.length)
      Some(data)
    else
      for r1 <- readBlocks(block,apmBlockOffset - block)
          r2 <- readBlocks(apmBlockOffset,size - apmBlockOffset) yield
        r1 ++ r2

  override protected def writeBlocks(block:Int,data:Array[Byte],offset:Int,size:Int): Boolean =
    var blockSize = size >> 9
    if blockSize == 0 then blockSize = 1

    if block >= apmBlockOffset then
      val pos = (block - apmBlockOffset) * BLOCK_SIZE
      if pos + BLOCK_SIZE > storage.capacity() then
        return false

      storage.put(pos,data,offset,size)
      true
    else if block + blockSize < apmBlockOffset then
      System.arraycopy(data,offset,APM_BLOCKS,block * 512,size)
      true
    else
      writeBlocks(block,data,offset,(apmBlockOffset - block) * 512) && writeBlocks(apmBlockOffset,data,offset + (apmBlockOffset - block) * 512,size - (apmBlockOffset - block) * 512)
  override def disconnect(): Unit =
    storage.force()
    
  override def sizeInBytes: Int = storage.capacity()
