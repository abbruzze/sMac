package ucesoft.mac.scsi

import java.io.{File, FileNotFoundException, IOException}
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
  
  private val storage = mapFile()

  private def mapFile(): MappedByteBuffer =
    val storageFile = new File(file)
    if !storageFile.exists() then
      throw new FileNotFoundException(file)
    if (storageFile.length() % BLOCK_SIZE) != 0 then
      throw new IOException(s"File $file's length is not a multiple of block size $BLOCK_SIZE'")

    val channel = Files.newByteChannel(storageFile.toPath,StandardOpenOption.READ,StandardOpenOption.WRITE).asInstanceOf[FileChannel]
    channel.map(FileChannel.MapMode.READ_WRITE,0,channel.size())

  def readBlocks(block:Int,size:Int): Option[Array[Byte]] =
    val pos = block * BLOCK_SIZE
    if pos + size * BLOCK_SIZE > storage.capacity() then
      return None

    val data = Array.ofDim[Byte](size * BLOCK_SIZE)
    storage.get(pos,data,0,data.length)
    Some(data)
  override def writeBlocks(block: Int, data: Array[Byte]): Boolean =
    val pos = block * BLOCK_SIZE
    if pos + BLOCK_SIZE > storage.capacity() then
      return false

    storage.put(pos,data,0,data.length)
    true

  override def disconnect(): Unit =
    storage.force()
    
  override def sizeInBytes: Int = storage.capacity()
