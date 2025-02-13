package ucesoft.mac.storage

import ucesoft.mac.MessageBus
import ucesoft.mac.storage.DiskImage.DiskEncoding
import ucesoft.mac.storage.DiskImage.DiskEncoding.*
import ucesoft.mac.storage.DiskImage.ImageFormat.MOOF

import java.io.{File, RandomAccessFile}
/**
 * @author Alessandro Abbruzzetti
 *         Created on 30/10/2024 15:20  
 */
class MacDiskImage(val fileName:String,private val emptyDisk:Boolean = false) extends DiskImage:
  import DiskImage.ImageFormat

  private var WP = if emptyDisk then false else !new File(fileName).canWrite
  private var name : String = ""
  private var imageFormat : ImageFormat = ImageFormat.RAW

  private val tracks = Array.ofDim[Track](2,80)
  private var encoding : DiskEncoding = GCR400K

  init()

  def getDiskFileName: Option[String] = Option(fileName)

  override def getEncoding: DiskEncoding = encoding
  override def getFormat: ImageFormat = imageFormat
  override def isModified: Boolean = tracks.exists(_.exists(_.isModified))
  override def diskName: String = name
  override def getHeadCount: Int = encoding.sides
  override def getTrackCount: Int = 80

  override def getTrack(head:Int,pos: TrackPos): Track = tracks(head)(pos)
  
  override def eject(flush:Boolean,writeAsMoof:Boolean): Unit =
    if flush then
      val error = flushChangesOnHostDisk(writeAsMoof)
      MessageBus.send(MessageBus.FloppyEjected(this,name,error))

  override def isWriteProtected: Boolean = WP || !imageFormat.writeable // avoid to write back when format is DC42

  private def init(): Unit =
    if emptyDisk then
      GCREncoder.encodeGCRTracks(tracks,encoding,null,null)
    else
      loadFromFile()

  private def loadFromFile(): Unit =
    val file = new File(fileName)
    if !checkRawFormat(file) && !checkMOOFFormat(file) && !checkDC42Format(file) then
      throw new IllegalArgumentException(s"Invalid disk image format: $fileName")
  end loadFromFile

  private def checkRawFormat(file:File): Boolean =
    if file.length() == GCR400K.rawByteSize || file.length() == GCR800K.rawByteSize then
      val data = java.nio.file.Files.readAllBytes(file.toPath)
      name = file.getName
      encoding = if file.length() == GCR400K.rawByteSize then GCR400K else GCR800K
      GCREncoder.encodeGCRTracks(tracks,encoding,data, null)
      imageFormat = ImageFormat.RAW
      true
    else if file.length() == MFM720K.rawByteSize || file.length() == MFM1440K.rawByteSize then
      val data = java.nio.file.Files.readAllBytes(file.toPath)
      name = file.getName
      encoding = if file.length() == MFM720K.rawByteSize then MFM720K else MFM1440K
      MFM.encodeMFMTracks(tracks,encoding,data)
      imageFormat = ImageFormat.RAW
      true
    else
      false
  end checkRawFormat

  /*
     * DiskCopy 4.2 format
     * ====================================================================================
     * 0x00       byte        Length of image name string ('Pascal name length')
     * 0x01-0x3F  63 bytes    Image name, in ascii, padded with NULs
     * 0x40-0x43  BE_UINT32   Data size in bytes (of block starting at 0x54)
     * 0x44-0x47  BE_UINT32   Tag size in bytes (of block starting after end of Data block)
     * 0x48-0x4B  BE_UINT32   Data Checksum
     * 0x4C-0x4F  BE_UINT32   Tag Checksum
     * 0x50       byte        Disk encoding
     * 0x51       byte        Format Byte
     * 0x52-0x53  BE_UINT16   '0x01 0x00' ('Private Word') AKA Magic Number
     * 0x54-...   variable    Image data
     * ...-EOF    variable    Tag data
     */
  private def checkDC42Format(file:File): Boolean =
    val in = new RandomAccessFile(file, "r")
    try
      in.seek(0x52) // Magic number
      if in.read() != 0x01 || in.read() != 0x00 then
        return false
      in.seek(0)
      val nameLen = in.read()
      val sb = new StringBuilder()
      for _ <- 0 until nameLen do
        sb += in.read().toChar
      name = sb.toString.trim
      in.seek(0x40)
      val dataSize = in.read() << 24 | in.read() << 16 | in.read() << 8 | in.read()
      val tagSize = in.read() << 24 | in.read() << 16 | in.read() << 8 | in.read()
      // ignore checksums
      in.seek(0x50)
      encoding = in.read() match
        case 0x00 => GCR400K // GCR CLV ssdd (400k)
        case 0x01 => GCR800K // GCR CLV dsdd (800k)
        case 0x02 => MFM720K // MFM 2sdd (720k)
        case 0x03 => MFM1440K // MFM 2dd (1440k)
      val format = in.read() // skip this byte: format is already defined by encoding...for now
      // image data
      in.seek(0x54)
      val data = Array.ofDim[Byte](dataSize)
      val readDataSize = in.read(data)
      if readDataSize != dataSize then
        println(s"Invalid data size: expected $dataSize, found $readDataSize")
        return false
      val tags = Array.ofDim[Byte](tagSize)
      val readTagSize = in.read(tags)
      if readTagSize != tagSize then
        println(s"Invalid tag size: expected $tagSize, found $readTagSize")
        return false

      imageFormat = ImageFormat.DC42
      if encoding == GCR400K || encoding == GCR800K then
        GCREncoder.encodeGCRTracks(tracks,encoding,data,tags)
      else
        MFM.encodeMFMTracks(tracks,encoding,data)
      true
    finally
      in.close()
  end checkDC42Format

  private def checkMOOFFormat(file:File): Boolean =
    MOOFFormat.decodeMoofFile(file, tracks) match
      case Some(MOOFFormat.MOOFFile(enc,dname,wp)) =>
        imageFormat = ImageFormat.MOOF
        encoding = enc
        name = dname
        if !WP then WP = wp
        true
      case None =>
        false
  end checkMOOFFormat

  private def flushChangesOnHostDisk(writeAsMoof:Boolean): Option[String] =
    if isWriteProtected then return None
    if !isModified && !(writeAsMoof && imageFormat != MOOF) then return None

    println(s"Flushing $name ... ${if writeAsMoof && imageFormat != MOOF then " converting to MOOF" else ""}")

    encoding match
      case GCR400K | GCR800K =>
        if writeAsMoof && imageFormat != MOOF then
          MOOFFormat.encodeMoofFile(s"$fileName.moof",tracks,encoding,diskName)
          None
        else
          imageFormat match
            case ImageFormat.RAW =>
              GCREncoder.writeBackToDisk(fileName, tracks, encoding) match
                case Left(err) => Some(err)
                case Right(newEncoding) =>
                  encoding = newEncoding
                  None
            case ImageFormat.MOOF =>
              MOOFFormat.encodeMoofFile(fileName,tracks,encoding,diskName)
              None
            case f =>
              Some(s"Format $f not supported for GCR")
      case MFM720K | MFM1440K =>
        if writeAsMoof && imageFormat != MOOF then
          MOOFFormat.encodeMoofFile(s"$fileName.moof",tracks,encoding,diskName)
          None
        else
          imageFormat match
            case ImageFormat.RAW =>
              MFM.writeBackToDisk(fileName, tracks, encoding)
            case ImageFormat.MOOF =>
              MOOFFormat.encodeMoofFile(fileName,tracks,encoding,diskName)
              None
            case f =>
              Some(s"Format $f not supported for MFM")