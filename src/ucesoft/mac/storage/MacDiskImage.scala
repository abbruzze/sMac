package ucesoft.mac.storage

import ucesoft.mac.MessageBus
import ucesoft.mac.storage.GCR.SectorDecoding.NumberOfSectorsMismatch

import java.io.{BufferedOutputStream, File, FileOutputStream, RandomAccessFile}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
/**
 * @author Alessandro Abbruzzetti
 *         Created on 30/10/2024 15:20  
 */
class MacDiskImage(val fileName:String,private val emptyDisk:Boolean = false) extends DiskImage:
  import DiskImage.ImageFormat

  private final val BITS_PER_TRACK_GROUP = Array(74_640,68_240,62_200,55_980,49_760)
  private val WP = if emptyDisk then false else !new File(fileName).canWrite
  private var name : String = ""
  private var imageFormat : ImageFormat = ImageFormat.RAW

  private val tracks = Array.ofDim[Track](2,80)

  private enum GCREncoding(val rawByteSize:Int,
                           val sides:Int,
                           val interleave:Array[Array[Int]],
                           val format: Int):
    case GCR400K extends GCREncoding(409_600,1,GCR.SECTORS_PER_GROUP_TRACK.map(getSectorInterleave(2,_)),0x2)
    case GCR800K extends GCREncoding(819_200,2,GCR.SECTORS_PER_GROUP_TRACK.map(getSectorInterleave(4,_)),0x22)

  import GCREncoding.*

  private var encoding : GCREncoding = GCR400K

  init()

  def getDiskFileName: Option[String] = Option(fileName)

  override def getFormat: ImageFormat = imageFormat
  override def isModified: Boolean = tracks.exists(_.exists(_.isModified))
  override def diskName: String = name
  override def getHeadCount: Int = encoding.sides
  override def getTrackCount: Int = 80

  override def getTrack(head:Int,pos: TrackPos): Track = tracks(head)(pos)
  
  override def eject(flush:Boolean): Unit =
    if flush then
      val error = flushChangesOnHostDisk()
      MessageBus.send(MessageBus.FloppyEjected(this,name,error))

  override def isWriteProtected: Boolean = WP || !imageFormat.writeable // avoid to write back when format is DC42

  private def init(): Unit =
    if emptyDisk then
      encodeGCRTraks(null,null)
    else
      loadFromFile()

  private def loadFromFile(): Unit =
    val file = new File(fileName)
    if !checkRawFormat(file) && !checkDC42Format(file) && !checkMOOFFormat(file) then
      throw new IllegalArgumentException(s"Invalid disk image format: $fileName")
  end loadFromFile

  private def checkRawFormat(file:File): Boolean =
    if file.length() == GCR400K.rawByteSize || file.length() == GCR800K.rawByteSize then
      val data = java.nio.file.Files.readAllBytes(file.toPath)
      name = file.getName
      if file.length() == GCR400K.rawByteSize then
        encoding = GCR400K
      else
        encoding = GCR800K

      encodeGCRTraks(data, null)
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
        case e => throw new IllegalArgumentException(s"Invalid disk encoding: $e")
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
      encodeGCRTraks(data,tags)
      true
    finally
      in.close()
  end checkDC42Format

  private def checkMOOFFormat(file:File): Boolean =
    val HEADER = Array[Byte](0x4D, 0x4F, 0x4F, 0x46, 0xFF.toByte, 0x0A, 0x0D, 0x0A)
    val in = new RandomAccessFile(file, "r")
    try
      val header = Array.ofDim[Byte](8)
      in.read(header)
      if !header.sameElements(HEADER) then
        return false

      val tmap = Array.ofDim[Int](80,2)

      in.skipBytes(4) // skip CRC
      val chunkHeader = Array.ofDim[Byte](8)
      var continueReading = in.read(chunkHeader) == 8
      while continueReading do
        val chunkSize = (chunkHeader(7).toInt & 0xFF) << 24 | (chunkHeader(6).toInt & 0xFF) << 16 | (chunkHeader(5).toInt & 0xFF) << 8 | (chunkHeader(4).toInt & 0xFF)
        val chunk = Array.ofDim[Byte](chunkSize)
        in.read(chunk)
        new String(chunkHeader, 0, 4) match
          case "INFO" =>
            // disk type
            if chunk(1) != 1 && chunk(1) != 2 then
              println(s"MOOF unsupported encoding type: ${chunk(1)}")
              return false
            encoding = chunk(1) match
              case 1 => GCR400K
              case 2 => GCR800K
          case "TMAP" =>
            for t <- 0 to 79 do
              for s <- 0 to 1 do
                tmap(t)(s) = chunk(t * 2 + s).toInt & 0xFF
          case "TRKS" =>
            for t <- 0 to 79 do
              for s <- 0 to 1 do
                if tmap(t)(s) == 0xFF then
                  tracks(s)(t) = new Track(BITS_PER_TRACK_GROUP(t >> 4)) 
                else
                  tracks(s)(t) = new Track()
                  val tpos = tmap(t)(s) << 3 // 8 bytes each entry
                  val startingBlock = (chunk(tpos + 1).toInt & 0xFF) << 8 | (chunk(tpos).toInt & 0xFF)
                  val blockCount = (chunk(tpos + 3).toInt & 0xFF) << 8 | (chunk(tpos + 2).toInt & 0xFF)
                  var bitCount = (chunk(7).toInt & 0xFF) << 24 | (chunk(6).toInt & 0xFF) << 16 | (chunk(5).toInt & 0xFF) << 8 | (chunk(4).toInt & 0xFF)
                  var byteCount = bitCount >> 3
                  val trackData = Array.ofDim[Byte](byteCount + (if (bitCount % 8 != 0) 1 else 0))
                  in.seek(startingBlock << 9)
                  in.read(trackData)
  
                  var byteOffset = 0
                  while byteCount > 0 do
                    tracks(s)(t).setInt(trackData(byteOffset).toInt & 0xFF)
                    byteOffset += 1
                    bitCount -= 8
                    byteCount -= 1
                  if bitCount > 0 then
                    var lastByte = trackData(byteOffset).toInt & 0xFF
                    while bitCount > 0 do
                      lastByte <<= 1
                      if (lastByte & 0x100) != 0 then tracks(s)(t).setAndMoveOn() else tracks(s)(t).clearAndMoveOn()
                      bitCount -= 1
                end if
                tracks(s)(t).finish()
          case "META" =>
          case cname =>
            in.skipBytes(chunkSize)

        continueReading = in.read(chunkHeader) == 8
      end while
      imageFormat = ImageFormat.MOOF
//      for t <- 0 to 79 do 
//        for s <- 0 to 1 do
//          decodeSectorsForTrack(s, t) match
//            case Left(e) =>
//              println(s"$t/$s error $e")
//            case Right(_) =>
//              println(s"$t/$s OK!")
    finally
      in.close()

    true
  end checkMOOFFormat

  private def encodeGCRTraks(data:Array[Byte],tags:Array[Byte]): Unit =
    var tmp : Array[Byte] = Array()
    var offset = 0
    for track <- 0 until 80 do
      for side <- 0 to 1  do
        val trackGroup = track >> 4
        val emptyTrack = emptyDisk || side >= encoding.sides
        val t = if emptyTrack then new Track(BITS_PER_TRACK_GROUP(trackGroup)) else new Track()
        if !emptyTrack then
          // GAP 1
          GCR.autoSync(24,t)
          val interleave = encoding.interleave(trackGroup)
          for _sector <- interleave do
            val sector = offset + _sector
            val tagAndSector = Array.ofDim[Byte](12 + 512)
            if tags != null then // if null tags remain as 12 byte of 0
              System.arraycopy(tags,sector * 12,tagAndSector,0,12)
            System.arraycopy(data,sector * 512,tagAndSector,12,512)
            if track == 0 && sector == 0 && side == 0 then tmp = tagAndSector
            // add header
            GCR.writeAddressField(t,encoding.format,track,_sector,side)
            // GAP 2
            GCR.autoSync(16, t) // 7
            // add data
            GCR.writeDataField(t,_sector,tagAndSector)
            // GAP 3
            GCR.autoSync(27, t) // 20
          offset += interleave.length
        t.finish()
        tracks(side)(track) = t
  end encodeGCRTraks

  private def getSectorInterleave(format:Int,sectorsPerTrack:Int): Array[Int] =
    val sectors = Array.fill(sectorsPerTrack)(0xFF)
    var dest = 0
    for c <- sectors.indices do
      while sectors(dest) != 0xFF do
        dest = (dest + 1) % sectors.length
      sectors(dest) = c
      dest = (dest + (format & 0x1F)) % sectors.length

    sectors
  end getSectorInterleave

  override def flushChangesOnHostDisk(): Option[String] =
    import GCR.SectorDecoding.*
    if !isModified || isWriteProtected then return None

    println(s"Flushing $name ...")

    decodeSectors(dropTags = true) match
      case Left(GCRDecodeError) => Some("Error while decoding GCR byte")
      case Left(ChecksumError) => Some("Checksum error")
      case Left(SideMismatch) => Some("Side ID mismatch")
      case Left(TrackMismatch) => Some("Track ID mismatch")
      case Left(NumberOfSectorsMismatch(side,track,expected,found)) => Some(s"Number of sector mismatch on side $side, track $track: expected: $expected, found $found")
      case Right((doubleSide,sectors)) =>
        encoding = if doubleSide then GCR800K else GCR400K
        saveImage(sectors)
        None
      case _ =>
        None

  private def saveImage(sectors:List[Array[Byte]]): Unit =
    if new File(fileName).exists() then
      val out = new BufferedOutputStream(new FileOutputStream(fileName))
      for s <- sectors do
        out.write(s)

      out.close()
    else
      println(s"File $fileName does not exist anymore, skipped saving")

  private def decodeSectors(dropTags:Boolean): Either[GCR.SectorDecoding,(Boolean,List[Array[Byte]])] =
    val sectors = new ListBuffer[Array[Byte]]
    // 1) 400K -> 400K single side, odd tracks are not modified
    // 2) 400K -> 800K double side, odd tracks are formatted (modified)
    // 3) 800K -> 800K double side
    // 4) 800K -> 400K single side, how to handle the case when an 800k floppy is then formatted as 400k ?
    val doubleSide = encoding == GCR800K || (encoding == GCR400K && (1 to 79).exists(tracks(1)(_).isModified))
    val sides = if doubleSide then 2 else 1
    var t = 0
    while t < 80 do
      val group = t >> 4
      var side = 0
      while side < sides do
        decodeSectorsForTrack(side = side, t) match
          case Left(e) =>
            println(s"$side/$t error $e")
            return Left(e)
          case Right(sd) =>
            if sd.length != GCR.SECTORS_PER_GROUP_TRACK(group) then
              return Left(NumberOfSectorsMismatch(side = side,track = t,expected = GCR.SECTORS_PER_GROUP_TRACK(group),found = sd.length))
            val sorted = sd.sortBy(_.sector)
            sectors.addAll(sorted.map(sd => if dropTags then sd.sectorData.drop(12) else sd.sectorData))
        side += 1
      end while
      t += 1
    end while
    Right((doubleSide,sectors.toList))

  private def decodeSectorsForTrack(side:Int,t:TrackPos): Either[GCR.SectorDecoding,Array[GCR.SectorDecoding.Decoded]] =
    import GCR.SectorDecoding.*
    val track = tracks(side)(t)
    track.resetPositionTo()
    var markFirstAddress = true
    try
      val sectors = new ArrayBuffer[Decoded]
      var finished = false
      while !finished do
        //println(s"Flushing side $side track $t sectors=${sectors.length}")
        GCR.getNextDecodedSector(track,markFirstAddress) match
          case Some(d@Decoded(trackID,_,isDoubleSide,_)) =>
            if (side == 0 && isDoubleSide) || (side == 1 && !isDoubleSide) then
              return Left(SideMismatch)
            if t != trackID then
              return Left(TrackMismatch)
            sectors += d
          case Some(e) =>
            return Left(e)
          case None =>
            finished = true
        markFirstAddress = false
      end while
      Right(sectors.toArray)
    finally
      track.clearMark()