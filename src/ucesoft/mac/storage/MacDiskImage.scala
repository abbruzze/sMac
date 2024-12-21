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
  private final val BITS_PER_TRACK_GROUP = Array(74_640,68_240,62_200,55_980,49_760)
  private val WP = if emptyDisk then false else !new File(fileName).canWrite
  private var name : String = ""
  private var isDC42Format = false

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

  override def isModified: Boolean = tracks.exists(_.exists(_.isModified))
  override def diskName: String = name
  override def getHeadCount: Int = encoding.sides
  override def getTrackCount: Int = 80

  override def getTrack(head:Int,pos: TrackPos): Track = tracks(head)(pos)
  
  override def eject(flush:Boolean): Unit =
    if flush then
      val error = flushChangesOnHostDisk()
      MessageBus.send(MessageBus.FloppyEjected(this,name,error))

  override def isWriteProtected: Boolean = WP || isDC42Format // avoid to write back when format is DC42

  private def init(): Unit =
    if emptyDisk then
      initTracks(null,null)
    else
      loadFromFile()

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
  private def loadFromFile(): Unit =
    var in: RandomAccessFile = null

    try
      var data : Array[Byte] = null
      var tags : Array[Byte] = null
      val file = new File(fileName)
      // check if it's a raw file
      if file.length() == GCR400K.rawByteSize || file.length() == GCR800K.rawByteSize then
        data = java.nio.file.Files.readAllBytes(file.toPath)
        name = file.getName
        if file.length() == GCR400K.rawByteSize then
          encoding = GCR400K
        else
          encoding = GCR800K
      else
        in = new RandomAccessFile(file, "r")
        in.seek(0x52) // Magic number
        if in.read() != 0x01 || in.read() != 0x00 then
          throw new IllegalArgumentException(s"Invalid format of DC42 image $fileName")
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
        data = Array.ofDim[Byte](dataSize)
        val readDataSize = in.read(data)
        if readDataSize != dataSize then
          throw new IllegalArgumentException(s"Invalid data size: expected $dataSize, found $readDataSize")
        tags = Array.ofDim[Byte](tagSize)
        val readTagSize = in.read(tags)
        if readTagSize != tagSize then
          throw new IllegalArgumentException(s"Invalid tag size: expected $tagSize, found $readTagSize")
        
        isDC42Format = true
      initTracks(data,tags)
    finally
      if in != null then
        in.close()
  end loadFromFile

  private def initTracks(data:Array[Byte],tags:Array[Byte]): Unit =
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
  end initTracks

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
    if !isModified || isDC42Format then return None

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
    val out = new BufferedOutputStream(new FileOutputStream(fileName))
    for s <- sectors do
      out.write(s)

    out.close()

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