package ucesoft.mac.storage

import ucesoft.mac.storage.DiskImage.DiskEncoding
import ucesoft.mac.storage.DiskImage.DiskEncoding.{MFM1440K, MFM720K}

import java.io.FileOutputStream

/**
 * @author Alessandro Abbruzzetti
 *         Created on 06/01/2025 18:47
 *
 * MFM track format:
 *
 * The start of a track is aligned with a physical index pulse that is built into the drive. This index
 * pulse occurs once per revolution.
 *
 *         4E .. 4E | 00 .. 00 | C2 C2 C2 FC | 4E .. 4E | sector 1| sector 2| ... | sector 18 | 4E .. 4E |
 *           Gap to     Sync     Index Mark      Gap to                                          Gap to
 *           index      Field                     first                                          end of
 *           mark                                sector                                          track
 *           (80)        (12)        (4)          (50)                                            (??)
 *
 * Right after the index pulse is a field called the index mark that marks the start of the track. It is
 * composed of three "mark byte" $C2s followed by a normal $FC byte. Notice that the first of a set of
 * mark bytes must be preceeded by a set of twelve zeroes. These zeros synchronize the hardware to the
 * bits being read from the disk. The hardware then looks for the first non-2μsec pulse-to-pulse time
 * (remember that sets of zeroes have a clock pulse between the bit cells), which will hopefully be the start
 * of a mark byte.
 *
 * The sectors are separated from each other by a fixed number of gap bytes. This gap is 84 bytes
 * long on single-density disks and 101 bytes long on double-density disks. After the last sector there is a
 * large gap that fills the empty space from there to the start of the next index pulse. Currently two disk
 * formats are being used: the single-density format uses the same double-sided media as the 800K GCR
 * disks, has 9 sectors per track side and can hold 720K bytes. The double-density format requires a
 * different kind of media (marked with "HD"), has 18 sectors per track side and can hold 1440K bytes.
 *
 *            00 .. 00 | A1 A1 A1 FE | tt | sd | ss | bs | CRC | 4E .. 4E | 00 .. 00 | A1 A1 A1 FB | SECTOR DATA | CRC | 4E .. 4E |
 *              Sync     Address      trk  side sect bls   crc    Intra       Sync     Data Mark     Sector bytes  crc     Inter
 *              field    mark         Address field             sector gap    field                                     sector gap
 *              (12)      (4)                (4)           (2)    (22)        (12)        (4)          (512)      (2)   (84/101)
 *
 * Each of the sectors is made up of an address field and a data field. Again, note that before the
 * first of a set of mark bytes is a 12-byte sync field of all zeroes which synchronizes the hardware with
 * the data being read from the disk.
 * The address field consists of a 4-byte address mark, the track, side and sector numbers, and a
 * byte that tells how big the sector is. This byte should be $02 for both 720K and 1440K disks. Finally,
 * there is a 2-byte CRC checksum which is calculated on all the just-mentioned bytes.
 * The data field contains a 4-byte data mark, 512 bytes of data, and a 2-byte CRC. The 12 bytes
 * of tag information included in GCR sectors is not supported on MFM disks.
 *
 * Synchronization is accomplished by means of the sync field and the mark bytes that follow it.
 * Before writing an index, address, or data mark, the writer writes a sync field of twelve 0x00 bytes.
 * This appears to the unsynchronized reader as a sequence of 96 flux transitions, each separated by two microseconds.
 * The reader can look for this sequence immediately followed by three mark bytes and, on finding it, know that it is synchronized.
 */
object MFM:
  private inline val INDEX_MARK             = 0xC2
  inline val INDEX_MARK_NEXT_BYTE           = 0xFC
  inline val ADDRESS_MARK                   = 0xA1
  inline val ADDRESS_MARK_NEXT_BYTE         = 0xFE
  inline val CRC_MARK                       = 52660 // crc of A1 x 3
  private inline val CRC_ADDRESS_MARK       = 45616 // 45616 = crc of A1 x 3, FE x 1
  inline val DATA_MARK                      = 0xA1
  inline val DATA_MARK_NEXT_BYTE            = 0xFB
  private inline val CRC_DATA_MARK          = 58005 // 58005 crc of A1 x 3, FB x 1
  inline val CRC_RESET_VALUE                = 0xFFFF

  final val MARK = byte2MFM(DATA_MARK,isMark = true)
  final val I_MARK = byte2MFM(INDEX_MARK,isMark = true)
  inline val MARK_BYTE_SIZE = 3
  inline val SECTOR_BYTE_POS = 3

  private inline val MFM_TRACK_BITS = 200_000

  private final val crcCache = {
    val cache = Array.ofDim[Int](256)
    for i <- 0 to 255 do
      var w = i << 8
      for _ <- 0 to 7 do
        if (w & 0x8000) == 0x8000 then
          w <<= 1
          w ^= 0x1021
        else
          w <<= 1
      cache(i) = w & 0xFFFF

    cache
  }

  def main(args:Array[String]): Unit =
    def fillz(s:String): String = ("0" * (16 - s.length)) + s
    println(fillz(byte2MFM(0xA1,true).toBinaryString).sliding(4,4).mkString(" "))
    println(fillz(byte2MFM(0xA1,false).toBinaryString).sliding(4,4).mkString(" "))
    println(byte2MFM(0xC2,true).toHexString)
    println(0xA1.toBinaryString)
    //println(byte2MFM(0xA1).toBinaryString.sliding(2,2).mkString("_"))
    println(mfm2Byte(byte2MFM(0xA1,true)).toHexString)

  /**
   * Encode a byte into a word: the odd bits represents the byte, the even ones the clock.
   * If isMark is true a clock on 3rd position will be dropped, otherwise the clocks will follow the MFM encoding:
   * A pair of "O"s causes a clock pulse to occur on the common cell boundary of the two "O"s.
   */
  def byte2MFM(byte:Int,isMark:Boolean = false): Int =
    var mfm = 0
    var prev = false
    var bit = 7
    while bit >= 0 do
      val b = (byte & (1 << bit)) != 0
      if b then mfm |= 1 << (bit << 1)
      if bit < 7 then
        if !prev && !b then // 00
          mfm |= 1 << ((bit << 1) + 1)
      prev = b
      bit -= 1
    if isMark then
      // drop 3rd clock bit
      mfm &= ~(1 << 5)
    mfm
  end byte2MFM

  /**
   * Decode a 16 bit mfm value into an 8 bit value, dropping clock bits.
   */
  def mfm2Byte(mfm:Int): Int =
    var byte = 0
    var bit = 7
    while bit >= 0 do
      byte |= (mfm & (1 << (bit << 1))) >> bit
      bit -= 1
    byte

  def crc(b:Int,crc:Int = 0xFFFF) : Int =
    val _crc = crc & 0xFFFF
    val _b = b & 0xFF
    (crcCache(_crc >> 8 ^ _b) ^ _crc << 8) & 0xFFFF

  def encodeMFMTracks(tracks: Array[Array[Track]],diskFormat:DiskImage.DiskEncoding, data: Array[Byte]): Unit =
    val emptyDisk = data == null

    for t <- 0 to 79 do
      for side <- 0 to 1 do
        tracks(side)(t) = if emptyDisk then new Track(MFM_TRACK_BITS) else encodeTrack(t,side,diskFormat,data)
  end encodeMFMTracks

  private def encodeTrack(trackId:TrackPos,side:Int,diskFormat:DiskImage.DiskEncoding,data:Array[Byte]): Track =
    val track = new Track()
    // 1. Gap to index mark (80)
    for _ <- 1 to 80 do track.setInt(byte2MFM(0x4E),bits = 16)
    // 2. Sync field (12)
    for _ <- 1 to 12 do track.setInt(byte2MFM(0x00),bits = 16)
    // 3. Index mark (4)
    for _ <- 1 to 3 do track.setInt(byte2MFM(INDEX_MARK,isMark = true),bits = 16)
    track.setInt(byte2MFM(INDEX_MARK_NEXT_BYTE),bits = 16)
    // 4. Gap to first sector (50)
    for _ <- 1 to 50 do track.setInt(byte2MFM(0x4E),bits = 16)
    // sectors
    val bytesPerTrack = diskFormat.interleave(0).length * 512
    val interleave = diskFormat.interleave(0) // only 1 group; MFM sectors start from 1 (not 0)
    val trackOffset = (trackId * 2 + side) * bytesPerTrack // 2 sides, 512 byte per sector
    for sector <- interleave do
      val sectorOffset = trackOffset + (sector - 1) * 512 // sectors start from 1 (not 0)
      // 5. Sync field (12)
      for _ <- 1 to 12 do track.setInt(byte2MFM(0x00), bits = 16)
      // 6. Address mark (4)
      for _ <- 1 to 3 do track.setInt(byte2MFM(ADDRESS_MARK, isMark = true), bits = 16)
      track.setInt(byte2MFM(ADDRESS_MARK_NEXT_BYTE), bits = 16)
      // 7. Address field (4)
      track.setInt(byte2MFM(trackId), bits = 16)
      track.setInt(byte2MFM(side), bits = 16)
      track.setInt(byte2MFM(sector), bits = 16)
      track.setInt(byte2MFM(0x02), bits = 16) // block size: 2 = 512 byte
      // 8. CRC (2)
      var _crc = crc(trackId,CRC_ADDRESS_MARK)
      _crc = crc(side,_crc)
      _crc = crc(sector,_crc)
      _crc = crc(0x02,_crc)
      track.setInt(byte2MFM(_crc >> 8), bits = 16)
      track.setInt(byte2MFM(_crc & 0xFF), bits = 16)
      // 9. Intra sector gap (22)
      for _ <- 1 to 22 do track.setInt(byte2MFM(0x4E), bits = 16)
      // 10. Sync field (12)
      for _ <- 1 to 12 do track.setInt(byte2MFM(0x00), bits = 16)
      // 11. Data mark (4)
      for _ <- 1 to 3 do track.setInt(byte2MFM(DATA_MARK, isMark = true), bits = 16)
      track.setInt(byte2MFM(DATA_MARK_NEXT_BYTE), bits = 16)
      // 12. Sector bytes (512)
      _crc = CRC_DATA_MARK
      for d <- sectorOffset until sectorOffset + 512 do
        track.setInt(byte2MFM(data(d)), bits = 16)
        _crc = crc(data(d), _crc)
      // 13. CRC (2)
      track.setInt(byte2MFM(_crc >> 8), bits = 16)
      track.setInt(byte2MFM(_crc & 0xFF), bits = 16)
      // 14. Inter-sector gap (84/101)
      val gap = diskFormat match
        case DiskImage.DiskEncoding.MFM720K => 84
        case DiskImage.DiskEncoding.MFM1440K => 101
        case _ => 0
      for _ <- 1 to gap do track.setInt(byte2MFM(0x4E), bits = 16)
    track.finish()
    track
  end encodeTrack

  private def findDataMark(track:Track,nextByte:Int,markTrack:Boolean): Boolean =
    var sr = 0
    var dmCount = 0
    val buffer = Array(0,0,0)

    while !track.isMarkReached do
      sr <<= 1
      sr |= track.getAndMoveOn

      if (sr & 0xFFFF) == MARK then
        for i <- 0 to 2 do
          buffer(i) = track.getNextByte << 8 | track.getNextByte
          if i == 2 then buffer(i) = mfm2Byte(buffer(i))
        if buffer(0) == MARK && buffer(1) == MARK && buffer(2) == nextByte then
          if markTrack then track.setMark()
          return true
        sr = 0
    end while

    false
  end findDataMark

  def writeBackToDisk(fileName:String,tracks:Array[Array[Track]],encoding: DiskEncoding): Option[String] =
    var track = 0
    val sides = encoding match
      case MFM720K => 1
      case MFM1440K => 2
      case _ =>
        throw new IllegalArgumentException(s"Invalid disk encoding for MFM: $encoding")
        
    val sectors = Array.ofDim[Array[Byte]](80 * sides)
    var s = 0
    while track < 80 do
      var side = 0
      while side < sides do
        decodeTrack(tracks(side)(track), track, side, encoding) match
          case Left(err) =>
            return Some(err)
          case Right(bytes) =>
            sectors(s) = bytes
        side += 1
        s += 1
      end while
      track += 1
    end while

    val out = new FileOutputStream(fileName) 
    try
      for t <- sectors do 
        out.write(t)
    finally
      out.close()
    None
  end writeBackToDisk

  private def decodeTrack(track:Track,trackID:TrackPos,side:Int,encoding: DiskEncoding): Either[String,Array[Byte]] =
    val totalSectors = if encoding == MFM720K then 9 else 18
    val sectors = Array.ofDim[Byte](totalSectors * 512)
    var firstMark = true
    var sectorsFound = 0

    track.resetPositionTo()
    track.setMark()

    while sectorsFound < totalSectors do
      // find address mark
      if !findDataMark(track,ADDRESS_MARK_NEXT_BYTE,firstMark) then return Left(s"Address mark not found on track $trackID side $side")
      firstMark = false
      val tid = mfm2Byte(track.getNextByte << 8 | track.getNextByte)
      val sd = mfm2Byte(track.getNextByte << 8 | track.getNextByte)
      val sector = mfm2Byte(track.getNextByte << 8 | track.getNextByte)
      //println(s"[$sectorsFound/$totalSectors]Found address mark: track $tid side $sd sector $sector")
      if tid != trackID then return Left(s"Track ID mismatch on track $trackID side $side: found $tid")
      if sd != side then return Left(s"Side mismatch on track $trackID side $side: found $sd")
      if sector < 1 || sector > totalSectors then return Left(s"Invalid sector number on track $trackID side $side: $sector")
      if !findDataMark(track,DATA_MARK_NEXT_BYTE,false) then return Left(s"Data mark not found on track $trackID side $side")
      //println(s"[$sectorsFound/$totalSectors]Found data mark: track $tid side $sd sector $sector")
      // 512 byte of data
      val sectorOffset = (sector - 1) * 512
      for i <- 0 until 512 do
        sectors(sectorOffset + i) = mfm2Byte(track.getNextByte << 8 | track.getNextByte).toByte
      sectorsFound += 1
    end while

    Right(sectors)
  end decodeTrack
