package ucesoft.mac.storage

import scala.collection.mutable.ListBuffer

/**
 * @author Alessandro Abbruzzetti
 *         Created on 30/10/2024 16:31
 *
 * Track format:
 * +------+-------------+------+----------+------+-------------+------+----------+-------+-------------+-------+----------+------+----+
 * | GAP1 |Address field| GAP2 |Data field| GAP3 |Address field| GAP2 |Data field| GAP 3 |Address field| GAP 2 |Data field| GAP 3|....|
 * |      |     #0      |      |    #0    |      |     #1      |      |    #1    |       |      #2     |       |     #2   |      |    |
 * +------+-------------+------+----------+------+-------------+------+----------+-------+-------------+-------+----------+------+----+
 *
 *
 */
object GCR:
  final val SECTORS_PER_GROUP_TRACK = Array(12, 11, 10, 9, 8)
  private final val ENC = Array(
    // 0x00 - 0x0F
    0x96, 0x97, 0x9A, 0x9B, 0x9D, 0x9E, 0x9F, 0xA6, 0xA7, 0xAB, 0xAC, 0xAD, 0xAE, 0xAF, 0xB2, 0xB3,
    // 0x10 - 0x1F
    0xB4, 0xB5, 0xB6, 0xB7, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF, 0xCB, 0xCD, 0xCE, 0xCF, 0xD3,
    // 0x20 - 0x2F
    0xD6, 0xD7, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD, 0xDE, 0xDF, 0xE5, 0xE6, 0xE7, 0xE9, 0xEA, 0xEB, 0xEC,
    // 0x30 - 0x3F
    0xED, 0xEE, 0xEF, 0xF2, 0xF3, 0xF4, 0xf5, 0xF6, 0xF7, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF
  )
  final val DEC = ENC.zipWithIndex.map((e,z) => (e,z)).toMap

  enum SectorDecoding:
    case GCRDecodeError
    case SectorMismatch(headerSector:Int,sector:Int)
    case ChecksumError
    case Decoded(track:Int, sector:Int, isDoubleSide:Boolean, sectorData:Array[Byte])
    case SideMismatch
    case TrackMismatch
    case NumberOfSectorsMismatch(side:Int,track:Int,expected:Int,found:Int)

  import SectorDecoding.*

  private final val ADDRESS_MARK = Array(0xD5, 0xAA, 0x96)
  final val ADDRESS_MARK_BYTES = ADDRESS_MARK(0) << 16 | ADDRESS_MARK(1) << 8 | ADDRESS_MARK(2)
  final val ADDRESS_MARK_MASK = 0xFFFFFF
  final val SECTOR_BYTE_POS_IN_ADDRESS_MARK = 2
  private final val DATA_MARK = Array(0xD5, 0xAA, 0xAD)
  private final val BIT_SLIP_SEQ = Array(0xDE, 0xAA, 0xEB)
  private inline val AUTO_SYNC_PATTERN = 0b1111111100

  def sectorsPerTrack(track:TrackPos): Int =
    val offset = track >> 4
    if offset > 4 then 0 else SECTORS_PER_GROUP_TRACK(offset)

  /*
    For gap1, gap2, gap3 filling
   */
  def autoSync(length:Int,track:Track): Unit =
    var l = length
    while l > 0 do
      track.setInt(AUTO_SYNC_PATTERN,10)
      l -= 1

  def writeAddressField(t:Track,_type:Int,track:TrackPos,sector:Int,side:Int): Unit =
    t.setInts(ADDRESS_MARK)
    val toEncode = Array(
      track & 0x3F,
      sector,
      (side << 5) | ((track >> 6) & 1),
      _type
    )
    for v <- toEncode do t.setInt(ENC(v))
    t.setInt(ENC(toEncode(0) ^ toEncode(1) ^ toEncode(2) ^ toEncode(3)))

    t.setInts(BIT_SLIP_SEQ)

  def writeDataField(t:Track,sector:Int,data:Array[Byte]): Unit =
    inline val DATA_LEN = 512 + 12
    if data.length != DATA_LEN then
      throw new IllegalArgumentException(s"Invalid data field size: must be 524, found ${data.length}")

    val checksum = Array(0,0,0)

    t.setInts(DATA_MARK)
    t.setInt(ENC(sector & 0x3F))

    var c = 0
    var dataPtr = 0
    val values = Array[Byte](0,0,0)
    while c < 175 do // (DATA_LEN / 3) + 1
      // rotate left
      checksum(0) = (checksum(0) << 1) | (checksum(0) >> 7)
      values(0) = (data(dataPtr) ^ checksum(0)).toByte
      checksum(2) += (data(dataPtr).toInt & 0xFF) + (checksum(0) >> 8)
      dataPtr += 1

      values(1) = (data(dataPtr) ^ checksum(2)).toByte
      checksum(1) += (data(dataPtr).toInt & 0xFF) + (checksum(2) >> 8)
      dataPtr += 1

      if c == 174 then
        values(2) = 0
      else
        values(2) = (data(dataPtr) ^ checksum(1)).toByte
        checksum(0) += (data(dataPtr).toInt & 0xFF) + (checksum(1) >> 8)
        dataPtr += 1

      checksum(0) &= 0xFF
      checksum(1) &= 0xFF
      checksum(2) &= 0xFF

      t.setInt(ENC(
        ((values(0) >> 2) & 0x30) | ((values(1) >> 4) & 0x0C) | ((values(2) >> 6) & 0x03))
      )
      t.setInt(ENC(values(0) & 0x3F))
      t.setInt(ENC(values(1) & 0x3F))
      if c != 174 then
        t.setInt(ENC(values(2) & 0x3F))

      c += 1
    end while

    t.setInt(ENC(
      ((checksum(2) >> 2) & 0x30) | ((checksum(1) >> 4) & 0x0C) | ((checksum(0) >> 6) & 0x03))
    )
    t.setInt(ENC(checksum(2) & 0x3F))
    t.setInt(ENC(checksum(1) & 0x3F))
    t.setInt(ENC(checksum(0) & 0x3F))

    t.setInts(BIT_SLIP_SEQ)
  end writeDataField

  private def searchPattern(t:Track,pattern:Array[Int],markFirstMark:Boolean): Boolean =
    var sh = 0
    var p = 0
    while !t.isMarkReached do
      sh <<= 1
      sh |= t.getAndMoveOn
      if (sh & 0x80) == 0x80 then
        if (sh & 0xFF) == pattern(p) then
          p += 1
          if p == pattern.length then
            if markFirstMark then t.setMark()
            return true
        else
          p = 0

        sh = 0
    false

  private def checksum(diskByte:Byte,checksums:Array[Int],current:Int): Byte =
    val prev = (current + 2) % 3
    val v = (diskByte ^ checksums(prev)) & 0xFF
    checksums(current) += v
    if (checksums(prev) & 0x100) != 0 then
      checksums(current) += 1
      checksums(prev) &= 0xFF
    v.toByte

  def getNextDecodedSector(t:Track,markFirstMark:Boolean): Option[SectorDecoding] =
    val data = new ListBuffer[Byte]
    if !searchPattern(t, ADDRESS_MARK,markFirstMark) then return None
    try
      val track = DEC(t.getNextByte)
      val headerSector = DEC(t.getNextByte)
      val sideTrack = DEC(t.getNextByte)
      val trackID = track | (sideTrack & 1) << 6
      val format = DEC(t.getNextByte)
      val doubleSide = (sideTrack & 0x20) != 0
      val hcs = DEC(t.getNextByte)

      if hcs != (track ^ headerSector ^ sideTrack ^ format) then
        return Some(ChecksumError)

      if !searchPattern(t, DATA_MARK,markFirstMark = false) then return None
      val sector = DEC(t.getNextByte)
      if headerSector != sector then
        return Some(SectorMismatch(headerSector, sector))
        //println(s"Sector mismatch header=$headerSector sector=$sector")

      var c = 0
      val checksums = Array(0,0,0)
      while c < 175 do
        checksums(2) = (checksums(2) & 0xFF) << 1 
        if (checksums(2) & 0x100) != 0 then checksums(2) |= 1 
        
        val d3 = DEC(t.getNextByte).toByte
        val d0 = DEC(t.getNextByte).toByte
        val d1 = DEC(t.getNextByte).toByte

        val b0 = (d0 | ((d3 << 2) & 0xC0)).toByte
        val b1 = (d1 | ((d3 << 4) & 0xC0)).toByte

        data += checksum(b0,checksums,0)
        data += checksum(b1,checksums,1)

        if c != 174 then
          val d2 = DEC(t.getNextByte).toByte
          val b2 = (d2 | (d3 << 6)).toByte
          data += checksum(b2, checksums, 2)

        c += 1
      end while

      // checksum bytes
      val d3 = DEC(t.getNextByte).toByte
      val d0 = DEC(t.getNextByte).toByte
      val d1 = DEC(t.getNextByte).toByte
      val d2 = DEC(t.getNextByte).toByte

      val b0 = (d0 | ((d3 << 2) & 0xC0)).toByte
      val b1 = (d1 | ((d3 << 4) & 0xC0)).toByte
      val b2 = (d2 | (d3 << 6)).toByte

      if (checksums(0) & 0xFF).toByte != b0 ||
        (checksums(1) & 0xFF).toByte != b1 ||
        (checksums(2) & 0xFF).toByte != b2 then
        Some(ChecksumError)
      else
        Some(Decoded(track = trackID, sector = sector,isDoubleSide = doubleSide,sectorData = data.toArray))
    catch
      case t:java.util.NoSuchElementException =>
        t.printStackTrace()
        Some(SectorDecoding.GCRDecodeError)