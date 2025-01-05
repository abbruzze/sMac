package ucesoft.mac.storage

import ucesoft.mac.storage.DiskImage.DiskEncoding.*
import ucesoft.mac.storage.DiskImage.{BITS_PER_TRACK_GROUP, DiskEncoding}
import ucesoft.mac.storage.GCR.SectorDecoding.*

import java.io.{BufferedOutputStream, File, FileOutputStream}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
 * @author Alessandro Abbruzzetti
 *         Created on 05/01/2025 19:58  
 */
object GCREncoder:
  def encodeGCRTracks(tracks:Array[Array[Track]],encoding: DiskEncoding,data: Array[Byte], tags: Array[Byte]): Unit =
    val emptyDisk = data == null && tags == null
    
    var tmp: Array[Byte] = Array()
    var offset = 0
    for track <- 0 until 80 do
      for side <- 0 to 1 do
        val trackGroup = track >> 4
        val emptyTrack = emptyDisk || side >= encoding.sides
        val t = if emptyTrack then
          val track = new Track(BITS_PER_TRACK_GROUP(trackGroup))
          track.fillRandom()
          track
        else
          new Track()
        if !emptyTrack then
          // GAP 1
          GCR.autoSync(24, t)
          val interleave = encoding.interleave(trackGroup)
          for _sector <- interleave do
            val sector = offset + _sector
            val tagAndSector = Array.ofDim[Byte](12 + 512)
            if tags != null then // if null tags remain as 12 byte of 0
              System.arraycopy(tags, sector * 12, tagAndSector, 0, 12)
            System.arraycopy(data, sector * 512, tagAndSector, 12, 512)
            if track == 0 && sector == 0 && side == 0 then tmp = tagAndSector
            // add header
            GCR.writeAddressField(t, encoding.format, track, _sector, side)
            // GAP 2
            GCR.autoSync(16, t) // 7
            // add data
            GCR.writeDataField(t, _sector, tagAndSector)
            // GAP 3
            GCR.autoSync(27, t) // 20
          offset += interleave.length
        t.finish()
        tracks(side)(track) = t
  end encodeGCRTracks
  
  def writeBackToDisk(fileName:String,tracks:Array[Array[Track]],encoding: DiskEncoding): Either[String,DiskEncoding] =
    decodeSectors(tracks,encoding,dropTags = true) match
      case Left(GCRDecodeError) => Left("Error while decoding GCR byte")
      case Left(ChecksumError) => Left("Checksum error")
      case Left(SideMismatch) => Left("Side ID mismatch")
      case Left(TrackMismatch) => Left("Track ID mismatch")
      case Left(NumberOfSectorsMismatch(side, track, expected, found)) => Left(s"Number of sector mismatch on side $side, track $track: expected: $expected, found $found")
      case Left(err) => Left("Error while decoding track ...")
      case Right((doubleSide, sectors)) =>
        val newEncoding = if doubleSide then GCR800K else GCR400K
        if new File(fileName).exists() then
          val out = new BufferedOutputStream(new FileOutputStream(fileName))
          for s <- sectors do
            out.write(s)
      
          out.close()
        else
          println(s"File $fileName does not exist anymore, skipped saving")
        Right(newEncoding)

  private def decodeSectors(tracks:Array[Array[Track]],encoding: DiskEncoding,dropTags: Boolean): Either[GCR.SectorDecoding, (Boolean, List[Array[Byte]])] =
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
        decodeSectorsForTrack(tracks,side = side, t) match
          case Left(e) =>
            println(s"$side/$t error $e")
            return Left(e)
          case Right(sd) =>
            if sd.length != GCR.SECTORS_PER_GROUP_TRACK(group) then
              return Left(NumberOfSectorsMismatch(side = side, track = t, expected = GCR.SECTORS_PER_GROUP_TRACK(group), found = sd.length))
            val sorted = sd.sortBy(_.sector)
            sectors.addAll(sorted.map(sd => if dropTags then sd.sectorData.drop(12) else sd.sectorData))
        side += 1
      end while
      t += 1
    end while
    Right((doubleSide, sectors.toList))
  end decodeSectors

  private def decodeSectorsForTrack(tracks:Array[Array[Track]],side: Int, t: TrackPos): Either[GCR.SectorDecoding, Array[GCR.SectorDecoding.Decoded]] =
    import GCR.SectorDecoding.*
    val track = tracks(side)(t)
    track.resetPositionTo()
    var markFirstAddress = true
    try
      val sectors = new ArrayBuffer[Decoded]
      var finished = false
      while !finished do
        //println(s"Flushing side $side track $t sectors=${sectors.length}")
        GCR.getNextDecodedSector(track, markFirstAddress) match
          case Some(d@Decoded(trackID, _, isDoubleSide, _)) =>
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
  end decodeSectorsForTrack
  
