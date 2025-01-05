package ucesoft.mac.storage

import ucesoft.mac.storage.DiskImage.{BITS_PER_TRACK_GROUP, DiskEncoding}
import ucesoft.mac.storage.DiskImage.DiskEncoding.*

import java.io.{File, RandomAccessFile}

/**
 * @author Alessandro Abbruzzetti
 *         Created on 05/01/2025 20:43  
 */
object MOOFFormat:
  case class MOOFFile(encoding:DiskEncoding,diskName:String)

  def decodeMoofFile(file:File,tracks:Array[Array[Track]]): Option[MOOFFile] =
    val HEADER = Array[Byte](0x4D, 0x4F, 0x4F, 0x46, 0xFF.toByte, 0x0A, 0x0D, 0x0A)
    val in = new RandomAccessFile(file, "r")
    try
      val header = Array.ofDim[Byte](8)
      in.read(header)
      if !header.sameElements(HEADER) then
        return None

      val tmap = Array.ofDim[Int](80, 2)
      var encoding = GCR400K
      var diskName = ""

      in.skipBytes(4) // skip CRC
      val chunkHeader = Array.ofDim[Byte](8)
      var continueReading = in.read(chunkHeader) == 8
      while continueReading do
        val chunkSize = (chunkHeader(7).toInt & 0xFF) << 24 | (chunkHeader(6).toInt & 0xFF) << 16 | (chunkHeader(5).toInt & 0xFF) << 8 | (chunkHeader(4).toInt & 0xFF)
        val chunk = Array.ofDim[Byte](chunkSize)
        if in.read(chunk) != chunkSize then
          return None
        new String(chunkHeader, 0, 4) match
          case "INFO" =>
            // disk type
            if chunk(1) != 1 && chunk(1) != 2 then
              println(s"MOOF unsupported encoding type: ${chunk(1)}")
              return None
            encoding = chunk(1) match
              case 1 => GCR400K
              case 2 => GCR800K
          case "TMAP" =>
            for t <- 0 to 79 do
              for s <- 0 to 1 do
                tmap(t)(s) = chunk(t * 2 + s).toInt & 0xFF
          case "TRKS" =>
            val chunkSeekPos = in.getFilePointer
            for t <- 0 to 79 do
              for s <- 0 to 1 do
                if tmap(t)(s) == 0xFF then
                  tracks(s)(t) = new Track(BITS_PER_TRACK_GROUP(t >> 4))
                  tracks(s)(t).fillRandom()
                else
                  tracks(s)(t) = new Track()
                  val tpos = tmap(t)(s) << 3 // 8 bytes each entry
                  val startingBlock = (chunk(tpos + 1).toInt & 0xFF) << 8 | (chunk(tpos).toInt & 0xFF)
                  val blockCount = (chunk(tpos + 3).toInt & 0xFF) << 8 | (chunk(tpos + 2).toInt & 0xFF)
                  val bitCount = (chunk(tpos + 7).toInt & 0xFF) << 24 | (chunk(tpos + 6).toInt & 0xFF) << 16 | (chunk(tpos + 5).toInt & 0xFF) << 8 | (chunk(tpos + 4).toInt & 0xFF)
                  val trackData = Array.ofDim[Byte](blockCount << 9)
                  in.seek(startingBlock << 9)
                  in.read(trackData)

                  var trackBit = 0
                  while trackBit < bitCount do
                    val byte = trackBit >> 3
                    val bit = 7 - (trackBit % 8)
                    val bitToWrite = (trackData(byte) & (1 << bit)) != 0
                    if bitToWrite then tracks(s)(t).setAndMoveOn() else tracks(s)(t).clearAndMoveOn()
                    trackBit += 1

                tracks(s)(t).finish()
            in.seek(chunkSeekPos)
          case "META" =>
            val metaInfo = new String(chunk)
            val kv = metaInfo.split("\n").map(_.split("\t"))
            diskName = kv.find(_(0) == "disk_name").orElse(kv.find(_(0) == "title")).map(_(1)).getOrElse(file.getName)
          case cname =>
            in.skipBytes(chunkSize)

        continueReading = in.read(chunkHeader) == 8
      end while
      Some(MOOFFile(encoding, diskName))
    finally
      in.close()
  end decodeMoofFile
