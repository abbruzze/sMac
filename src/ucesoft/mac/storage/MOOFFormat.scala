package ucesoft.mac.storage

import ucesoft.mac.Version
import ucesoft.mac.storage.DiskImage.DiskEncoding.*
import ucesoft.mac.storage.DiskImage.{BITS_PER_TRACK_GROUP, DiskEncoding}

import java.io.{File, FileOutputStream, RandomAccessFile}
import java.nio.{ByteBuffer, ByteOrder}
import java.time.Instant
import java.time.format.DateTimeFormatter
import java.util.zip.CRC32

/**
 * @author Alessandro Abbruzzetti
 *         Created on 05/01/2025 20:43  
 */
object MOOFFormat:
  case class MOOFFile(encoding:DiskEncoding,diskName:String,writeProtected:Boolean)

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
      var wp = false

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
            if chunk(1) != 1 && chunk(1) != 2 && chunk(1) != 3 then
              println(s"MOOF unsupported encoding type: ${chunk(1)}")
              return None
            encoding = chunk(1) match
              case 1 => GCR400K
              case 2 => GCR800K
              case 3 => MFM1440K
            wp = chunk(2) == 1
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
      Some(MOOFFile(encoding, diskName,wp))
    finally
      in.close()
  end decodeMoofFile

  def encodeMoofFile(fileName:String,tracks:Array[Array[Track]],encoding: DiskEncoding,diskName:String): Unit =
    val out = new FileOutputStream(fileName)
    val crc32 = new CRC32
    // make INFO CHUNK ==============================================
    var buffer = ByteBuffer.allocate(8 + 60)
    buffer.order(ByteOrder.LITTLE_ENDIAN)
    buffer.put("INFO".getBytes) // ‘INFO’ Chunk ID
    buffer.putInt(60)           // Chunk size
    buffer.put(1.toByte)        // Version
    val diskType = encoding match
      case GCR400K => 1
      case GCR800K => 2
      case MFM1440K => 3
      case _ =>
        throw IllegalArgumentException(s"Cannot write MOOF file with $encoding encoding")
    buffer.put(diskType.toByte) // Disk Type
    buffer.put(0.toByte)        // Write Protect
    buffer.put(0.toByte)        // Synchronized
    if encoding == GCR400K || encoding == GCR800K then
      buffer.put(16.toByte)     // Optimal Bit Timing
    else
      buffer.put(8.toByte)      // Optimal Bit Timing
    val creator = s"sMac Emulator ver. ${Version.VERSION}"
    buffer.put((creator + " " * (32 - creator.length)).getBytes) // Creator
    buffer.put(0.toByte)        // Padding
    val largestTrackBlock = math.max(tracks(0).map(_.getBitSize / 8 / 512).max,tracks(1).map(_.getBitSize / 8 / 512).max)
    buffer.putShort(largestTrackBlock.toShort)  // Largest Track
    val infoChunk = buffer.array()
    crc32.update(infoChunk)
    // make TMAP CHUNK =================================================
    buffer = ByteBuffer.allocate(8 + 160)
    buffer.order(ByteOrder.LITTLE_ENDIAN)
    buffer.put("TMAP".getBytes) // ‘TMAP’ Chunk ID
    buffer.putInt(160)          // Chunk size
    var index = 0
    var track = 0
    while track < 160 do
      if (track & 1) == 1 && diskType == 1 then // GCR400K
        buffer.put(0xFF.toByte)
      else
        buffer.put(index.toByte)
        index += 1
      track += 1
    end while
    val tmapChunk = buffer.array()
    crc32.update(tmapChunk)
    // make TRKS CHUNK =================================================
    class TrackBytes(val bytes:Array[Byte],val blockSize:Int,val bitCount:Int)

    val sides = if diskType == 1 then 1 else 2
    val trackBytes = Array.ofDim[TrackBytes](sides,80)
    var totalTrackSize = 0
    for side <- 0 until sides do
      for track <- 0 to 79 do
        val bytes = tracks(side)(track).asArrayOfByte
        val blocks = bytes.length / 512 + (if (bytes.length % 512) != 0 then 1 else 0)
        trackBytes(side)(track) = new TrackBytes(bytes,blocks,tracks(side)(track).getBitSize)
        totalTrackSize += blocks * 512

    buffer = ByteBuffer.allocate(1288 + totalTrackSize)
    buffer.order(ByteOrder.LITTLE_ENDIAN)
    buffer.put("TRKS".getBytes)   // ‘TMAP’ Chunk ID
    buffer.putInt(1280 + totalTrackSize) // Chunk size

    var blockOffset = 1536 // relative to the start of the file
    for track <- 0 to 79 do
      for side <- 0 until sides do
        buffer.putShort((blockOffset >> 9).toShort)  // Starting Block
        buffer.putShort(trackBytes(side)(track).blockSize.toShort)   // Block Count
        buffer.putInt(trackBytes(side)(track).bitCount)               // Bit Count
        blockOffset += trackBytes(side)(track).blockSize * 512

    blockOffset = 1280 + 8
    for track <- 0 to 79 do
      for side <- 0 until sides do
        buffer.position(blockOffset)
        buffer.put(trackBytes(side)(track).bytes)   // BITS
        blockOffset += trackBytes(side)(track).blockSize * 512

    val trksChunk = buffer.array()
    crc32.update(trksChunk)
    // make META CHUNK =================================================
    val imageDate = DateTimeFormatter.ISO_INSTANT.format(Instant.now())
    val meta = s"disk_name=$diskName${0xA.toChar}publisher=$creator${0xA.toChar}image_date=$imageDate"
    buffer = ByteBuffer.allocate(8 + meta.length)
    buffer.order(ByteOrder.LITTLE_ENDIAN)
    buffer.put("META".getBytes) // ‘META’ Chunk ID
    buffer.putInt(meta.length)  // Chunk size
    buffer.put(meta.getBytes)

    val metaChunk = buffer.array()
    crc32.update(metaChunk)

    // HEADER
    out.write("MOOF".getBytes)
    out.write(0xFF)
    out.write(0xA)
    out.write(0xD)
    out.write(0xA)
    val crc = crc32.getValue.toInt
    out.write((crc >> 24) & 0xFF)
    out.write((crc >> 16) & 0xFF)
    out.write((crc >> 8) & 0xFF)
    out.write(crc & 0xFF)

    out.write(infoChunk)
    out.write(tmapChunk)
    out.write(trksChunk)
    out.write(metaChunk)

    out.close()
  end encodeMoofFile