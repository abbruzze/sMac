package ucesoft.mac
import ucesoft.mac.cpu.m68k.Size
/**
 * @author Alessandro Abbruzzetti
 *         Created on 13/11/2024 14:54  
 */
enum MacModel(val clockRateMhz:Int,
              val ramSizesInK:Array[Int],
              val md5:Array[String],
              val skipMemoryTestAddress:Option[(Int,Int,Size)],
              val videoSettings: VideoSettings,
              val audioSettings: AudioSettings,
              val pramExtended:Boolean,
              val scsi:Boolean,
              val floppySettings:FloppySettings):
  private var ramInBytes = ramSizesInK(0)

  def totalRAMInBytes: Int = ramInBytes
  def setTotalRamInK(ramInK:Int): Unit =
    ramInBytes = ramInK * 1024

  case MAC128K extends MacModel(
    clockRateMhz = 7_833_600,
    ramSizesInK = Array(128,512),
    md5 = Array(
      "DB7E6D3205A2B48023FBA5AA867AC6D6", // Mac 128/512, rom size 64K
      "1D7F52D2D490524954F6AFCE083D9593"  // Mac 128/512 V1, rom size 64K
    ),
    skipMemoryTestAddress = None,
    videoSettings = VideoSettings(
      horizontalPixels = 512,
      hblankInPixels = 192,
      verticalLines = 342,
      vblankLines = 28,
      alternateVideoBufferOffset = 0x60_000
    ),
    audioSettings = AudioSettings(alternateAudioBufferOffset = 0x60_000),
    pramExtended = false,
    scsi = false,
    floppySettings = FloppySettings(drivesNumber = 1, doubleDensity = false)
  )
  case PLUS extends MacModel(
    clockRateMhz = 7_833_600,
    ramSizesInK = Array(1024,2 * 1024,4 * 1024),
    md5 = Array(
      "4D8D1E81FA606F57C7ED7188B8B5F410", // Mac Plus/512ke V1, rom size 128K
      "16B516E13918A439CF9031EC24353610", // Mac Plus/512ke V2, rom size 128K
      "8A41E0754FFD1BB00D8183875C55164C", // Mac Plus/512ke V3, rom size 128K
    ),
    skipMemoryTestAddress = Some((0x0002AE,0x0040_0000,Size.Long)),
    videoSettings = VideoSettings(
      horizontalPixels = 512,
      hblankInPixels = 192,
      verticalLines = 342,
      vblankLines = 28,
      alternateVideoBufferOffset = -0x8000
    ),
    audioSettings = AudioSettings(alternateAudioBufferOffset = -0x5C00),
    pramExtended = true,
    scsi = true,
    floppySettings = FloppySettings(drivesNumber = 2, doubleDensity = true)
  )

case class FloppySettings(drivesNumber:Int,
                          doubleDensity:Boolean)
case class AudioSettings(alternateAudioBufferOffset:Int):
  final val audioBufferAddress = 0x3FFD00
  final val alternateAudioBufferAddress = audioBufferAddress + alternateAudioBufferOffset
case class VideoSettings(horizontalPixels:Int, 
                         hblankInPixels:Int, 
                         verticalLines:Int, 
                         vblankLines:Int,
                         alternateVideoBufferOffset:Int):
  final val videoBufferAddress = 0x3FA700
  final val alternateVideoBufferAddress = videoBufferAddress + alternateVideoBufferOffset
  final val totalHorizontalPixels : Int = horizontalPixels + hblankInPixels
  final val totalLines : Int = verticalLines + vblankLines