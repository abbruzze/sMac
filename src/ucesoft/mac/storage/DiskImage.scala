package ucesoft.mac.storage

type TrackPos = Int

object DiskImage:
  final val BITS_PER_TRACK_GROUP = Array(74_640,68_240,62_200,55_980,49_760)
  
  enum ImageFormat(val writeable:Boolean):
    case RAW extends ImageFormat(true)
    case DC42 extends ImageFormat(false)
    case MOOF extends ImageFormat(true)

  enum DiskEncoding(val rawByteSize: Int,
                    val sides: Int,
                    val interleave: Array[Array[Int]],
                    val format: Int):
    case GCR400K extends DiskEncoding(409_600, 1, GCR.SECTORS_PER_GROUP_TRACK.map(getSectorInterleave(2, _)), 0x2)
    case GCR800K extends DiskEncoding(819_200, 2, GCR.SECTORS_PER_GROUP_TRACK.map(getSectorInterleave(4, _)), 0x22)

  private def getSectorInterleave(format: Int, sectorsPerTrack: Int): Array[Int] =
    val sectors = Array.fill(sectorsPerTrack)(0xFF)
    var dest = 0
    for c <- sectors.indices do
      while sectors(dest) != 0xFF do
        dest = (dest + 1) % sectors.length
      sectors(dest) = c
      dest = (dest + (format & 0x1F)) % sectors.length

    sectors
  end getSectorInterleave
  
end DiskImage
/**
 * @author Alessandro Abbruzzetti
 *         Created on 29/10/2024 18:02  
 */
trait DiskImage:
  def diskName: String
  def getHeadCount: Int
  def getTrackCount: Int
  def getTrack(head:Int,pos:TrackPos): Track
  def eject(flush:Boolean): Unit
  def isWriteProtected: Boolean
  def isModified: Boolean
  def flushChangesOnHostDisk(): Option[String]
  def getFormat: DiskImage.ImageFormat
