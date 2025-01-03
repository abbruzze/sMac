package ucesoft.mac.storage

type TrackPos = Int

object DiskImage:
  enum ImageFormat(val writeable:Boolean):
    case RAW extends ImageFormat(true)
    case DC42 extends ImageFormat(false)
    case MOOF extends ImageFormat(false)
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
