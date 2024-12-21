package ucesoft.mac.scsi

/**
 * @author Alessandro Abbruzzetti
 *         Created on 09/12/2024 18:42  
 */
trait SCSIListener:
  def targetSelected(id:Int): Unit
  def noTargetSelected(): Unit
