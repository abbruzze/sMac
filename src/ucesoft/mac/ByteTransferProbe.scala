package ucesoft.mac

/**
 * @author Alessandro Abbruzzetti
 *         Created on 17/01/2025 11:39  
 */
trait ByteTransferProbe:
  val probeName : String
  def getAndResetByteAccessed: Int
