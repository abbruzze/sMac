package ucesoft.mac.storage

import ucesoft.mac.{ByteTransferProbe, MACComponent}

object DiskController:
  trait DiskControllerListener:
    def onHeadSelected(driveIndex: Int, head: Int): Unit
    def onTrackChanged(driveIndex: Int, track: TrackPos): Unit
    def onSectorOnHead(driveIndex: Int, sector: Int): Unit
    def onMotorChanged(driveIndex: Int, on: Boolean): Unit
    def onFloppyInserted(driveIndex: Int, image: DiskImage): Unit
    def onFloppyEjected(driveIndex: Int): Unit
    def onFloppyModeChanged(driveIndex: Int, writing: Boolean): Unit
/**
 * @author Alessandro Abbruzzetti
 *         Created on 14/01/2025 18:12  
 */
abstract class DiskController extends MACComponent with ByteTransferProbe:
  override val probeName = "Floppy disk controller"
  private var probeBytes = 0

  override def getAndResetByteAccessed: Int =
    val pb = probeBytes
    probeBytes = 0
    pb
  def incProbeBytes(inc:Int = 1): Unit = probeBytes += inc
  def read(address:Int): Int
  def write(address:Int,value:Int): Unit

  def setHeadSelLine(set:Boolean): Unit
  def setInternalDriveSE(set:Boolean): Unit
  
  def cycle(): Unit
  
  def updatePWMSample(sample:Int): Unit
  
  def addDiskControllerListener(dcl:DiskController.DiskControllerListener): Unit
  def removeDiskControllerListener(l:DiskController.DiskControllerListener): Unit

  def insertFloppy(driveIndex:Int,floppy:DiskImage): Boolean
