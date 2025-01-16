package ucesoft.mac.ui

import ucesoft.mac.scsi.{SCSIListener, SCSITarget}
import ucesoft.mac.storage.{DiskController, DiskImage, TrackPos}

import java.awt.{Color, FlowLayout, Font}
import javax.swing.*

/**
 * @author Alessandro Abbruzzetti
 *         Created on 02/12/2024 14:31  
 */
class StoragePanel extends JPanel with DiskController.DiskControllerListener with SCSIListener:
  private val disketteDisabled = new ImageIcon(getClass.getResource("/resources/disk_disabled.png"))
  private val disketteOff = new ImageIcon(getClass.getResource("/resources/disk_off.png"))
  private val disketteRead = new ImageIcon(getClass.getResource("/resources/disk_read.png"))
  private val disketteWrite = new ImageIcon(getClass.getResource("/resources/disk_write.png"))

  private class Scsi(var scsiOnIcon:Icon = null,var scsiOffIcon:Icon = null):
    val label = new JLabel

  private class Diskette(val index:Int) extends JPanel:
    private val info = new JLabel(getInfo)
    val icon = new JLabel(disketteDisabled)
    var head = 0
    var track = 0
    var sector = 0
    var doubleSide = false

    init()

    private def getInfo: String =
      if doubleSide then
        "[%d] %d-%02d/%02d".format(index,head,track,sector)
      else
        "[%d] %02d/%02d".format(index,track, sector)

    private def init(): Unit =
      setLayout(new FlowLayout(FlowLayout.LEFT))
      add(icon)
      info.setFont(Font.getFont(Font.MONOSPACED))
      add(info)

    def updateInfo(): Unit = info.setText(getInfo)
  end Diskette

  private var diskette : Array[Diskette] = Array()
  private val scsi = Array.fill(8)(new Scsi())
  
  def setSCSI(s:SCSITarget): Unit =
    scsi(s.id).scsiOnIcon = s.scsiOn
    scsi(s.id).scsiOffIcon = s.scsiOff
    
    scsi(s.id).label.setIcon(s.scsiOff)
    add(scsi(s.id).label)
    scsi(s.id).label.setText(s.id.toString)
    scsi(s.id).label.setToolTipText(s.name)
  def removeSCSI(id:Int): Unit =
    remove(scsi(id).label)

  def setDiskette(size:Int): Unit =
    removeAll()
    diskette = (0 until size).map(new Diskette(_)).toArray
    diskette.foreach(add(_))

  private def init(): Unit =
    setLayout(new FlowLayout(FlowLayout.LEFT))
    setBorder(BorderFactory.createLineBorder(Color.BLACK))

  private def swing(event: => Unit): Unit =
    SwingUtilities.invokeLater(() => event)
    
  override def targetSelected(id:Int): Unit =
    swing { scsi(id).label.setIcon(scsi(id).scsiOnIcon) }

  override def noTargetSelected(): Unit =
    swing {
      for s <- scsi do 
        s.label.setIcon(s.scsiOffIcon)
    }

  override def onHeadSelected(driveIndex: Int, head: Int): Unit =
    if driveIndex < diskette.length then
      swing {
        diskette(driveIndex).head = head
        diskette(driveIndex).updateInfo()
      }
  override def onTrackChanged(driveIndex: Int, track: TrackPos): Unit =
    if driveIndex < diskette.length then
      swing {
        diskette(driveIndex).track = track
        diskette(driveIndex).updateInfo()
      }
  override def onSectorOnHead(driveIndex: Int, sector: Int): Unit =
    if driveIndex < diskette.length then
      swing {
        diskette(driveIndex).sector = sector
        diskette(driveIndex).updateInfo()
      }
  override def onMotorChanged(driveIndex: Int, on: Boolean): Unit =
    if driveIndex < diskette.length then
      swing { diskette(driveIndex).icon.setIcon(if on then disketteRead else disketteOff) }
  override def onFloppyInserted(driveIndex: Int, image: DiskImage): Unit =
    swing {
      diskette(driveIndex).doubleSide = image.getHeadCount == 2
      diskette(driveIndex).icon.setIcon(disketteOff)
      diskette(driveIndex).head = 0
      diskette(driveIndex).track = 0
      diskette(driveIndex).sector = 0
      diskette(driveIndex).updateInfo()
      diskette(driveIndex).setToolTipText(image.diskName)
      if image.isWriteProtected then
        diskette(driveIndex).setBorder(BorderFactory.createLineBorder(Color.RED))
      else
        diskette(driveIndex).setBorder(BorderFactory.createEmptyBorder())
    }
  override def onFloppyEjected(driveIndex: Int): Unit =
    swing {
      diskette(driveIndex).icon.setIcon(disketteDisabled)
      diskette(driveIndex).head = 0
      diskette(driveIndex).track = 0
      diskette(driveIndex).sector = 0
      diskette(driveIndex).updateInfo()
      diskette(driveIndex).setToolTipText(null)
      diskette(driveIndex).setBorder(BorderFactory.createEmptyBorder())
    }
  override def onFloppyModeChanged(driveIndex: Int, writing: Boolean): Unit =
    swing { diskette(driveIndex).icon.setIcon(if writing then disketteWrite else disketteRead) }