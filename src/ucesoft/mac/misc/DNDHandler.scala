package ucesoft.mac.misc

import java.awt.datatransfer.*
import java.io.File
import javax.swing.TransferHandler

class DNDHandler(handleDND:(File,Boolean) => Unit) extends TransferHandler:
  override def canImport(support:TransferHandler.TransferSupport) : Boolean = support.isDataFlavorSupported(DataFlavor.javaFileListFlavor)
  override def importData(support: TransferHandler.TransferSupport) : Boolean =
    if !canImport(support) then
      return false

    val t = support.getTransferable
    
    try
      import scala.jdk.CollectionConverters.*
      t.getTransferData(DataFlavor.javaFileListFlavor).asInstanceOf[java.util.List[File]].asScala.headOption match
        case None =>
          false
        case Some(f) =>
          val copy = support.getDropAction match
            case TransferHandler.COPY =>
              true
            case TransferHandler.MOVE =>
              false
          val name = f.getName.toUpperCase
          handleDND(f,copy)
          true
    catch
      case t:Throwable =>
        t.printStackTrace()
        false