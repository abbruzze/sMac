package ucesoft.mac

import java.io.IOException
import java.nio.file.Files
import java.security.MessageDigest

/**
 * @author Alessandro Abbruzzetti
 *         Created on 29/11/2024 19:12  
 */
case class ROM(file:String,rom:Array[Byte],hash:String,model:MacModel)

object ROM:
  def loadROM(file:String): Either[String,ROM] =
    try
      val rom = Files.readAllBytes(new java.io.File(file).toPath)
      val md = MessageDigest.getInstance("MD5")
      md.update(rom)
      val hash = md.digest().map("%02X".format(_)).mkString
      MacModel.values.find(_.md5.contains(hash)) match
        case Some(model) =>
          Right(ROM(file,rom,hash,model))
        case None =>
          Left(s"ROM's md5 '$hash' not recognized")
    catch
      case err:IOException =>
        Left(err.toString)
