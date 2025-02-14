package ucesoft.mac.keyboard

import java.awt.event.KeyEvent
import scala.collection.mutable.ListBuffer
import java.io.InputStream

/**
 * @author Alessandro Abbruzzetti
 *         Created on 14/02/2025 10:16  
 */
object KeyboardConfig:
  private val JAVA_KEY_MAP = {
    val clazz = classOf[KeyEvent]
    val fields = clazz.getDeclaredFields
    fields.filter(_.getName.startsWith("VK_")).map(f => (f.getName,f.get(null).asInstanceOf[Int])).toMap
  }
  case class Config(name:String,codes:List[List[Int]])

  def readConfig(in:InputStream): Either[String,List[Config]] =
    val src = io.Source.fromInputStream(in)
    try
      val conf = src.getLines()

      val configs = new ListBuffer[Config]
      var searchingKeyb = true
      val keybConf = new ListBuffer[List[Int]]
      var keybName = ""
      var lineNumber = 0
      while conf.hasNext do
        val line = conf.next()
        lineNumber += 1
        val tline = line.replaceAll("\t"," ").trim
        if !tline.startsWith("#") && tline.nonEmpty then
          if searchingKeyb then
            if tline.endsWith("{") then
              keybName = tline.substring(0,tline.length - 1).trim
              searchingKeyb = false
              keybConf.clear()
            else
              return Left(s"Expected start of keyboard definition on line $lineNumber: $line")
          else if tline == "}" then
            configs += Config(keybName,keybConf.toList)
            searchingKeyb = true
          else
            val codes = tline.split("""[\s\t]+""")
            val keyCodes = codes.map { c =>
              if c.startsWith("0x") then
                Integer.parseInt(c.substring(2),16)
              else if c.startsWith("VK_") then
                JAVA_KEY_MAP.get(c) match
                  case Some(code) => code
                  case None =>
                    -1
                else
                  Integer.parseInt(c)
            }.toList
            if keyCodes.contains(-1) then
              return Left(s"Invalid code on line $lineNumber: $line")

            keybConf += keyCodes
      end while

      if searchingKeyb then
        Right(configs.toList)
      else
        Left(s"End of file reached: definition not closed")
    finally
      src.close()



