package ucesoft.mac.util

import java.io.IOException

/**
 * @author Alessandro Abbruzzetti
 *         Created on 25/01/2025 18:40  
 */
object EmptyDiskMaker:
  def main(args:Array[String]): Unit =
    if args.length < 2 then
      println("Usage: EmptyDiskMaker <file> <size in k or m: example 720k or 1m>")
      sys.exit(1)
    val file = args(0)
    val size = if args(1).endsWith("k") then
      args(1).substring(0,args(1).length - 1).toInt * 1024
    else if args(1).endsWith("m") then
      args(1).substring(0, args(1).length - 1).toInt * 1024 * 1024
    else
      println("Invalid size format. Use k or m")
      sys.exit(1)
      
    if makeDisk(file,size) then
      println(s"Empty disk $file created")
    else
      println(s"Error while creating empty disk $file")
      
  def makeDisk(file:String,sizeInKb:Int): Boolean =
    val buffer = Array.ofDim[Byte](1024)
    val block = sizeInKb / 1024
    val out = new java.io.FileOutputStream(file)
    try
      for i <- 0 until block do
        out.write(buffer)
      true
    catch
      case io:IOException =>
        println(s"Error while creating empty disk $file: $io")
        false
    finally
      out.close()
