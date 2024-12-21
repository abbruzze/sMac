package ucesoft.mac.cpu.m68k

import java.io.{BufferedInputStream, InputStream}
import scala.collection.mutable

trait Memory:
  def read(address:Int, size: Size, readOptions:Int = 0): Int
  def write(address:Int, value:Int, size: Size, writeOptions:Int = 0): Unit

object Memory:
  object DummyMemory extends Memory:
    override def read(address: Int, size: Size, readOptions: Int = 0): Int = 0
    override def write(address: Int, value: Int, size: Size, writeOptions: Int = 0): Unit = {}

  class ByteBufferMemory(val size:Int) extends Memory:
    import java.nio.ByteBuffer
    private final val buffer = ByteBuffer.allocateDirect(size)

    override def read(address: Int, size: Size, readOptions:Int): Int =
      size match
        case Size.Byte =>
          buffer.get(address) & 0x000000FF
        case Size.Word =>
          buffer.getShort(address) & 0x0000FFFF
        case Size.Long =>
          buffer.getInt(address)

    override def write(address: Int, value: Int, size: Size, writeOptions:Int): Unit =
      size match
        case Size.Byte =>
          buffer.put(address,(value & 0x000000FF).asInstanceOf[Byte])
        case Size.Word =>
          buffer.putShort(address,(value & 0x0000FFFF).asInstanceOf[Short])
        case Size.Long =>
          buffer.putInt(address,value)

    def fill(offset:Int,in:InputStream): Unit =
      val bin = new BufferedInputStream(in)
      var read = bin.read()
      var m = offset
      while read != -1 do
        buffer.put(m,read.asInstanceOf[Byte])
        read = bin.read()
        m += 1

    def clear(): Unit = buffer.clear()
  end ByteBufferMemory

  class MapMemory extends Memory:
    private final val map = new mutable.HashMap[Int,Int]()

    def clear(): Unit = map.clear()

    override def read(address: Int, size: Size, readOptions:Int): Int =
      size match
        case Size.Byte =>
          map.getOrElse(address,0) & 0x000000FF
        case Size.Word =>
          (map.getOrElse(address,0) << 8 | map.getOrElse(address + 1,0)) & 0x0000FFFF
        case Size.Long =>
          map.getOrElse(address,0) << 24 | map.getOrElse(address + 1,0) << 16 | map.getOrElse(address + 2,0) << 8 | map.getOrElse(address + 3,0)

    override def write(address: Int, value: Int, size: Size, writeOptions:Int): Unit =
      size match
        case Size.Byte =>
          map.put(address, value & 0xFF)
        case Size.Word =>
          map.put(address, (value >> 8) & 0xFF)
          map.put(address + 1, value & 0xFF)
        case Size.Long =>
          map.put(address, (value >> 24) & 0xFF)
          map.put(address + 1, (value >> 16) & 0xFF)
          map.put(address + 2, (value >> 8) & 0xFF)
          map.put(address + 3, value & 0xFF)

      //println(s"Writing memory at ${address.toHexString} = $value size=$size")
      //println(s"Memory=$map")
  end MapMemory
  