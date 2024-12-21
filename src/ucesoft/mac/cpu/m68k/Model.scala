package ucesoft.mac.cpu.m68k

enum Model(val addressBusBits:Int):
  val addressBUSMask: Int = ((1L << addressBusBits) - 1).toInt
  case _68K extends Model(24)