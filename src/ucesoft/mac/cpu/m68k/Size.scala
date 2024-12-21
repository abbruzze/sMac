package ucesoft.mac.cpu.m68k

enum Size(val bytes:Int,val ext:String):
  val msb : Int = (1L << ((bytes << 3) - 1)).toInt
  val mask : Int = ((1L << (bytes << 3)) - 1).toInt

  case Byte extends Size(1,".b")
  case Word extends Size(2,".w")
  case Long extends Size(4,".l")
  //case NoSize extends Size(0,"")
