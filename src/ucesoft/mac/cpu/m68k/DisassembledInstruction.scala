package ucesoft.mac.cpu.m68k

case class DisassembledInstruction(address:Int,opcode:Int,mnemonic:String,extendedWords:List[Int] = Nil,op1:Option[String] = None,op2:Option[String] = None):
  final val size : Int = if opcode == -1 then 2 else 2 + (extendedWords.size << 1)
  final val isValidOpCode = opcode != -1

  override def toString: String =
    val numbers = if opcode == -1 then address :: extendedWords else address :: opcode :: extendedWords
    val addresses = "%-40s".format(numbers.zipWithIndex.map((a,i) => s"%0${if i == 0 then 8 else 4}x".format(a & 0xFFFFFF)).mkString(" "))
    var ops = if op1.isDefined then s" ${op1.get}" else ""
    if op2.isDefined then
      ops = s"$ops,${op2.get}"

    s"$addresses $mnemonic$ops"