package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

import scala.collection.mutable.ListBuffer

object MOVEM:
  class MOVEM(override protected val ctx: M6800X0.Context,
              override protected val opcode:Int) extends Instruction(ctx,opcode):
    import RegisterType.*

    override val instructionType : InstructionType = InstructionType.MOVEM
    private final val size = if (opcode & 0x40) == 0 then Size.Word else Size.Long
    private final val R2M = (opcode & 0x400) == 0
    private final val preDecrementMode = mode == 4
    private final val postIncrementMode = mode == 3

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
          MOVEM       |      INSTR      |                  INSTR
    ------------------+-----------------+------------------------------------------
    M --> R           |                 |
      .W              |                 |
        (An)          | 12+4m(3+m/0)    |                np (nr)*    nr np
        (An)+         | 12+4m(3+m/0)    |                np (nr)*    nr np
        (d16,An)      | 16+4m(4+m/0)    |             np np (nr)*    nr np
        (d8,An,Xn)    | 18+4m(4+m/0)    |          np n  np (nr)*    nr np
        (xxx).W       | 16+4m(4+m/0)    |             np np (nr)*    nr np
        (xxx).L       | 20+4m(5+m/0)    |          np np np (nr)*    nr np
      .L              |                 |
        (An)          | 12+8m(3+2m/0)   |                np nR (nr nR)* np
        (An)+         | 12+8m(3+2m/0)   |                np nR (nr nR)* np
        (d16,An)      | 16+8m(4+2m/0)   |             np np nR (nr nR)* np
        (d8,An,Xn)    | 18+8m(4+2m/0)   |          np n  np nR (nr nR)* np
        (xxx).W       | 16+8m(4+2m/0)   |             np np nR (nr nR)* np
        (xxx).L       | 20+8m(5+2m/0)   |          np np np nR (nr nR)* np
    R --> M           |                 |
      .W              |                 |
        (An)          |  8+4m(2/m)      |                np (nw)*       np
        -(An)         |  8+4m(2/m)      |                np (nw)*       np
        (d16,An)      | 12+4m(3/m)      |             np np (nw)*       np
        (d8,An,Xn)    | 14+4m(3/m)      |          np n  np (nw)*       np
        (xxx).W       | 12+4m(3/m)      |             np np (nw)*       np
        (xxx).L       | 16+4m(4/m)      |          np np np (nw)*       np
      .L              |                 |
        (An)          |  8+8m(2/2m)     |                np (nW nw)*    np
        -(An)         |  8+8m(2/2m)     |                np (nw nW)*    np
        (d16,An)      | 12+8m(3/2m)     |             np np (nW nw)*    np
        (d8,An,Xn)    | 14+8m(3/2m)     |          np n  np (nW nw)*    np
        (xxx).W       | 12+8m(3/2m)     |             np np (nW nw)*    np
        (xxx).L       | 16+8m(4/2m)     |          np np np (nW nw)*    np
    NOTES :
      .'m' is the number of registers to move.
      .'(nr)*' should be replaced by m consecutive 'nr'
      .'(nw)*' should be replaced by m consecutive 'nw'
      .'(nR nr)*' should be replaced by m consecutive 'nR nr'
      .'(nW nw)*' (or '(nw nW)*') should be replaced by m consecutive 'nW nw' (or m
       consecutive 'nw nW').
      .In M --> R mode, an extra bus read cycle occurs. This extra access can cause
       errors (for exemple if occuring beyond the bounds of physical memory).
      .In M --> R mode, MOVEM.W sign-extends words moved to data registers.
    */
    final override def execute(): Unit =
      val listOp = ctx.getEA(AddressingMode.IM, 0, Size.Word, None)
      val regList = listOp.get(Size.Word)
      var addressPtr = if preDecrementMode then ctx.getRegister(RegisterType.Address,reg).get(Size.Long) else ctx.getEA(mode,reg,size,None).getAddress

      if !R2M && size == Size.Long then // In M --> R mode, an extra bus read cycle occurs.
        ctx.readMemory(addressPtr, Size.Word)

      val addressIncrement = if preDecrementMode then -size.bytes else size.bytes

      if preDecrementMode then
        addressPtr += addressIncrement

      var bit = 1
      var ri = 0
      var regType = if preDecrementMode then RegisterType.Address else RegisterType.Data

      while bit < 0x10000 do
        if (regList & bit) != 0 then
          val rx = if preDecrementMode then 7 - (ri & 7) else ri & 7
          if R2M then
            ctx.writeMemory(addressPtr,ctx.getRegister(regType,rx).get(size),size)
          else
            val read = if size == Size.Word then extendSign(Size.Word,ctx.readMemory(addressPtr,Size.Word)) else ctx.readMemory(addressPtr,Size.Long)
            ctx.getRegister(regType,rx).set(read,Size.Long)

          addressPtr += addressIncrement
        end if
        bit <<= 1
        ri += 1
        if ri == 8 then
          regType = if preDecrementMode then RegisterType.Data else RegisterType.Address
      end while

      if !R2M && size == Size.Word then // In M --> R mode, an extra bus read cycle occurs.
        ctx.readMemory(addressPtr,Size.Word)

      if preDecrementMode  then
        ctx.getRegister(RegisterType.Address,reg).set(addressPtr + size.bytes,Size.Long)
      else if postIncrementMode then
        ctx.getRegister(RegisterType.Address,reg).set(addressPtr,Size.Long)

      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================

    override def disassemble(address: Int): DisassembledInstruction =
      val listOp = ctx.getEA(AddressingMode.IM, 0, Size.Word, Some(address))
      val regList = listOp.get(Size.Word)
      val regListStr = regListToString(regList,preDecrementMode)
      val eaOp = ctx.getEA(mode,reg,size,Some(address + 2))
      if R2M then
        DisassembledInstruction(address,opcode,s"${instructionType.mnemonic}${size.ext}",listOp.getExtensionWords ++ eaOp.getExtensionWords,Some(regListStr),Some(eaOp.getMnemonic(address)))
      else
        DisassembledInstruction(address,opcode,s"${instructionType.mnemonic}${size.ext}",listOp.getExtensionWords ++ eaOp.getExtensionWords,Some(eaOp.getMnemonic(address)),Some(regListStr))

    protected def regListToString(_list:Int,predecrementMode:Boolean): String =
      val list = _list & 0xFFFF
      val groups = new ListBuffer[(Int,Int,Char)]
      val group = Array(-1,-1)
      var bit = 1
      for(ri <- 0 to 16)
        if (list & bit) != 0 then
          if group(0) == -1 then
            group(0) = ri & 7
          else
            group(1) = ri & 7
        if ri == 7 || ri == 15 || (list & bit) == 0 then
          if group(0) != -1 then
            if predecrementMode then
              if group(1) == -1 then
                groups += ((7 - group(0),-1,if ri < 8 then 'a' else 'd'))
              else
                groups += ((7 - group(1),7 - group(0),if ri < 8 then 'a' else 'd'))
            else
              groups += ((group(0),group(1),if ri < 8 then 'd' else 'a'))
          group(0) = -1
          group(1) = -1
        bit <<= 1
      groups.map(g => if g._2 != -1 then s"${g._3}${g._1}-${g._3}${g._2}" else s"${g._3}${g._1}").mkString("/")

class MOVEM(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     MOVEM < list > , < ea >
     MOVEM < ea > , < list >
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 1 | 0 | 0 | 1 | dr| 0 | 0 | 1 |sz |   mode    |   reg     |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     |               register list mask                              |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     Size = (Word, Long)

     dr field—Specifies the direction of the transfer.
      0 — Register to memory.
      1 — Memory to register.

    Size field—Specifies the size of the registers being transferred.
      0 — Word transfer
      1 — Long transfer


     Condition codes:
      Not affected.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    import Size.*
    val code = genOpcode("01001_001_______")
    for(dr <- 0 to 1)
      for(size <- 0 to 1)
        val modeList = if dr == 0 then Seq(2,4,5,6,7) else Seq(2,3,5,6,7)
        for(mode <- modeList)
          val regEnd = if mode == 7 then
            if dr == 0 then 1 else 3
          else 7
          for(reg <- 0 to regEnd)
            val opcode = code | dr << 10 | size << 6 | mode << 3 | reg
            instructionSetHandler.registerInstruction(opcode,new MOVEM.MOVEM(ctx,opcode))
