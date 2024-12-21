package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.{AddressingMode, DisassembledInstruction, Instruction, InstructionGenerator, InstructionSet, InstructionType, M6800X0, Operand, RegisterType, Size, StatusRegister}

object ABCD:
  class ABCD(override protected val ctx: M6800X0.Context,
             override protected val opcode:Int,
             protected val rtype:RegisterType) extends Instruction(ctx,opcode):
    import RegisterType.*

    override val instructionType : InstructionType = InstructionType.ABCD
    protected final val register = (opcode >> 9) & 7

    protected def bcd_op(s:Int, d:Int): Int =
      import StatusRegister.StatusFlag.*

      var x = flags.getFlag(X)
      var z = flags.getFlag(Z)
      var c = 0
      val ss = (s + d + x) & 0xFF
      val bc = ((s & d) | (~ss & s) | (~ss & d)) & 0x88
      val dc = (((ss + 0x66) ^ ss) & 0x110) >> 1
      val corf = (bc | dc) - ((bc | dc) >> 2)
      val rr = (ss + corf) & 0xFF
      x = (bc | (ss & ~rr)) >> 7
      c = x
      val v = (~ss & rr) >> 7
      z = z & (if (rr == 0) 1 else 0)
      val n = rr >> 7
      val ccr = ((x & 1) << X.ordinal) | ((n & 1) << N.ordinal) | ((z & 1) << Z.ordinal) | ((v & 1) << V.ordinal) | ((c & 1) << C.ordinal)
      flags.setCCR(ccr)
      rr
    end bcd_op

    /*
      -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
        ABCD, SBCD    |      INSTR      |                  INSTR
      ------------------+-----------------+------------------------------------------
      Dy,Dx :           |                 |
        .B :            |  6(1/0)         |                               np       n
      -(Ay),-(Ax) :     |                 |
        .B :            | 18(3/1)         |                 n    nr    nr np nw
    */
    override def execute(): Unit =
      val regSrc = reg
      val regDst = register

      (rtype : @unchecked) match
        case Data =>
          val destinationReg = ctx.getRegister(rtype,regDst)
          val result = bcd_op(ctx.getRegister(rtype,regSrc).get(Size.Byte),destinationReg.get(Size.Byte))
          destinationReg.set(result,Size.Byte)
          // =============== prefetch==================
          ctx.fetchWord(false)
          // ==========================================
          ctx.busIdle(2)
        case Address =>
          ctx.busIdle(2)
          // predecrement addressing mode for source and destination
          val sourceReg = ctx.getRegister(rtype,regSrc)
          val destinationReg = ctx.getRegister(rtype,regDst)
          sourceReg.decrement(Size.Byte)
          val s = ctx.readMemory(sourceReg.get(Size.Long),Size.Byte)
          destinationReg.decrement(Size.Byte)
          val destinationAddress = destinationReg.get(Size.Long)
          val d = ctx.readMemory(destinationAddress,Size.Byte)
          val result = bcd_op(s,d)
          // =============== prefetch==================
          ctx.fetchWord(false)
          // ==========================================
          ctx.writeMemory(destinationAddress,result,Size.Byte)

    override def disassemble(address: Int): DisassembledInstruction =
      val regSrc = reg
      val regDst = register
      val (s,d) = (rtype : @unchecked) match
        case Data =>
          (s"d$regSrc",s"d$regDst")
        case Address =>
          (s"-(a$regSrc)",s"-(a$regDst)")

      DisassembledInstruction(address,opcode,instructionType.mnemonic,Nil,Some(s),Some(d))

class ABCD(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 1 | 1 | 0 | 0 |  Reg dst  | 1 | 0 | 0 | 0 | 0 |R/M| Reg src   |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     Size = Byte
     R/M:
      - If R/M = 0, specifies a data register
      - If R/M = 1, specifies an address register for the predecrement addressing mode

     X — Set the same as the carry bit.
     N — Undefined.
     Z — Cleared if the result is nonzero; unchanged otherwise.
     V — Undefined.
     C — Set if a decimal carry was generated; cleared otherwise.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    import RegisterType.*
    val code = genOpcode("1100___10000____")
    for(rm <- Seq(Data,Address)) {
      for(regSrc <- 0 to 7) {
        for(regDst <- 0 to 7) {
          val opcode = code | regDst << 9 | rm.ordinal << 3| regSrc
          instructionSetHandler.registerInstruction(opcode,new ABCD.ABCD(ctx,opcode,rm))
        }
      }
    }
