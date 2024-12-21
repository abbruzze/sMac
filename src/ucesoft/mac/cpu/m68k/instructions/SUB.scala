package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object SUB:
  class SUB(override protected val ctx: M6800X0.Context,
            override protected val opcode:Int,
            size: Size,
            destinationDn: Boolean) extends Instruction(ctx,opcode):
    import RegisterType.*

    override val instructionType : InstructionType = InstructionType.SUB
    protected final val register = (opcode >> 9) & 7

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
         ADD, SUB     |  INSTR     EA   | 1st Operand |  2nd OP (ea)  |   INSTR
    ------------------+-----------------+-------------+---------------+------------
    <ea>,Dn :         |                 |              \              |
      .B or .W :      |                 |               |             |
        Dn            |  4(1/0)  0(0/0) |               |             | np
        An            |  4(1/0)  0(0/0) |               |             | np
        (An)          |  4(1/0)  4(1/0) |            nr |             | np
        (An)+         |  4(1/0)  4(1/0) |            nr |             | np
        -(An)         |  4(1/0)  6(1/0) | n          nr |             | np
        (d16,An)      |  4(1/0)  8(2/0) |      np    nr |             | np
        (d8,An,Xn)    |  4(1/0) 10(2/0) | n    np    nr |             | np
        (xxx).W       |  4(1/0)  8(2/0) |      np    nr |             | np
        (xxx).L       |  4(1/0) 12(3/0) |   np np    nr |             | np
        #<data>       |  4(1/0)  4(1/0)        np                     | np
      .L :            |                 |               |             |
        Dn            |  8(1/0)  0(0/0) |               |             | np       nn
        An            |  8(1/0)  0(0/0) |               |             | np       nn
        (An)          |  6(1/0)  8(2/0) |         nR nr |             | np       n
        (An)+         |  6(1/0)  8(2/0) |         nR nr |             | np       n
        -(An)         |  6(1/0) 10(2/0) | n       nR nr |             | np       n
        (d16,An)      |  6(1/0) 12(3/0) |      np nR nr |             | np       n
        (d8,An,Xn)    |  6(1/0) 14(3/0) | n    np nR nr |             | np       n
        (xxx).W       |  6(1/0) 12(3/0) |      np nR nr |             | np       n
        (xxx).L       |  6(1/0) 16(4/0) |   np np nR nr |             | np       n
        #<data>       |  8(1/0)  8(2/0) |   np np       |               np       nn
    Dn,<ea> :         |                 |              /              |
      .B or .W :      |                 |             |               |
        (An)          |  8(1/1)  4(1/0) |             |            nr | np nw
        (An)+         |  8(1/1)  4(1/0) |             |            nr | np nw
        -(An)         |  8(1/1)  6(1/0) |             | n          nr | np nw
        (d16,An)      |  8(1/1)  8(2/0) |             |      np    nr | np nw
        (d8,An,Xn)    |  8(1/1) 10(2/0) |             | n    np    nr | np nw
        (xxx).W       |  8(1/1)  8(2/0) |             |      np    nr | np nw
        (xxx).L       |  8(1/1) 12(3/0) |             |   np np    nr | np nw
      .L :            |                 |             |               |
        (An)          | 12(1/2)  8(2/0) |             |            nr | np nw nW
        (An)+         | 12(1/2)  8(2/0) |             |            nr | np nw nW
        -(An)         | 12(1/2) 10(2/0) |             | n       nR nr | np nw nW
        (d16,An)      | 12(1/2) 12(3/0) |             |      np nR nr | np nw nW
        (d8,An,Xn)    | 12(1/2) 14(3/0) |             | n    np nR nr | np nw nW
        (xxx).W       | 12(1/2) 12(3/0) |             |      np nR nr | np nw nW
        (xxx).L       | 12(1/2) 16(4/0) |             |   np np nR nr | np nw nW
    */
    override def execute(): Unit =
      val eaOp = ctx.getEA(mode, reg, size)
      val dnReg = ctx.getRegister(Data, register)

      val ea = eaOp.get(size, signExtended = true)
      val dn = dnReg.get(size, signExtended = true)
      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================
      if destinationDn then
        val r = dn - ea
        dnReg.set(r, size)
        setFlags(ea, dn, r, size)
      else
        val r = ea - dn
        eaOp.set(r, size)
        setFlags(dn, ea, r, size)

      if size == Size.Long && destinationDn then
        if eaOp.mode == AddressingMode.DN || eaOp.mode == AddressingMode.AN || eaOp.mode == AddressingMode.IM then
          ctx.busIdle(4)
        else
          ctx.busIdle(2)

    protected def setFlags(s: Int, d: Int, r: Int, size: Size): Unit = {
      import StatusRegister.StatusFlag.*
      var ccr = flags.getCCR
      val sm = (s & size.msb) != 0
      val dm = (d & size.msb) != 0
      val rm = (r & size.msb) != 0
      val rz = (r & size.mask) == 0

      if (!sm && dm && !rm) || (sm && !dm && rm) then ccr |= V.flag else ccr &= ~V.flag
      if (sm && !dm) || (rm && !dm) || (sm && rm) then ccr |= C.flag | X.flag else ccr &= ~(C.flag | X.flag)
      if rz then ccr |= Z.flag else ccr &= ~Z.flag
      if rm then ccr |= N.flag else ccr &= ~N.flag

      flags.setCCR(ccr)
    }

    override def disassemble(address: Int): DisassembledInstruction =
      val eaOp = ctx.getEA(mode, reg, size, Some(address))
      val dn = ctx.getRegister(Data, register)

      if destinationDn then
        DisassembledInstruction(address, opcode, s"${instructionType.mnemonic}${size.ext}", eaOp.getExtensionWords, Some(eaOp.getMnemonic(address)), Some(dn.mnemonic))
      else
        DisassembledInstruction(address, opcode, s"${instructionType.mnemonic}${size.ext}", eaOp.getExtensionWords, Some(dn.mnemonic), Some(eaOp.getMnemonic(address)))

class SUB(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     SUB < ea > ,Dn
     SUB Dn, < ea >
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 1 | 0 | 0 | 1 |  Register |  Opmode   |   Mode    |    Reg    |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

    Size = (Byte, Word, Long)
    Opmode:
     Byte Word Long         Operation
     000  001  010          Dn – <ea> -> Dn
     100  101  110          <ea> - Dn -> <ea>

    Condition codes:
      X — Set to the value of the carry bit.
      N — Set if the result is negative; cleared otherwise.
      Z — Set if the result is zero; cleared otherwise.
      V — Set if an overflow is generated; cleared otherwise.
      C — Set if a borrow is generated; cleared otherwise.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    import Size.*
    val code = genOpcode("1001____________")
    for (size <- Seq(Byte, Word, Long))
      for (opmode <- 0 to 1)  // opmode = 0 Dn – <ea> -> Dn, opmode = 1 <ea> - Dn -> <ea>
        for (register <- 0 to 7)
          val modeStart = if opmode == 0 then 0 else 2
          for (mode <- modeStart to 7)
            if !(opmode == 0 && mode == 1 && size == Byte) then
              val regEnd = if opmode == 0 then
                if mode == 7 then 4 else 7
              else if mode == 7 then 1 else 7
              for (reg <- 0 to regEnd)
                val opcode = code | register << 9 | (opmode << 2 | size.ordinal) << 6 | mode << 3 | reg
                instructionSetHandler.registerInstruction(opcode, new SUB.SUB(ctx, opcode, size, opmode == 0))
