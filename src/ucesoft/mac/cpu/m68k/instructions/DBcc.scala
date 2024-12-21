package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*
import ucesoft.mac.cpu.m68k.M6800X0.BusAccessMode

object DBcc:
  class DBcc(override protected val ctx: M6800X0.Context,
             override protected val opcode:Int) extends Bcc.Bcc(ctx,opcode):
    import InstructionType.*
    import RegisterType.*

    private val instructions = Array(
      DBT, DBRA, DBHI, DBLS, DBCC, DBCS, DBNE, DBEQ,
      DBVC, DBVS, DBPL, DBMI, DBGE, DBLT, DBGT, DBLE
    )
    override val instructionType : InstructionType = instructions(conditionCode)


    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
           DBcc       |      INSTR      |                   INSTR
    ------------------+-----------------+------------------------------------------
    Dn,<label> :      |                 |
      branch taken    | 10(2/0)         |                      n np       np
      branch not taken|                 |
        cc true       | 12(2/0)         |                      n     n np np
        counter exp   | 14(3/0)         |                      n np    np np
    */
    final override def execute(): Unit = {
      ctx.busIdle(2)
      if testCC then
        ctx.busIdle(2)
        ctx.fetchWord() // skip displacement
      else
        // read PC value before reading displacement (PC will be incremented)
        val pcValue = ctx.getPC
        val disp = extendSign(Size.Word,ctx.fetchWord())
        val dn = ctx.getRegister(RegisterType.Data,reg)
        val dnValue = dn.get(Size.Word,signExtended = true) - 1
        dn.set(dnValue,Size.Word)
        if dnValue != -1 then
          ctx.adjustCycles(-4) // TODO: Why this adjustment ??
          ctx.branch(pcValue + disp)
        else
          ctx.adjustCycles(4)

      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================
          
    }

    override def disassemble(address: Int): DisassembledInstruction = {
      val immOp = ctx.getEA(AddressingMode.IM, 0, Size.Word, Some(address))
      val disp = immOp.get(Size.Word, signExtended = true)
      val targetAddress = disp + 2 + address

      DisassembledInstruction(address, opcode, s"${instructionType.mnemonic}", List(disp), Some(s"d$reg"), Some(targetAddress.toHexString))
    }

class DBcc(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     1) DBcc Dn, < label > : dbt, dbra, dbhi, dbls, dbcc, dbcs, dbne, dbeq, dbvc, dbvs, dbpl, dbmi, dbge, dblt, dbgt, dble
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 1 | 0 | 1 |   Condition   | 1 | 1 | 0 | 0 | 1 |  Register |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     |                      16bit displacement                       |
     +---------------------------------------------------------------+


     Size = (Word)

    Mnemonic Condition          Encoding   Test
    =================================================================
    T*          True              0000    1
    F*          False             0001    0
    HI          High              0010    !(C or Z)
    LS          Low or Same       0011    C or Z
    CC(HI)      Carry Clear       0100    !C
    CS(LO)      Carry Set         0101    C
    NE          Not Equal         0110    !Z
    EQ          Equal             0111    Z
    VC          Overflow Clear    1000    !V
    VS          Overflow Set      1001    V
    PL          Plus              1010    !N
    MI          Minus             1011    N
    GE          Greater or Equal  1100    N and V or !N and !V
    LT          Less Than         1101    N and !V or !N and V
    GT          Greater Than      1110    N and V and !Z or !N and !V and !Z
    LE          Less or Equal     1111    Z or N and !V or !N and V

  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    val code = genOpcode("0101____11001___")
    for(condition <- 0 to 15)
      for(register <- 0 to 7)
        val opcode = code | condition << 8 | register
        instructionSetHandler.registerInstruction(opcode,new DBcc.DBcc(ctx,opcode))
