package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*
import ucesoft.mac.cpu.m68k.M6800X0.BusAccessMode

object PEA:
  class PEA(override protected val ctx: M6800X0.Context,
            override protected val opcode:Int) extends Instruction(ctx,opcode):
    import RegisterType.*

    override val instructionType : InstructionType = InstructionType.PEA
    private final val abs = mode == 7 && (reg == 0 || reg == 1)
    private final val indirectIndex = mode == 6 || (mode == 7 && reg == 3)

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
            PEA       |      INSTR      |                  INSTR
    ------------------+-----------------+------------------------------------------
    <ea> :            |                 |
      .L :            |                 |
        (An)          | 12(1/2)         |                               np nS ns
        (d16,An)      | 16(2/2)         |                          np   np nS ns
        (d8,An,Xn)    | 20(2/2)         |                        n np n np nS ns
        (xxx).W       | 16(2/2)         |                               np nS ns np
        (xxx).L       | 20(3/2)         |                          np   np nS ns np
    */
    final override def execute(): Unit =
      val eaOp = ctx.getEA(mode,reg,Size.Long)
      val address = eaOp.getAddress
      val sp = ctx.getRegister(RegisterType.SP)
      sp.decrement(Size.Long)
      if indirectIndex then
        ctx.busIdle(2)
      if !abs then
        // =============== prefetch==================
          ctx.fetchWord(false)
        // ==========================================
      ctx.writeMemory(sp.get(Size.Long),address,Size.Long,BusAccessMode.Push)
      if abs then
        // =============== prefetch==================
          ctx.fetchWord(false)
        // ==========================================


    override def disassemble(address: Int): DisassembledInstruction =
      val eaOp = ctx.getEA(mode,reg,Size.Long,Some(address))
      DisassembledInstruction(address,opcode,instructionType.mnemonic,eaOp.getExtensionWords,Some(eaOp.getMnemonic(address)))

class PEA(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     PEA < ea >
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 1 | 0 | 0 | 1 | 0 | 0 | 0 | 0 | 1 |   Mode    |    Reg    |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     Size = (Long)

     Condition Codes:
      Not affected.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    val code = genOpcode("0100100001______")
    for(mode <- Seq(2,5,6,7))
      val regEnd = if mode == 7 then 3 else 7
      for(reg <- 0 to regEnd)
        val opcode = code | mode << 3 | reg
        instructionSetHandler.registerInstruction(opcode,new PEA.PEA(ctx,opcode))
