package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object SWAP:
  class SWAP(override protected val ctx: M6800X0.Context,
             override protected val opcode:Int) extends Instruction(ctx,opcode):
    import RegisterType.*

    override val instructionType : InstructionType = InstructionType.SWAP

    /*
    ------------------------------------------------------------------------------- 
                      |    Exec Time    |               Data Bus Usage              
           SWAP       |      INSTR      |                  INSTR                    
    ------------------+-----------------+------------------------------------------ 
    Dn :              |                 |                                           
      .W :            |  4(1/0)         |                               np          
    */
    final override def execute(): Unit =
      import ucesoft.mac.cpu.m68k.StatusRegister.StatusFlag.*
      val r = ctx.getRegister(RegisterType.Data,reg)
      val value = r.swapAndGet()
      var ccr = flags.getCCR
      if value < 0 then ccr |= N.flag else ccr &= ~N.flag
      if value == 0 then ccr |= Z.flag else ccr &= ~Z.flag
      ccr &= ~(V.flag | C.flag)
      flags.setCCR(ccr)
      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================

    override def disassemble(address: Int): DisassembledInstruction =
      DisassembledInstruction(address,opcode,instructionType.mnemonic,Nil,Some(s"d$reg"))

class SWAP(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     SWAP Dn
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 1 | 0 | 0 | 1 | 0 | 0 | 0 | 0 | 1 | 0 | 0 | 0 |    Reg    |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     Size = (Word)

     Condition codes:
      X — Not affected.
      N — Set if the most significant bit of the 32-bit result is set; cleared otherwise.
      Z — Set if the 32-bit result is zero; cleared otherwise.
      V — Always cleared.
      C — Always cleared.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    val code = genOpcode("0100100001000___")
    for(reg <- 0 to 7)
      val opcode = code | reg
      instructionSetHandler.registerInstruction(opcode,new SWAP.SWAP(ctx,opcode))
