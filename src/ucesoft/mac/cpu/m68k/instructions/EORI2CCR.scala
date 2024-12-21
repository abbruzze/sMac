package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object EORI2CCR:
  class EORI2CCR(override protected val ctx: M6800X0.Context,
                 override protected val opcode:Int) extends Logical.Logical(ctx,opcode):

    override val instructionType : InstructionType = InstructionType.EORI2CCR
    
    /*
    -------------------------------------------------------------------------------
     ORI, ANDI, EORI  |    Exec Time    |               Data Bus Usage
      to CCR, to SR   |      INSTR      | 1st Operand |          INSTR
    ------------------+-----------------+-------------+----------------------------
    #<data>,CCR       |                 |             |
      .B :            | 20(3/0)         |          np |              nn nn np np

    */
    final override def execute(): Unit = {
      val immOp = ctx.getEA(AddressingMode.IM, 0, Size.Byte, None)
      val b = immOp.get(Size.Byte) & 0x1F // All implemented bits of the condition code register are affected
      ctx.busIdle(8)
      val r = b ^ flags.getCCR
      flags.setCCR(r)
      // =============== prefetch==================
      ctx.fetchWord(false,clearPrefetchQueue = true)
      // ==========================================
    }

    override def disassemble(address: Int): DisassembledInstruction = {
      val immOp = ctx.getEA(AddressingMode.IM, 0, Size.Byte, Some(address))

      DisassembledInstruction(address, opcode, s"${instructionType.mnemonic}", immOp.getExtensionWords, Some(immOp.getMnemonic(address)), Some("CCR"))
    }

class EORI2CCR(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     EORI # < data > ,CCR
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 0 | 0 | 0 | 1 | 0 | 1 | 0 | 0 | 0 | 1 | 1 | 1 | 1 | 0 | 0 |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     Size = Byte
  
      X — Changed if bit 4 of immediate operand is one; unchanged otherwise.
      N — Changed if bit 3 of immediate operand is one; unchanged otherwise.
      Z — Changed if bit 2 of immediate operand is one; unchanged otherwise.
      V — Changed if bit 1 of immediate operand is one; unchanged otherwise.
      C — Changed if bit 0 of immediate operand is one; unchanged otherwise.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    val opcode = genOpcode("0000101000111100")
    instructionSetHandler.registerInstruction(opcode,new EORI2CCR.EORI2CCR(ctx,opcode))
