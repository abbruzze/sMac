package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object ORI2SR:
  class ORI2SR(override protected val ctx: M6800X0.Context,
               override protected val opcode:Int) extends Logical.Logical(ctx,opcode):

    override val instructionType : InstructionType = InstructionType.ORI2SR

    /*
      -------------------------------------------------------------------------------
       ORI, ANDI, EORI  |    Exec Time    |               Data Bus Usage
        to CCR, to SR   |      INSTR      | 1st Operand |          INSTR
      ------------------+-----------------+-------------+----------------------------
      #<data>,CCR       |                 |             |
        .B :            | 20(3/0)         |          np |              nn nn np np
    */
    final override def execute(): Unit = {
      val immOp = ctx.getEA(AddressingMode.IM, 0, Size.Word, None)
      val b = immOp.get(Size.Word) & 0xA71F // All implemented bits of the status register are affected.
      ctx.busIdle(8)
      val r = b | flags.get(Size.Word)
      flags.set(r,Size.Word)
      // =============== prefetch==================
      ctx.fetchWord(false, clearPrefetchQueue = true)
      // ==========================================
    }

    override def disassemble(address: Int): DisassembledInstruction = {
      val immOp = ctx.getEA(AddressingMode.IM, 0, Size.Word, Some(address))

      DisassembledInstruction(address, opcode, s"${instructionType.mnemonic}", immOp.getExtensionWords, Some(immOp.getMnemonic(address)), Some("SR"))
    }

class ORI2SR(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 1 | 1 | 1 | 1 | 1 | 0 | 0 |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     Size = Byte
  
     X — Set if bit 4 of immediate operand is one; unchanged otherwise.
     N — Set if bit 3 of immediate operand is one; unchanged otherwise.
     Z — Set if bit 2 of immediate operand is one; unchanged otherwise.
     V — Set if bit 1 of immediate operand is one; unchanged otherwise.
     C — Set if bit 0 of immediate operand is one; unchanged otherwise.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    val opcode = genOpcode("0000000001111100")
    instructionSetHandler.registerInstruction(opcode,new ORI2SR.ORI2SR(ctx,opcode))
