package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object ANDI2SR:
  class ANDI2SR(override protected val ctx: M6800X0.Context,
                override protected val opcode:Int) extends Logical.Logical(ctx,opcode):

    override val instructionType : InstructionType = InstructionType.ANDI2SR

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
      val b = immOp.get(Size.Word)
      ctx.busIdle(8)
      val r = b & flags.get(Size.Word)
      flags.set(r,Size.Word)
      // =============== prefetch==================
      ctx.fetchWord(false, clearPrefetchQueue = true)
      // ==========================================
    }

    override def disassemble(address: Int): DisassembledInstruction = {
      val immOp = ctx.getEA(AddressingMode.IM, 0, Size.Word, Some(address))

      DisassembledInstruction(address, opcode, s"${instructionType.mnemonic}", immOp.getExtensionWords, Some(immOp.getMnemonic(address)), Some("SR"))
    }

class ANDI2SR(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 0 | 0 | 0 | 0 | 0 | 1 | 0 | 0 | 1 | 1 | 1 | 1 | 1 | 0 | 0 |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     Size = Byte
  
     X — Cleared if bit 4 of immediate operand is zero; unchanged otherwise.
     N — Cleared if bit 3 of immediate operand is zero; unchanged otherwise.
     Z — Cleared if bit 2 of immediate operand is zero; unchanged otherwise.
     V — Cleared if bit 1 of immediate operand is zero; unchanged otherwise.
     C — Cleared if bit 0 of immediate operand is zero; unchanged otherwise.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    val opcode = genOpcode("0000001001111100")
    instructionSetHandler.registerInstruction(opcode,new ANDI2SR.ANDI2SR(ctx,opcode))
