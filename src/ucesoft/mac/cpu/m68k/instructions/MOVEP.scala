package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.*

object MOVEP:
  class MOVEP(override protected val ctx: M6800X0.Context,
              override protected val opcode:Int) extends Instruction(ctx,opcode):
    import RegisterType.*

    override val instructionType : InstructionType = InstructionType.MOVEP
    private final val dataReg = (opcode >> 9) & 7
    private final val opmode = (opcode >> 6) & 7

    /*
    -------------------------------------------------------------------------------
                      |    Exec Time    |               Data Bus Usage
          MOVEP       |      INSTR      |                  INSTR
    ------------------+-----------------+------------------------------------------
    Dx,(d16,Ay) :     |                 |
      .W :            | 16(2/2)         |                np    nW    nw np
      .L :            | 24(2/4)         |                np nW nW nw nw np
    (d16,Ay),Dx :     |                 |
      .W :            | 16(4/0)         |                np    nR    nr np
      .L :            | 24(6/0)         |                np nR nR nr nr np
    NOTES :
      .Read and write operations are done from the MSB to the LSB on 2 words if
      using ".w" (first read/write word at Ay+d16 then word at Ay+d16+2)and on
      4 words if using ".l" (first read/write word at Ay+d16 then word at Ay+d16+2
      then word at Ay+d16+4 and finally word at Ay+d16+6).
    */
    final override def execute(): Unit =
      val immOp = ctx.getEA(AddressingMode.IM, 0, Size.Word, None)
      val disp = immOp.get(Size.Word,signExtended = true)
      val addrReg = ctx.getRegister(RegisterType.Address,reg)
      val dReg = ctx.getRegister(RegisterType.Data,dataReg)
      val address = addrReg.get(Size.Long) + disp

      opmode match
        case 4 /* Word M -> R */ =>
          val w = ctx.readMemory(address,Size.Byte) << 8 | ctx.readMemory(address + 2,Size.Byte)
          dReg.set(w,Size.Word)
        case 5 /* Long M -> R */ =>
          val l = ctx.readMemory(address, Size.Byte) << 24 | ctx.readMemory(address + 2, Size.Byte) << 16 | ctx.readMemory(address + 4, Size.Byte) << 8 | ctx.readMemory(address + 6, Size.Byte)
          dReg.set(l, Size.Long)
        case 6 /* Word R -> M */ =>
          val w = dReg.get(Size.Word)
          ctx.writeMemory(address,(w >> 8) & 0xFF,Size.Byte)
          ctx.writeMemory(address + 2,w & 0xFF,Size.Byte)
        case 7 /* Long R -> M */ =>
          val l = dReg.get(Size.Long)
          ctx.writeMemory(address, (l >> 24) & 0xFF, Size.Byte)
          ctx.writeMemory(address + 2, (l >> 16) & 0xFF, Size.Byte)
          ctx.writeMemory(address + 4, (l >> 8) & 0xFF, Size.Byte)
          ctx.writeMemory(address + 6, l & 0xFF, Size.Byte)

      // =============== prefetch==================
      ctx.fetchWord(false)
      // ==========================================

    override def disassemble(address: Int): DisassembledInstruction =
      val immOp = ctx.getEA(AddressingMode.IM, 0, Size.Word, Some(address))
      val disp = immOp.get(Size.Word)
      opmode match
        case 4 | 5 => // M -> R
          val size = if opmode == 4 then Size.Word else Size.Long
          DisassembledInstruction(address,opcode,s"${instructionType.mnemonic}${size.ext}",immOp.getExtensionWords,Some(s"(${disp.toHexString},a$reg)"),Some(s"d$dataReg"))
        case 6 | 7 => // R -> M
          val size = if opmode == 6 then Size.Word else Size.Long
          DisassembledInstruction(address,opcode,s"${instructionType.mnemonic}${size.ext}",immOp.getExtensionWords,Some(s"d$dataReg"),Some(s"(${disp.toHexString},a$reg)"))

class MOVEP(ctx: M6800X0.Context) extends InstructionGenerator:
  /*
     MOVEP Dx,(d16,Ay)
     MOVEP (d16,Ay),Dx
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | 0 | 0 | 0 | 0 | Data Reg  |   Opmode  | 0 | 0 | 1 | Addr Reg  |
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

     Size = (Word, Long)

     Opmode field—Specifies the direction and size of the operation.
      100 — Transfer word from memory to register.
      101 — Transfer long from memory to register.
      110 — Transfer word from register to memory.
      111 — Transfer long from register to memory.

     Condition Codes:
     Not affected.
  */
  override def generate(instructionSetHandler: InstructionSet): Unit =
    val code = genOpcode("0000______001___")
    for(dataReg <- 0 to 7)
      for(addressReg <- 0 to 7)
        for(opmode <- Seq(4,5,6,7))
          val opcode = code | dataReg << 9 | opmode << 6 | addressReg
          instructionSetHandler.registerInstruction(opcode,new MOVEP.MOVEP(ctx,opcode))
