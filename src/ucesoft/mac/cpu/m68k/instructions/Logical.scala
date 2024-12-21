package ucesoft.mac.cpu.m68k.instructions

import ucesoft.mac.cpu.m68k.{Instruction, M6800X0, Operand, Size, StatusRegister}

object Logical:
  abstract class Logical(override protected val ctx: M6800X0.Context,
                         override protected val opcode:Int) extends Instruction(ctx,opcode):

    protected def setFlags(a: Int, b: Int, r: Int, size: Size): Unit =
      import StatusRegister.StatusFlag.*
      var ccr = flags.getCCR
      val rm = (r & size.msb) != 0
      val rz = (r & size.mask) == 0

      if rm then ccr |= N.flag else ccr &= ~N.flag
      if rz then ccr |= Z.flag else ccr &= ~Z.flag
      ccr &= ~V.flag
      ccr &= ~C.flag

      flags.setCCR(ccr)