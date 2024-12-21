package ucesoft.mac.cpu.m68k

enum AddressingMode(words:Int):
  case DN extends AddressingMode(1)    // 0: Dn
  case AN extends AddressingMode(1)    // 1: An
  case AI extends AddressingMode(1)    // 2: (An)
  case PI extends AddressingMode(1)    // 3: (An)+
  case PD extends AddressingMode(1)    // 4: -(An)
  case DI extends AddressingMode(2)    // 5: (d,An)
  case IX extends AddressingMode(2)    // 6: (d,An,Xi)
  case AW extends AddressingMode(1)    // 7: (####).W
  case AL extends AddressingMode(2)    // 8: (####).L
  case DIPC extends AddressingMode(2)  // 9: (d,PC)
  case IXPC extends AddressingMode(2)  //10: (d,PC,Xi)
  case IM extends AddressingMode(1)    //11: ####
