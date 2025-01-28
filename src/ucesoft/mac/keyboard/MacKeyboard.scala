package ucesoft.mac.keyboard

import ucesoft.mac.{MACComponent, MacModel}
import ucesoft.mac.adb.ADBKeyboard

import java.awt.event.{KeyEvent, KeyListener}
import scala.collection.mutable
import java.awt.event.KeyEvent.*
import java.util.concurrent.locks.ReentrantLock
import javax.swing.ImageIcon

/**
 * @author Alessandro Abbruzzetti
 *         Created on 18/11/2024 11:09
 *
 *         M0110 / M0120
 *         ,---------------------------------------------------------.    ,---------------.
 *         |  `|  1|  2|  3|  4|  5|  6|  7|  8|  9|  0|  -|  =|Backs|    |Clr|  -|Lft|Rgt|
 *         |---------------------------------------------------------|    |---------------|
 *         |Tab  |  Q|  W|  E|  R|  T|  Y|  U|  I|  O|  P|  [|  ]|  \|    |  7|  8|  9|Up |
 *         |---------------------------------------------------------|    |---------------|
 *         |CapsLo|  A|  S|  D|  F|  G|  H|  J|  K|  L|  ;|  '|Return|    |  4|  5|  6|Dn |
 *         |---------------------------------------------------------|    |---------------|
 *         |Shift   |  Z|  X|  C|  V|  B|  N|  M|  ,|  .|  /|        |    |  1|  2|  3|   |
 *         `---------------------------------------------------------'    |-----------|Ent|
 *         |Opt|Mac |         Space               |Enter|Opt|             |  0|  .|   |   |
 *         `------------------------------------------------'             `---------------'
 *
 *         ,---------------------------------------------------------.    ,---------------.
 *         | 65| 25| 27| 29| 2B| 2F| 2D| 35| 39| 33| 3B| 37| 31|   67|    |+0F|+1D|+0D|+05|
 *         |---------------------------------------------------------|    |---------------|
 *         |   61| 19| 1B| 1D| 1F| 23| 21| 41| 45| 3F| 47| 43| 3D| 55|    |+33|+37|+39|+1B|
 *         |---------------------------------------------------------|    |---------------|
 *         |    73| 01| 03| 05| 07| 0B| 09| 4D| 51| 4B| 53| 4F|    49|    |+2D|+2F|+31|+11|
 *         |---------------------------------------------------------|    |---------------|
 *         |      71| 0D| 0F| 11| 13| 17| 5B| 5D| 57| 5F| 59|      71|    |+27|+29|+2B|   |
 *         `---------------------------------------------------------'    |-----------|+19|
 *         | 75|   6F|            63              |   69| 75|             |    +25|+03|   |
 *         `------------------------------------------------'             `---------------'
 *
 *         M0110A
 *
 *         ,---------------------------------------------------------. ,---------------.
 *         |  `|  1|  2|  3|  4|  5|  6|  7|  8|  9|  0|  -|  =|Bcksp| |Clr|  =|  /|  *|
 *         |---------------------------------------------------------| |---------------|
 *         |Tab  |  Q|  W|  E|  R|  T|  Y|  U|  I|  O|  P|  [|  ]|   | |  7|  8|  9|  -|
 *         |-----------------------------------------------------'   | |---------------|
 *         |CapsLo|  A|  S|  D|  F|  G|  H|  J|  K|  L|  ;|  '|Return| |  4|  5|  6|  +|
 *         |---------------------------------------------------------| |---------------|
 *         |Shift   |  Z|  X|  C|  V|  B|  N|  M|  ,|  .|  /|Shft|Up | |  1|  2|  3|   |
 *         |---------------------------------------------------------' |-----------|Ent|
 *         |Optio|Mac    |           Space           |  \|Lft|Rgt|Dn | |      0|  .|   |
 *         `---------------------------------------------------------' `---------------'
 *         ,---------------------------------------------------------. ,---------------.
 *         | 65| 25| 27| 29| 2B| 2F| 2D| 35| 39| 33| 3B| 37| 31|   67| |+0F|*11|*1B|*05|
 *         |---------------------------------------------------------| |---------------|
 *         |   61| 19| 1B| 1D| 1F| 23| 21| 41| 45| 3F| 47| 43| 3D|   | |+33|+37|+39|+1D|
 *         |-----------------------------------------------------'   | |---------------|
 *         |    73| 01| 03| 05| 07| 0B| 09| 4D| 51| 4B| 53| 4F|    49| |+2D|+2F|+31|*0D|
 *         |---------------------------------------------------------| |---------------|
 *         |      71| 0D| 0F| 11| 13| 17| 5B| 5D| 27| 5F| 59|  71|+1B| |+27|+29|+2B|   |
 *         |---------------------------------------------------------' |-----------|+19|
 *         |   75|     6F|            63             | 55|+0D|+05|+11| |    +25|+03|   |
 *         `---------------------------------------------------------' `---------------'
 *         [+] press: 0x79, 0xDD / release: 0x79, 0xUU
 *         [*] press: 0x71, 0x79, 0xDD / release: 0xF1, 0x79, 0xUU
 *         where DD = <raw code> and UU = <raw code> | 0x80
 *
 *         M0115 (ADB)
 *         ,---.   .---------------. ,---------------. ,---------------. ,-----------.             ,---.
 *         |Esc|   |F1 |F2 |F3 |F4 | |F5 |F6 |F7 |F8 | |F9 |F10|F11|F12| |F13|F14|F15|             |Pwr|
 *         `---'   `---------------' `---------------' `---------------' `-----------'             `---'
 *         ,-----------------------------------------------------------. ,-----------. ,---------------.
 *         |  `|  1|  2|  3|  4|  5|  6|  7|  8|  9|  0|  -|  =|Delete | |Hlp|Hom|PgU| |NmL|  =|  /|  *|
 *         |-----------------------------------------------------------| |-----------| |---------------
 *         |Tab  |  Q|  W|  E|  R|  T|  Y|  U|  I|  O|  P|  [|  ]|  \  | |Del|End|PgD| |  7|  8|  9|  -|
 *         |-----------------------------------------------------------| `-----------' |---------------|
 *         |CapsLo|  A|  S|  D|  F|  G|  H|  J|  K|  L|  ;|  "|Return  |               |  4|  5|  6|  +|
 *         |-----------------------------------------------------------|     ,---.     |---------------|
 *         |Shift   |  Z|  X|  C|  V|  B|  N|  M|  ,|  .|  /|     Shift|     |Up |     |  1|  2|  3|   |
 *         |-----------------------------------------------------------| ,-----------. |-----------|Ent|
 *         |Ctrl |Opt |Gui  |        Space            |Gui  |Opt |Ctrl | |Lef|Dow|Rig| |   0   |  .|   |
 *         `-----------------------------------------------------------' `-----------' `---------------'
 *
 *         The keyboard data line is bidirectional and is driven by whatever device is sending data. The keyboard clock line is driven by the keyboard only. All data transfers are synchronous with the keyboard clock. 
 *         Each transmission consists of eight bits, with the highestorder bits first.
 *         When sending data to the Macintosh, the keyboard clock transmits eight 330-usec cycles (160 usec low, 170 usec high) on the normally high clock line. 
 *         It places the data bit on the data line 40 usec before the falling edge of the clock line and maintains it for 330 usec. 
 *         The data bit is clocked into the Macintosh's VIA shift register on the rising edge of the keyboard clock cycle.
 *         When the Macintosh sends data to the keyboard, the keyboard clock transmits eight 400-usec cycles (180 usec low, 220 usec high) on the clock line. 
 *         On the falling edge of the keyboard clock cycle, the Macintosh places the data bit on the data line and holds it there for 400 usec. The keyboard reads the data bit 80 usec after the rising edge of the keyboard clock cycle.
 *         Only the Macintosh can initiate communication over the keyboard lines. On power-up of either the Macintosh or the keyboard, the Macintosh is In charge, and the external device is passive. 
 *         The Macintosh signals that it's ready to begin communication by pulling the keyboard data line low. Upon detecting this, the keyboard starts clocking and the
 *         Macintosh sends a command , The last bit of the command leaves the keyboard data line low; the Macintosh then indicates it's ready to receive the keyboard's response by setting the data line high.
 *         The Key Transition responses are sent out by the keyboard as a single byte: Bit 7 high means a key-up transition, and bit 7 low means a keydown. Bit 0 is always high. 
 *    
 *         (p.20 Apple Macintosh 128K/512K Computer Technical Information)
 *
 *         Commands
 *         ---------------------------------------------------------------------
 *         Inquiry     0x10    get key event with block or Null (0x7B)
 *         Instant     0x14    get key event or Null (0x7B)
 *         Model       0x16    get model number(M0110 responds with 0x09)
 *                             Bit 0: 1
 *                             Bits 1-3: keyboard model number, 1-8
 *                             Bits 4-6: next device number, 1-8
 *                             Bit 7: 1 if another device connected
 *         Test        0x36    test(ACK:0x7D/NAK:0x77)
 *         
 *         Model                   Code    Layout  Made in     Desc
 *         ---------------------------------------------------------------------
 *         M0110(GS536)            0x03    US      USA
 *         M0110(GS624)            0x09    US      USA
 *         M0110F                  0x03    French  Ireland     https://github.com/tmk/tmk_keyboard/issues/771
 *         M0110A(M923)            0x0B    US
 *         M0110AJ(M839)           0x0B    US
 *         M0110AJ(A615)           0x0B    US
 *         M0120(BCG9GRM0120)      0x11
 *         M0120 & M0110(G536)     0x13
 *         M0120 & M0110(G624)     0x19
 *         M0120 & M0110A(M923)    0x1B
 */
object MacKeyboard:
  private type KeyboardCodeTable = List[List[Int]]

  private inline val PLUS_MOD = 0x100
  private inline val STAR_MOD = 0x200
  // modifiers for M0115
  private inline val CAPS_LOCK_MOD  = 0x100
  private inline val NUM_LOCK_MOD   = 0x200
  private inline val POWER_MOD      = 0x400
  private inline val SCROLL_LOCK_MOD= 0x400
  private inline val DELETE_MOD     = 0x800
  private inline val CONTROL_MOD    = 0x1000
  private inline val SHIFT_MOD      = 0x2000
  private inline val OPTION_MOD     = 0x4000
  private inline val CMD_MOD        = 0x8000

  // ======================= M0110 =====================================================================================

  private final val M0110Table: KeyboardCodeTable = List(
    // |  `|  1|  2|  3|  4|  5|  6|  7|  8|  9|  0|  -|  =|Backs|
    List(0x10000f2/*ò*/,VK_1,VK_2,VK_3,VK_4,VK_5,VK_6,VK_7,VK_8,VK_9,VK_0,0x10000ec/*ì*/,0x10000f9/*ù*/,VK_BACK_SPACE),
    // |Tab  |  Q|  W|  E|  R|  T|  Y|  U|  I|  O|  P|  [|  ]|  \|
    List(VK_TAB,VK_Q,VK_W,VK_E,VK_R,VK_T,VK_Y,VK_U,VK_I,VK_O,VK_P,0x10000e8/*è*/,VK_PLUS,VK_BACK_SLASH),
    // |CapsLo|  A|  S|  D|  F|  G|  H|  J|  K|  L|  ;|  '|Return|
    List(VK_CAPS_LOCK,VK_A,VK_S,VK_D,VK_F,VK_G,VK_H,VK_J,VK_K,VK_L,0x10000e0/*à*/,VK_QUOTE,VK_ENTER),
    // |Shift   |  Z|  X|  C|  V|  B|  N|  M|  ,|  .|  /|        |
    List(VK_SHIFT,VK_Z,VK_X,VK_C,VK_V,VK_B,VK_N,VK_M,VK_COMMA,VK_PERIOD,VK_MINUS),
    // |Opt|Mac |         Space               |Enter|Opt|
    List(VK_LESS,VK_CONTROL,VK_SPACE,VK_INSERT,VK_LESS)
  )
  private final val M0110_ITALIAN_KEY_CODES: KeyboardCodeTable = List(
    // |  `|  1|  2|  3|  4|  5|  6|  7|  8|  9|  0|  -|  =|Backs|
    List(0x10000f2 /*ò*/ , VK_1, VK_2, VK_3, VK_4, VK_5, VK_6, VK_7, VK_8, VK_9, VK_0, 0x10000ec /*ì*/ , 0x10000f9 /*ù*/ , VK_BACK_SPACE),
    // |Tab  |  Q|  W|  E|  R|  T|  Y|  U|  I|  O|  P|  [|  ]|  \|
    List(VK_TAB, VK_Q, VK_W, VK_E, VK_R, VK_T, VK_Y, VK_U, VK_I, VK_O, VK_P, 0x10000e8 /*è*/ , VK_PLUS, VK_BACK_SLASH),
    // |CapsLo|  A|  S|  D|  F|  G|  H|  J|  K|  L|  ;|  '|Return|
    List(VK_CAPS_LOCK, VK_A, VK_S, VK_D, VK_F, VK_G, VK_H, VK_J, VK_K, VK_L, 0x10000e0 /*à*/ , VK_QUOTE, VK_ENTER),
    // |Shift   |  Z|  X|  C|  V|  B|  N|  M|  ,|  .|  /|        |
    List(VK_SHIFT, VK_Z, VK_X, VK_C, VK_V, VK_B, VK_N, VK_M, VK_COMMA, VK_PERIOD, VK_MINUS),
    // |Opt|Mac |         Space               |Enter|Opt|
    List(VK_LESS, VK_CONTROL, VK_SPACE, VK_INSERT, VK_LESS)
  )

  // ======================= M0110A ====================================================================================

  private final val M0110ATable: KeyboardCodeTable = List(
    // |  `|  1|  2|  3|  4|  5|  6|  7|  8|  9|  0|  -|  =|Backs|Clr|  =|  /|  *|
    List(0x65, 0x25, 0x27, 0x29, 0x2B, 0x2F, 0x2D, 0x35, 0x39, 0x33, 0x3B, 0x37, 0x31, 0x67,PLUS_MOD | 0x0F,STAR_MOD | 0x11,STAR_MOD | 0x1B,STAR_MOD | 0x05),
    // |Tab  |  Q|  W|  E|  R|  T|  Y|  U|  I|  O|  P|  [|  ]|   |  7|  8|  9|  -|
    List(0x61, 0x19, 0x1B, 0x1D, 0x1F, 0x23, 0x21, 0x41, 0x45, 0x3F, 0x47, 0x43, 0x3D,PLUS_MOD | 0x33,PLUS_MOD | 0x37,PLUS_MOD | 0x39,PLUS_MOD | 0x1D),
    // |CapsLo|  A|  S|  D|  F|  G|  H|  J|  K|  L|  ;|  '|Return|  4|  5|  6|  +|
    List(0x73, 0x01, 0x03, 0x05, 0x07, 0x0B, 0x09, 0x4D, 0x51, 0x4B, 0x53, 0x4F, 0x49,PLUS_MOD | 0x2D,PLUS_MOD | 0x2F,PLUS_MOD | 0x31,STAR_MOD | 0x0D),
    // |Shift |  Z|  X|  C|  V|  B|  N|  M|  ,|  .|  /|Shft|Up   |  1|  2|  3|
    List(0x71, 0x0D, 0x0F, 0x11, 0x13, 0x17, 0x5B, 0x5D, 0x57, 0x5F, 0x59, 0x71, PLUS_MOD | 0x1B,PLUS_MOD | 0x27,PLUS_MOD | 0x29,PLUS_MOD | 0x2B),
    // |Optio|Mac    |           Space           |  \|Lft|Rgt|Dn |      0|  .|   |
    List(0x75, 0x6F, 0x63, 0x55, PLUS_MOD | 0x0D,PLUS_MOD | 0x05,PLUS_MOD | 0x11,PLUS_MOD | 0x25,PLUS_MOD | 0x03)
  )
  private final val M0110A_ITALIAN_KEY_CODES: KeyboardCodeTable = List(
    // |  `|  1|  2|  3|  4|  5|  6|  7|  8|  9|  0|  -|  =|Backs|Clr|  =|  /|  *|
    List(0x10000f2 /*ò*/ , VK_1, VK_2, VK_3, VK_4, VK_5, VK_6, VK_7, VK_8, VK_9, VK_0, 0x10000ec /*ì*/ , 0x10000f9 /*ù*/ ,VK_BACK_SPACE,VK_HOME,VK_END,VK_DIVIDE,VK_MULTIPLY),
    // |Tab  |  Q|  W|  E|  R|  T|  Y|  U|  I|  O|  P|  [|  ]|   |   |  7|  8|  9|  -|
    List(VK_TAB, VK_Q, VK_W, VK_E, VK_R, VK_T, VK_Y, VK_U, VK_I, VK_O, VK_P, 0x10000e8 /*è*/ ,VK_PLUS,VK_NUMPAD7,VK_NUMPAD8,VK_NUMPAD9,VK_SUBTRACT),
    // |CapsLo|  A|  S|  D|  F|  G|  H|  J|  K|  L|  ;|  '|Return|  4|  5|  6|  +|
    List(VK_CAPS_LOCK, VK_A, VK_S, VK_D, VK_F, VK_G, VK_H, VK_J, VK_K, VK_L, 0x10000e0 /*à*/ , VK_QUOTE, VK_ENTER,VK_NUMPAD4,VK_NUMPAD5,VK_NUMPAD6,VK_ADD),
    // |Shift |  Z|  X|  C|  V|  B|  N|  M|  ,|  .|  /|Shft|Up   |  1|  2|  3|
    List(VK_SHIFT, VK_Z, VK_X, VK_C, VK_V, VK_B, VK_N, VK_M, VK_COMMA, VK_PERIOD, VK_MINUS,VK_SHIFT,VK_UP,VK_NUMPAD1,VK_NUMPAD2,VK_NUMPAD3),
    // |Optio|Mac    |           Space           |  \|Lft|Rgt|Dn |      0|  .|   |
    List(VK_LESS, VK_CONTROL, VK_SPACE, VK_BACK_SLASH, VK_LEFT,VK_RIGHT,VK_DOWN,VK_NUMPAD0,VK_DECIMAL)
  )

  // ======================= M0115 =====================================================================================
  private final val M0115Table: KeyboardCodeTable = List(
    // |Esc|   |F1 |F2 |F3 |F4 | |F5 |F6 |F7 |F8 | |F9 |F10|F11|F12| |F13|F14|F15|             |Pwr|
    List(0x35,0x7A,0x78,0x63,0x76,0x60,0x61,0x62,0x64,0x65,0x6D,0x67,0x6F/*No F13,14,15*/,POWER_MOD | 0x7F),
    // |  `|  1|  2|  3|  4|  5|  6|  7|  8|  9|  0|  -|  =|Delete | |Hlp|Hom|PgU| |NmL|  =|  /|  *|
    List(0x32,0x12,0x13,0x14,0x15,0x17,0x16,0x1A,0x1C,0x19,0x1D,0x1B,0x18,0x33,0x72,0x73,0x74,NUM_LOCK_MOD | 0x47/*No =*/,0x4B,0x43),
    // |Tab  |  Q|  W|  E|  R|  T|  Y|  U|  I|  O|  P|  [|  ]|  \  | |Del|End|PgD| |  7|  8|  9|  -|
    List(0x30,0xC,0xD,0xE,0xF,0x11,0x10,0x20,0x22,0x1F,0x23,0x21,0x1E,0x2A,DELETE_MOD | 0x75,0x77,0x79,0x59,0x5B,0x5C,0x4E),
    // |CapsLo|  A|  S|  D|  F|  G|  H|  J|  K|  L|  ;|  "|Return  |               |  4|  5|  6|  +|
    List(CAPS_LOCK_MOD | 0x39,0x0,0x1,0x2,0x3,0x5,0x4,0x26,0x28,0x25,0x29,0x27,0x24,0x56,0x57,0x58,0x45),
    // |Shift   |  Z|  X|  C|  V|  B|  N|  M|  ,|  .|  /|     Shift|     |Up |     |  1|  2|  3|Ent|
    List(SHIFT_MOD | 0x38,0x6,0x7,0x8,0x9,0xB,0x2D,0x2E,0x2B,0x2F,0x2C,/*No right shift*/0x3E,0x53,0x54,0x55,0x4C),
    // |Ctrl |Opt |Gui  |        Space            |Gui  |Opt |Ctrl | |Lef|Rig|Dow| |   0   |  .|   |
    List(CONTROL_MOD | 0x36,OPTION_MOD | 0x3A,CMD_MOD | 0x37,0x31,0x3B,0x3C,0x3D,0x52,0x41)
  )
  private final val M0115_ITALIAN_KEY_CODES: KeyboardCodeTable = List(
    // |Esc|   |F1 |F2 |F3 |F4 | |F5 |F6 |F7 |F8 | |F9 |F10|F11|F12| |F13|F14|F15|             |Pwr|
    List(VK_ESCAPE,VK_F1,VK_F2,VK_F3,VK_F4,VK_F5,VK_F6,VK_F7,VK_F8,VK_F9,VK_F10,VK_F11,VK_F12/*No F13,14,15*/,VK_PAUSE),
    // |  `|  1|  2|  3|  4|  5|  6|  7|  8|  9|  0|  -|  =|Delete | |Hlp|Hom|PgU| |NmL|  =|  /|  *|
    List(0x10000f2 /*ò*/ , VK_1, VK_2, VK_3, VK_4, VK_5, VK_6, VK_7, VK_8, VK_9, VK_0, 0x10000ec /*ì*/ , 0x10000f9 /*ù*/,VK_BACK_SPACE,VK_INSERT,VK_HOME,VK_PAGE_UP,VK_NUM_LOCK,/*No =*/VK_DIVIDE,VK_MULTIPLY),
    // |Tab  |  Q|  W|  E|  R|  T|  Y|  U|  I|  O|  P|  [|  ]|  \  | |Del|End|PgD| |  7|  8|  9|  -|
    List(VK_TAB, VK_Q, VK_W, VK_E, VK_R, VK_T, VK_Y, VK_U, VK_I, VK_O, VK_P, 0x10000e8 /*è*/ ,VK_PLUS,VK_BACK_SLASH,VK_DELETE,VK_END,VK_PAGE_DOWN,VK_NUMPAD7,VK_NUMPAD8,VK_NUMPAD9,VK_SUBTRACT),
    // |CapsLo|  A|  S|  D|  F|  G|  H|  J|  K|  L|  ;|  "|Return  |               |  4|  5|  6|  +|
    List(VK_CAPS_LOCK, VK_A, VK_S, VK_D, VK_F, VK_G, VK_H, VK_J, VK_K, VK_L, 0x10000e0 /*à*/ , VK_QUOTE, VK_ENTER,VK_NUMPAD4,VK_NUMPAD5,VK_NUMPAD6,VK_ADD),
    // |Shift   |  Z|  X|  C|  V|  B|  N|  M|  ,|  .|  /|     Shift|     |Up |     |  1|  2|  3|Ent|
    List(VK_SHIFT, VK_Z, VK_X, VK_C, VK_V, VK_B, VK_N, VK_M, VK_COMMA, VK_PERIOD, VK_MINUS,VK_UP,VK_NUMPAD1,VK_NUMPAD2,VK_NUMPAD3/*no Ent*/),
    // |Ctrl |Opt |Gui  |        Space            |Gui  |Opt |Ctrl | |Lef|Rig|Dow| |   0   |  .|   |
    List(VK_CONTROL,VK_LESS,VK_ALT_GRAPH, VK_SPACE,VK_LEFT,VK_RIGHT,VK_DOWN,VK_NUMPAD0,VK_DECIMAL)
  )

  enum KeyboardModel(val code:Int, val codeTable:KeyboardCodeTable,val keyCodeTable:KeyboardCodeTable):
    case M0110 extends KeyboardModel(code = 0x09, codeTable = M0110Table, keyCodeTable = M0110_ITALIAN_KEY_CODES)
    case M0110A extends KeyboardModel(code = 0x0B, codeTable = M0110ATable, keyCodeTable = M0110A_ITALIAN_KEY_CODES)
    case M0115 extends KeyboardModel(code = 0x0B, codeTable = M0115Table, keyCodeTable = M0115_ITALIAN_KEY_CODES)

  def main(args:Array[String]): Unit =
    val f = new javax.swing.JFrame()
    f.setSize(300,300)
    f.addKeyListener(new KeyListener:
      override def keyPressed(e: KeyEvent): Unit =
        println(e)
        println(e.getExtendedKeyCode == VK_MULTIPLY)
      override def keyReleased(e: KeyEvent): Unit = println(e)
      override def keyTyped(e: KeyEvent): Unit = {}
    )
    f.setVisible(true)

class MacKeyboard extends MACComponent with KeyListener:
  import MacKeyboard.*
  override protected val componentName = "Keyboard"
  override protected val icon = new ImageIcon(getClass.getResource("/resources/trace/keyboard.png"))
  
  private val eventQueue = new mutable.Queue[Int]
  private val queueLock = new ReentrantLock()
  private var keyboardModel = KeyboardModel.M0110
  private var keyMap : Map[Int,Int] = buildMap()
  private var capsLockOn = false
  private val adbKeyboard = new ADBKeyboard

  def getADBKeyboard: ADBKeyboard = adbKeyboard

  override def getProperties: List[MACComponent.Property] =
    import MACComponent.Property
    List(
      Property("Model",keyboardModel.toString),
      Property("Capslock on",capsLockOn.toString),
      Property("Key queue size",eventQueue.size.toString)
    )

  override protected def setModel(model: MacModel): Unit =
    super.setModel(model)
    keyboardModel = model match
      case MacModel.PLUS => KeyboardModel.M0110A
      case MacModel.SE | MacModel.SEFDHD | MacModel.CLASSIC => KeyboardModel.M0115
      case _ => KeyboardModel.M0110

    keyMap = buildMap()
    reset()

  private def buildMap(): Map[Int,Int] =
    val map = for lists <- keyboardModel.keyCodeTable.zip(keyboardModel.codeTable); codes <- lists._1.zip(lists._2) yield codes
    map.toMap

  override protected def reset(): Unit =
    super.reset()
    queueLock.lock()
    try
      eventQueue.clear()
    finally
      queueLock.unlock()

  private def adbKey(pressed:Boolean,code:Int): Unit =
    keyMap.get(code) match
      case Some(keyCode) =>
        queueLock.lock()
        try
          val capsLock = (keyCode & CAPS_LOCK_MOD) != 0
          if pressed && capsLock then
            adbKeyboard.setCapsLockPressed()
          adbKeyboard.setNumlockOn(on = (keyCode & NUM_LOCK_MOD) != 0)
          adbKeyboard.setScrolllockOn(on = (keyCode & SCROLL_LOCK_MOD) != 0)
          adbKeyboard.setDeleteOn(on = (keyCode & DELETE_MOD) != 0)
          adbKeyboard.setControlOn(on = (keyCode & CONTROL_MOD) != 0)
          adbKeyboard.setShiftOn(on = (keyCode & SHIFT_MOD) != 0)
          adbKeyboard.setOptionOn(on = (keyCode & OPTION_MOD) != 0)
          adbKeyboard.setCmdOn(on = (keyCode & CMD_MOD) != 0)

          if !(!pressed && capsLock) then
            adbKeyboard.keyEvent(keyCode & 0xFF,pressed)
        finally
          queueLock.unlock()
      case None =>

  override def keyPressed(e: KeyEvent): Unit =
    if e.isAltDown then return

    if macModel.ordinal >= MacModel.SE.ordinal then
      adbKey(pressed = true,code = e.getExtendedKeyCode)
    else if e.getExtendedKeyCode == VK_CAPS_LOCK then
      capsLockOn ^= true
    else
      keyMap.get(e.getExtendedKeyCode) match
        case Some(_code) =>
          queueLock.lock()
          try
            val code = _code & 0xFF
            val arrowMod = (_code & PLUS_MOD) != 0
            val keypadMod = (_code & STAR_MOD) != 0

            if !eventQueue.contains(code) then
              if arrowMod then eventQueue.enqueue(0x79)
              else if keypadMod then
                eventQueue.enqueue(0x71)
                eventQueue.enqueue(0x79)
              eventQueue.enqueue(code)
          finally
            queueLock.unlock()
        case None =>
  override def keyReleased(e: KeyEvent): Unit =
    if e.isAltDown then return

    if macModel.ordinal >= MacModel.SE.ordinal then
      adbKey(pressed = false,code = e.getExtendedKeyCode)
    else
      keyMap.get(e.getExtendedKeyCode) match
      case Some(_code) =>
        queueLock.lock()
        try
          val code = _code & 0xFF
          val arrowMod = (_code & PLUS_MOD) != 0
          val keypadMod = (_code & STAR_MOD) != 0
          val key = 0x80 | code

          if !eventQueue.contains(key) then
            if arrowMod then eventQueue.enqueue(0x79)
            else if keypadMod then
              eventQueue.enqueue(0xF1)
              eventQueue.enqueue(0x79)
            eventQueue.enqueue(key)
        finally
          queueLock.unlock()
      case None =>
  override def keyTyped(e: KeyEvent): Unit = {}

  def command(cmd:Int): Int =
    cmd match
      case 0x10|0x12 => // Inquiry, Istant
        queueLock.lock()
        try
          if eventQueue.isEmpty then
            if capsLockOn then keyMap(VK_CAPS_LOCK) else 0x7B
          else eventQueue.dequeue()
        finally
          queueLock.unlock()
      case 0x16 => // Model
        reset()
        keyboardModel.code
      case 0x36 => // Test
        0x7D
      case _ =>
        log.warning("Keyboard: received an unknown command: %02X",cmd)
        0x7B
