package ucesoft.mac.adb

import scala.collection.mutable

/**
 * @author Alessandro Abbruzzetti
 *         Created on 30/12/2024 16:10  
 */
class ADBKeyboard extends ADBDevice(address = 2,handlerID = 2,name = "ADB Keyboard"):
  private val eventQueue = new mutable.Queue[Int]
  private var capsLockOn = false
  private var capsLockPressed = false
  private var deleteOn = false
  private var controlOn = false
  private var shiftOn = false
  private var optionOn = false
  private var cmdOn = false
  private var numlockOn = false
  private var scrolllockOn = false
  private var led321 = 0

  def setCapsLockPressed(): Unit =
    capsLockOn ^= true
    capsLockPressed = true
  def setDeleteOn(on:Boolean): Unit = deleteOn = on
  def setControlOn(on:Boolean): Unit = controlOn = on
  def setShiftOn(on:Boolean): Unit = shiftOn = on
  def setOptionOn(on:Boolean): Unit = optionOn = on
  def setCmdOn(on:Boolean): Unit = cmdOn = on
  def setNumlockOn(on:Boolean): Unit = numlockOn = on
  def setScrolllockOn(on:Boolean): Unit = scrolllockOn = on

  private def getReg2: Int =
    var reg2 = 0xF8 | led321
    if deleteOn then reg2 &= ~(1 << 14)
    if capsLockOn then reg2 &= ~(1 << 13)
    if controlOn then reg2 &= ~(1 << 11)
    if shiftOn then reg2 &= ~(1 << 10)
    if optionOn then reg2 &= ~(1 << 9)
    if cmdOn then reg2 &= ~(1 << 8)
    if numlockOn then reg2 &= ~(1 << 7)
    if scrolllockOn then reg2 &= ~(1 << 6)
    reg2

  // prevent the handler id changing from 2 -> 3
  override protected def handlerIDChanged(oldHandlerID:Int,newHandlerID:Int): Boolean = false

  override def commandReset(): Unit =
    super.commandReset()
    commandFlush()
  override def commandFlush(): Unit =
    eventQueue.clear()

  def keyEvent(keyCode:Int,pressed:Boolean): Unit =
    val code = if capsLockPressed then
      capsLockPressed = false
      if capsLockOn then keyCode else 0x80 | keyCode
    else if pressed then keyCode else 0x80 | keyCode
    if !eventQueue.contains(code) then
      serviceRequest = true
      eventQueue += code
      if code == 0x7F then eventQueue += code // Power button

  override def commandTalk(register: Int): Array[Int] =
    if register == 0 then
      val size = eventQueue.size
      if size == 0 then
        serviceRequest = false
        return Array()
      if size == 1 then
        serviceRequest = true
        return Array(eventQueue.dequeue())
      serviceRequest = true
      Array(eventQueue.dequeue(),eventQueue.dequeue())
    else if register == 2 then
      val reg2 = getReg2
      Array(reg2 >> 8,reg2 & 0xFF)
    else
      super.commandTalk(register)

  override def commandListen(register:Int,values:List[Int]): Unit =
    if register == 2 then
      // leds
      led321 = values.tail.head & 7
      //println(s"LEDS changed to SCROLL_LOCK=${(led321 >> 2) & 1} CAPS_LOCK=${(led321 >> 1) & 1} NUM_LOCK=${led321 & 1}")
    else
      super.commandListen(register,values)

