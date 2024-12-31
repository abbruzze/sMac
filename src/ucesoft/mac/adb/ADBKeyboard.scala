package ucesoft.mac.adb

import scala.collection.mutable

/**
 * @author Alessandro Abbruzzetti
 *         Created on 30/12/2024 16:10  
 */
class ADBKeyboard extends ADBDevice(address = 2,handlerID = 1,name = "ADB Keyboard"):
  private val eventQueue = new mutable.Queue[Int]

  def keyEvent(keyCode:Int,pressed:Boolean): Unit =
    serviceRequest = true
    println("PRESSED")
    eventQueue += (if pressed then 0x24 else 0x24 | 0x80)

  override def commandTalk(register: Int): Array[Int] =
    serviceRequest = false

    if register == 0 then
      val size = eventQueue.size
      if size == 0 then
        return Array()
      if size == 1 then
        return Array(eventQueue.dequeue())
      Array(eventQueue.dequeue(),eventQueue.dequeue())
    else if register == 2 then
      Array(0,0)
    else
      super.commandTalk(register)

