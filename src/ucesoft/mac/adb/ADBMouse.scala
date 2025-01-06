package ucesoft.mac.adb

/**
 * @author Alessandro Abbruzzetti
 *         Created on 27/12/2024 15:11  
 */
class ADBMouse extends ADBDevice(address = 3,handlerID = 1,name = "ADB Mouse"):
  private var xCounter = 0
  private var yCounter = 0
  private var buttonPressed = false
  private var srCountdown = 0

  def moveDelta(deltaX:Int,deltaY:Int): Unit =
    xCounter += deltaX
    yCounter += deltaY
    serviceRequest = true // has new data
    srCountdown = 10

  def setButtonPressed(pressed:Boolean): Unit = 
    buttonPressed = pressed
    serviceRequest = true // has new data
    srCountdown = 10

  private inline def getAdjustedCounter(counter:Int): Int =
    var adj = counter & 0x3F
    if counter < 0 then adj |= 0x40
    adj

  override def commandReset(): Unit =
    xCounter = 0
    yCounter = 0
    reg3 = initR3
  override def commandFlush(): Unit =
    println("ADB Mouse flush")

  override def handlerIDChanged(oldHandlerID: Int, newHandlerID: Int): Boolean =
    println(s"ADB Mouse handlerIDChanged: ignored new $newHandlerID")
    false

  /**
   * Bit	Description
   * 15	  Button status: 1 if released
   * 14–8	Y axis moves (2’s complement, negative up)
   * 7	  Reserved, always 1
   * 6–0	X axis offset (2’s complement, negative left)
   */
  override def commandTalk(register: Int): Array[Int] =
    if register == 0 then
      if !serviceRequest then return Array()
      if srCountdown > 0 then
        srCountdown -= 1
        if srCountdown == 0 then
          serviceRequest = false
      
      //println(s"ADB Mouse: pressed=${buttonPressed} y=${getAdjustedCounter(yCounter)} x=${getAdjustedCounter(xCounter)}")
      val out = Array(
        (if !buttonPressed then 0x80 else 0) | getAdjustedCounter(yCounter),
        0x80 | getAdjustedCounter(xCounter)
      )
      xCounter = 0
      yCounter = 0
      out
    else
      super.commandTalk(register)

