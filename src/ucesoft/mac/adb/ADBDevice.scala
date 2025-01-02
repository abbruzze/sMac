package ucesoft.mac.adb

/**
 * @author Alessandro Abbruzzetti
 *         Created on 23/12/2024 19:29  
 */
abstract class ADBDevice(private val address:Int,private val handlerID:Int,val name:String):
  protected var serviceRequest = false
  protected var serviceRequestEnabled = false // seems to be not used
  protected var reg3 : Int = initR3

  protected def initR3: Int = (address & 0xF) << 8 | handlerID & 0xFF
  
  def getAddress: Int = (reg3 >> 8) & 0xF
  def isServiceRequest: Boolean = /*serviceRequestEnabled & */
    serviceRequest

  def commandReset(): Unit =
    reg3 = initR3
  def commandFlush(): Unit = {}
  def commandListen(register:Int,values:List[Int]): Unit =
    if register == 3 then
      val oldReg3 = reg3
      val value = values.head << 8 | values.tail.head
      val id = value & 0xFF
      // special id
      id match
        case 0xFF => // self-test
          //println(s"ADB Device[$handlerID] self-test")
        case 0x00 => // Change bits 8-13 of register 3 to match the rest of the command; leave Device Handler ID value unchanged.
          reg3 &= ~0x3F00
          reg3 |= value & 0x3F00
          //println(s"ADB Device[$handlerID] writing to register 3 with ID 00: ${value.toHexString}, new reg3=${reg3.toHexString}")
        case 0xFD => // Change Device Address to match bits 8-11 if the device activator has been depressed; leave Device Handler ID value and flags unchanged.
          reg3 &= ~0xF00
          reg3 |= value & 0xF00
          //println(s"ADB Device[$handlerID] writing to register 3 with ID FD: ${value.toHexString}, new reg3=${reg3.toHexString}")
        case 0xFE => // Change Device Address to match bits 8-11 if the result produces no address duplication on the bus; leave Device Handler ID value and flags unchanged.
          reg3 &= ~0xF00
          reg3 |= value & 0xF00
          //println(s"ADB Device[$handlerID] writing to register 3 with ID FE: ${value.toHexString}, new reg3=${reg3.toHexString}")
        case _ =>
          reg3 = value & 0xFFFF
          //println(s"ADB Device[$handlerID] writing to register 3: ${value.toHexString}, new reg3=${reg3.toHexString}")

      val oldSRE = serviceRequestEnabled
      serviceRequestEnabled = (reg3 & (1 << 13)) != 0
      //println(s"ADB Device[$handlerID] writing to register 3: serviceRequestEnabled=$serviceRequestEnabled")

      if oldSRE != serviceRequestEnabled then
        sreChanged()
      val oldAddress = (oldReg3 >> 8) & 0xF
      val newAddress = (reg3 >> 8) & 0xF
      if oldAddress != newAddress then
        addressChanged(oldAddress,newAddress)
      val oldHandlerID = oldReg3 & 0xFF
      val newHandlerID = reg3 & 0xFF
      if oldHandlerID != newHandlerID then
        if !handlerIDChanged(oldHandlerID,newHandlerID) then
          println(s"ADB Device[$handlerID] handlerIDChanged returned false: old=$oldHandlerID, new=$newHandlerID")
          reg3 = (reg3 & 0xFF00) | oldHandlerID
  end commandListen

  protected def sreChanged(): Unit = {}
  protected def addressChanged(oldAddress:Int,newAddress:Int): Unit = {}
  protected def handlerIDChanged(oldHandlerID:Int,newHandlerID:Int): Boolean = true

  def commandTalk(register:Int): Array[Int] =
    serviceRequest = false

    if register == 3 then
      val r3 = 1 << 14 | reg3 // bit 14 Exceptional event, always 1

      Array(r3 >> 8,r3 & 0xFF)
    else
      Array()
  end commandTalk
