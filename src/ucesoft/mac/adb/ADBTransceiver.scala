package ucesoft.mac.adb

import ucesoft.mac.MACComponent

import javax.swing.ImageIcon
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.compiletime.uninitialized

/**
 * Bases on https://github.com/lampmerchant/macseadb88/blob/main/macseadb88.asm
 *
 * @author Alessandro Abbruzzetti
 *         Created on 31/12/2024 14:49  
 */
class ADBTransceiver extends MACComponent:
  override protected val componentName = "ADB"
  override protected val icon = new ImageIcon(getClass.getResource("/resources/trace/adb.png"))

  private inline val STATE_COMMAND = 0
  private inline val STATE_T_1 = 1
  private inline val STATE_T_2 = 2
  private inline val STATE_IDLE = 3

  private inline val CMD_SEND_RESET = 0
  private inline val CMD_FLUSH = 1
  private inline val CMD_LISTEN = 2
  private inline val CMD_TALK = 3

  private val COMMANDS = Array("RESET", "FLUSH", "LISTEN", "TALK")
  private val STATES = Array("COMMAND", "T_1", "T_2", "IDLE")

  private var state = STATE_IDLE

  private val devices = new ListBuffer[ADBDevice]
  private val response = new ListBuffer[Int]
  private var irq = false
  private val cmd = new ArrayBuffer[Int]
  private var activeDevice : ADBDevice = uninitialized
  private var command = 0
  private var address = 0
  private var reg = 0

  override def getProperties: List[MACComponent.Property] =
    import MACComponent.Property
    List(
      Property("State", state.toString),
      Property("Last command", if command == -1 then "-" else s"${COMMANDS(command)} $address/$reg"),
      Property("Active device", if activeDevice == null then "-" else activeDevice.name)
    )

  def addDevice(device:ADBDevice): Unit = devices += device
  def removeDevice(device:ADBDevice): Unit = devices -= device

  def isInterrupt: Boolean = irq

  private def deviceHasSrq: Boolean = devices.exists(_.isServiceRequest)

  def setState(newState:Int): Option[Int] =
    if newState == state then return None

    state = newState
    irq = false

    state match
      case STATE_IDLE =>
        None
      case STATE_COMMAND =>
        if cmd.nonEmpty then
          processCommand(true)
          cmd.clear()
        response.clear()
        None
      case STATE_T_1 | STATE_T_2 =>
        if cmd.nonEmpty then
          if processCommand(false) then
            cmd.clear()
          else
            return None

        irq = cmd.isEmpty && response.isEmpty

        if response.isEmpty then Some(0xFF)
        else
          val data = response.remove(0)
          Some(data)
  end setState

  def clockInByte(data:Int): Unit = cmd += data

  private def processCommand(completeListen:Boolean): Boolean =
    address = cmd(0) >> 4
    command = (cmd(0) >> 2) & 3
    reg = cmd(0) & 3

    println(s"Processing command ${COMMANDS(command)} for device $address/$reg")

    if command == 0 then
      println("RESET")
      devices.foreach(_.commandReset())
      return true

    devices.find(_.getAddress == address) match
      case None =>
      case Some(device) =>
        activeDevice = device
        command match
          case CMD_FLUSH =>
            println("FLUSH")
            device.commandFlush()
          case CMD_LISTEN =>
            if completeListen then
              println(s"LISTEN $address/$reg")
              activeDevice.commandListen(reg, this.cmd.tail.toList)
              return true
            return false
          case CMD_TALK =>
            response.clear()
            response.addAll(device.commandTalk(reg))
          case _ =>
    true
  end processCommand

  def isServiceRequestPending: Boolean =
    if state == STATE_IDLE && !irq && deviceHasSrq then
      irq = true
      true
    else
      false