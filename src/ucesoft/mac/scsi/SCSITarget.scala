package ucesoft.mac.scsi

import javax.swing.Icon

/**
 * @author Alessandro Abbruzzetti
 *         Created on 06/12/2024 18:04  
 */
trait SCSITarget:
  val id : Int
  val scsiOn : Icon
  val scsiOff : Icon
  val name : String
  
  def sizeInBytes: Int
  def identifyCommandLen(cmd:Int): Option[Int]
  def executeCommand(cmd:Array[Int]): Option[SCSITargetResponse]
  def disconnect(): Unit

enum SCSITargetResponse:
  case Status(status:Int)
  case DataIn(data:Array[Byte])
  case DataOut(len:Int,writeHandler:Array[Byte] => Int)

/*
+=====================================-============-======-=============+
|  Command name                       | Operation  | Type | Subclause   |
|                                     |   code     |      |             |
|-------------------------------------+------------+------+-------------|
|  CHANGE DEFINITION                  |    40h     |   O  |   8.2.1     |
|  COMPARE                            |    39h     |   O  |   8.2.2     |
|  COPY                               |    18h     |   O  |   8.2.3     |
|  COPY AND VERIFY                    |    3Ah     |   O  |   8.2.4     |
|  FORMAT UNIT                        |    04h     |   M  |   9.2.1     |
|  INQUIRY                            |    12h     |   M  |   8.2.5     |
|  LOCK-UNLOCK CACHE                  |    36h     |   O  |   9.2.2     |
|  LOG SELECT                         |    4Ch     |   O  |   8.2.6     |
|  LOG SENSE                          |    4Dh     |   O  |   8.2.7     |
|  MODE SELECT(6)                     |    15h     |   O  |   8.2.8     |
|  MODE SELECT(10)                    |    55h     |   O  |   8.2.9     |
|  MODE SENSE(6)                      |    1Ah     |   O  |   8.2.10    |
|  MODE SENSE(10)                     |    5Ah     |   O  |   8.2.11    |
|  PRE-FETCH                          |    34h     |   O  |   9.2.3     |
|  PREVENT-ALLOW MEDIUM REMOVAL       |    1Eh     |   O  |   9.2.4     |
|  READ(6)                            |    08h     |   M  |   9.2.5     |
|  READ(10)                           |    28h     |   M  |   9.2.6     |
|  READ BUFFER                        |    3Ch     |   O  |   8.2.12    |
|  READ CAPACITY                      |    25h     |   M  |   9.2.7     |
|  READ DEFECT DATA                   |    37h     |   O  |   9.2.8     |
|  READ LONG                          |    3Eh     |   O  |   9.2.9     |
|  REASSIGN BLOCKS                    |    07h     |   O  |   9.2.10    |
|  RECEIVE DIAGNOSTIC RESULTS         |    1Ch     |   O  |   8.2.13    |
|  RELEASE                            |    17h     |   M  |   9.2.11    |
|  REQUEST SENSE                      |    03h     |   M  |   8.2.14    |
|  RESERVE                            |    16h     |   M  |   9.2.12    |
|  REZERO UNIT                        |    01h     |   O  |   9.2.13    |
|  SEARCH DATA EQUAL                  |    31h     |   O  |   9.2.14.1  |
|  SEARCH DATA HIGH                   |    30h     |   O  |   9.2.14.2  |
|  SEARCH DATA LOW                    |    32h     |   O  |   9.2.14.3  |
|  SEEK(6)                            |    0Bh     |   O  |   9.2.15    |
|  SEEK(10)                           |    2Bh     |   O  |   9.2.15    |
|  SEND DIAGNOSTIC                    |    1Dh     |   M  |   8.2.15    |
|  SET LIMITS                         |    33h     |   O  |   9.2.16    |
|  START STOP UNIT                    |    1Bh     |   O  |   9.2.17    |
|  SYNCHRONIZE CACHE                  |    35h     |   O  |   9.2.18    |
|  TEST UNIT READY                    |    00h     |   M  |   8.2.16    |
|  VERIFY                             |    2Fh     |   O  |   9.2.19    |
|  WRITE(6)                           |    0Ah     |   O  |   9.2.20    |
|  WRITE(10)                          |    2Ah     |   O  |   9.2.21    |
|  WRITE AND VERIFY                   |    2Eh     |   O  |   9.2.22    |
|  WRITE BUFFER                       |    3Bh     |   O  |   8.2.17    |
|  WRITE LONG                         |    3Fh     |   O  |   9.2.23    |
|  WRITE SAME                         |    41h     |   O  |   9.2.24    |
|-----------------------------------------------------------------------|
|Key:  M = Command implementation is mandatory.                         |
|      O = Command implementation is optional.                          |
+=======================================================================+
*/
abstract class DirectAccessSCSITarget(override val id:Int) extends SCSITarget:
  protected val BLOCK_SIZE : Int

  protected inline val STATUS_GOOD = 0
  protected inline val STATUS_CHECK_CONDITION = 2

  protected final val VENDOR        = "sMAC    ".getBytes
  protected final val PRODUCT_ID    = "VIRTUAL HDD".getBytes
  protected final val VERSION       = "1.0 ".getBytes
  protected final val MODE_SENSE_30 = "APPLE COMPUTER, INC.".getBytes

  private val commandLenMap = Map (
    // group 0 six-byte commands
    0x00 -> 6,    // UNIT TEST
    0x03 -> 6,    // REQUEST SENSE
    0x04 -> 6,    // FORMAT UNIT
    0x08 -> 6,    // READ
    0x0A -> 6,    // WRITE
    0x12 -> 6,    // INQUIRY
    0x15 -> 6,    // MODE SELECT
    0x1A -> 6,    // MODE SENSE
    // group 1 ten-byte commands
    0x25 -> 10,   // READ CAPACITY
    0x28 -> 10,   // READ
    0x2A -> 10,   // WRITE
    0x3C -> 10    // READ BUFFER
  )

  override def identifyCommandLen(cmd:Int): Option[Int] = commandLenMap.get(cmd)
  override def executeCommand(cmd:Array[Int]): Option[SCSITargetResponse] =
    import SCSITargetResponse.*
    //println(s"SCSI executing command: ${cmd.map(b => "%02X".format(b)).mkString(" ")}")
    cmd(0) match
      case 0x00 => // UNIT TEST
        Some(Status(STATUS_GOOD))
      case 0x03 => // REQUEST SENSE
        val allocationLength = cmd(4)
        val data = Array.ofDim[Byte](allocationLength)
        data(0) = 0x70
        Some(DataIn(data))
      /*
        +=====-========-========-========-========-========-========-========-========+
        |  Bit|   7    |   6    |   5    |   4    |   3    |   2    |   1    |   0    |
        |Byte |        |        |        |        |        |        |        |        |
        |=====+=======================================================================|
        | 0   |                           Operation code (04h)                        |
        |-----+-----------------------------------------------------------------------|
        | 1   | Logical unit number      | FmtData| CmpLst |   Defect list format     |
        |-----+-----------------------------------------------------------------------|
        | 2   |                           Vendor-specific                             |
        |-----+-----------------------------------------------------------------------|
        | 3   | (MSB)                                                                 |
        |-----+---                        Interleave                               ---|
        | 4   |                                                                 (LSB) |
        |-----+-----------------------------------------------------------------------|
        | 5   |                           Control                                     |
        +=============================================================================+
      */
      case 0x04 =>
        Some(Status(STATUS_GOOD))
      /* READ (6)
        +=====-========-========-========-========-========-========-========-========+
        |  Bit|   7    |   6    |   5    |   4    |   3    |   2    |   1    |   0    |
        |Byte |        |        |        |        |        |        |        |        |
        |=====+=======================================================================|
        | 0   |                           Operation code (08h)                        |
        |-----+-----------------------------------------------------------------------|
        | 1   | Logical unit number      | (MSB)                                      |
        |-----+------------------------------                                      ---|
        | 2   |                           Logical block address                       |
        |-----+---                                                                 ---|
        | 3   |                                                                 (LSB) |
        |-----+-----------------------------------------------------------------------|
        | 4   |                           Transfer length                             |
        |-----+-----------------------------------------------------------------------|
        | 5   |                           Control                                     |
        +=============================================================================+
      */
      case 0x08 =>
        val logicalBlockAddress = (cmd(1) & 0x1F) << 16 | cmd(2) << 8 | cmd(3)
        var transferLen = cmd(4)
        if transferLen == 0 then transferLen = 256
        readBlocks(logicalBlockAddress, transferLen) match
          case Some(data) =>
            Some(DataIn(data))
          case None =>
            Some(Status(STATUS_CHECK_CONDITION))
      /* READ (10)
        +=====-========-========-========-========-========-========-========-========+
        |  Bit|   7    |   6    |   5    |   4    |   3    |   2    |   1    |   0    |
        |Byte |        |        |        |        |        |        |        |        |
        |=====+=======================================================================|
        | 0   |                           Operation code (28h)                        |
        |-----+-----------------------------------------------------------------------|
        | 1   |   Logical unit number    |   DPO  |   FUA  |     Reserved    | RelAdr |
        |-----+-----------------------------------------------------------------------|
        | 2   | (MSB)                                                                 |
        |-----+---                                                                 ---|
        | 3   |                                                                       |
        |-----+---                        Logical block address                    ---|
        | 4   |                                                                       |
        |-----+---                                                                 ---|
        | 5   |                                                                 (LSB) |
        |-----+-----------------------------------------------------------------------|
        | 6   |                           Reserved                                    |
        |-----+-----------------------------------------------------------------------|
        | 7   | (MSB)                                                                 |
        |-----+---                        Transfer length                             |
        | 8   |                                                                 (LSB) |
        |-----+-----------------------------------------------------------------------|
        | 9   |                           Control                                     |
        +=============================================================================+
      */
      case 0x28 =>
        val logicalBlockAddress = cmd(2) << 24 | cmd(3) << 16 | cmd(4) << 8 | cmd(5)
        val transferLen = cmd(7) << 8 | cmd(8)
        readBlocks(logicalBlockAddress, transferLen) match
          case Some(data) =>
            Some(DataIn(data))
          case None =>
            Some(Status(STATUS_CHECK_CONDITION))
      /* WRITE (6)
        +=====-========-========-========-========-========-========-========-========+
        |  Bit|   7    |   6    |   5    |   4    |   3    |   2    |   1    |   0    |
        |Byte |        |        |        |        |        |        |        |        |
        |=====+=======================================================================|
        | 0   |                           Operation code (0Ah)                        |
        |-----+-----------------------------------------------------------------------|
        | 1   | Logical unit number      | (MSB)                                      |
        |-----+------------------------------                                      ---|
        | 2   |                           Logical block address                       |
        |-----+---                                                                 ---|
        | 3   |                                                                 (LSB) |
        |-----+-----------------------------------------------------------------------|
        | 4   |                           Transfer length                             |
        |-----+-----------------------------------------------------------------------|
        | 5   |                           Control                                     |
        +=============================================================================+
      */
      case 0x0A => // WRITE
        val logicalBlockAddress = (cmd(1) & 0x1F) << 16 | cmd(2) << 8 | cmd(3)
        var transferLen = cmd(4)
        if transferLen == 0 then transferLen = 256
        Some(DataOut(transferLen * BLOCK_SIZE,data => 
          if writeBlocks(logicalBlockAddress,data) then STATUS_GOOD
          else STATUS_CHECK_CONDITION)
        )
      /* WRITE (10)
        +=====-========-========-========-========-========-========-========-========+
        |  Bit|   7    |   6    |   5    |   4    |   3    |   2    |   1    |   0    |
        |Byte |        |        |        |        |        |        |        |        |
        |=====+=======================================================================|
        | 0   |                           Operation code (2Ah)                        |
        |-----+-----------------------------------------------------------------------|
        | 1   | Logical unit number      |   DPO  |   FUA  |Reserved|Reserved| RelAdr |
        |-----+-----------------------------------------------------------------------|
        | 2   | (MSB)                                                                 |
        |-----+---                                                                 ---|
        | 3   |                                                                       |
        |-----+---                        Logical block address                    ---|
        | 4   |                                                                       |
        |-----+---                                                                 ---|
        | 5   |                                                                 (LSB) |
        |-----+-----------------------------------------------------------------------|
        | 6   |                           Reserved                                    |
        |-----+-----------------------------------------------------------------------|
        | 7   | (MSB)                                                                 |
        |-----+---                        Transfer length                             |
        | 8   |                                                                 (LSB) |
        |-----+-----------------------------------------------------------------------|
        | 9   |                           Control                                     |
        +=============================================================================+
      */
      case 0x2A =>
        val logicalBlockAddress = cmd(2) << 24 | cmd(3) << 16 | cmd(4) << 8 | cmd(5)
        val transferLen = cmd(7) << 8 | cmd(8)
        Some(DataOut(transferLen * BLOCK_SIZE,data =>
          if writeBlocks(logicalBlockAddress,data) then STATUS_GOOD
          else STATUS_CHECK_CONDITION)
        )
      /* INQUIRY
        +=====-========-========-========-========-========-========-========-========+
        |  Bit|   7    |   6    |   5    |   4    |   3    |   2    |   1    |   0    |
        |Byte |        |        |        |        |        |        |        |        |
        |=====+=======================================================================|
        | 0   |                           Operation Code (12h)                        |
        |-----+-----------------------------------------------------------------------|
        | 1   | Logical Unit Number      |                  Reserved         |  EVPD  |
        |-----+-----------------------------------------------------------------------|
        | 2   |                           Page Code                                   |
        |-----+-----------------------------------------------------------------------|
        | 3   |                           Reserved                                    |
        |-----+-----------------------------------------------------------------------|
        | 4   |                           Allocation Length                           |
        |-----+-----------------------------------------------------------------------|
        | 5   |                           Control                                     |
        +=============================================================================+
        Standard INQUIRY Data Format
        +=====-========-========-========-========-========-========-========-========+
        |  Bit|   7    |   6    |   5    |   4    |   3    |   2    |   1    |   0    |
        |Byte |        |        |        |        |        |        |        |        |
        |=====+==========================+============================================|
        | 0   | Peripheral Qualifier     |           Peripheral Device Type           |
        |-----+-----------------------------------------------------------------------|
        | 1   |  RMB   |                  Device-Type Modifier                        |
        |-----+-----------------------------------------------------------------------|
        | 2   |   ISO Version   |       ECMA Version       |  ANSI-Approved Version   |
        |-----+-----------------+-----------------------------------------------------|
        | 3   |  AENC  | TrmIOP |     Reserved    |         Response Data Format      |
        |-----+-----------------------------------------------------------------------|
        | 4   |                           Additional Length (n-4)                     |
        |-----+-----------------------------------------------------------------------|
        | 5   |                           Reserved                                    |
        |-----+-----------------------------------------------------------------------|
        | 6   |                           Reserved                                    |
        |-----+-----------------------------------------------------------------------|
        | 7   | RelAdr | WBus32 | WBus16 |  Sync  | Linked |Reserved| CmdQue | SftRe  |
        |-----+-----------------------------------------------------------------------|
        | 8   | (MSB)                                                                 |
        |- - -+---                        Vendor Identification                    ---|
        | 15  |                                                                 (LSB) |
        |-----+-----------------------------------------------------------------------|
        | 16  | (MSB)                                                                 |
        |- - -+---                        Product Identification                   ---|
        | 31  |                                                                 (LSB) |
        |-----+-----------------------------------------------------------------------|
        | 32  | (MSB)                                                                 |
        |- - -+---                        Product Revision Level                   ---|
        | 35  |                                                                 (LSB) |
        |-----+-----------------------------------------------------------------------|
        | 36  |                                                                       |
        |- - -+---                        Vendor Specific                          ---|
        | 55  |                                                                       |
        |-----+-----------------------------------------------------------------------|
        | 56  |                                                                       |
        |- - -+---                        Reserved                                 ---|
        | 95  |                                                                       |
        |=====+=======================================================================|
        |     |                       Vendor-Specific Parameters                      |
        |=====+=======================================================================|
        | 96  |                                                                       |
        |- - -+---                        Vendor Specific                          ---|
        | n   |                                                                       |
        +=============================================================================+
      */
      case 0x12 =>
        val allocationLength = cmd(4)
        val data = Array.ofDim[Byte](36)
        data(0) = 0 // Direct access block device
        data(4) = (data.length - 4).toByte // The ADDITIONAL LENGTH field indicates the length in bytes of the remaining standard INQUIRY data
        // The T10 VENDOR IDENTIFICATION field contains eight bytes of left-aligned ASCII data identifying the vendor of the product.
        System.arraycopy(VENDOR,0,data,8,VENDOR.length)
        // The PRODUCT IDENTIFICATION field contains sixteen bytes of left-aligned ASCII data
        System.arraycopy(PRODUCT_ID,0,data,16,PRODUCT_ID.length)
        // The PRODUCT REVISION LEVEL field contains four bytes of left-aligned ASCII data
        System.arraycopy(VERSION,0,data,32,4)
        Some(DataIn(data))
      case 0x1A => // MODE SENSE
        val pageCode = cmd(2) & 0x3F
        val allocationLength = cmd(4)
        val data = Array.ofDim[Byte](allocationLength)
        data(0) = pageCode.toByte
        // the following codes are managed just to make HD SC Setup happy
        pageCode match
          case 0x01 => //  Read-Write Error Recovery mode page (01h)
            data(1) = (data.length - 2).toByte
            Some(DataIn(data))
          case 0x03 => // Format Device mode page
            data(1) = (data.length - 2).toByte
            Some(DataIn(data))
          case 0x30 => // Non standard mode page
            data(1) = (data.length - 2).toByte
            System.arraycopy(MODE_SENSE_30,0,data,14,MODE_SENSE_30.length)
            Some(DataIn(data))
          case _ =>
            println(s"Unrecognized page mode $pageCode")
            None
      case 0x3C => // READ BUFFER
        val allocationLen = cmd(6) << 16 | cmd(7) << 8 | cmd(8)
        // returns an empty buffer
        // bytes 1-3 are the buffer capacity => 0
        val data = Array.ofDim[Byte](allocationLen)
        Some(DataIn(data))
      /* READ CAPACITY
        +=====-========-========-========-========-========-========-========-========+
        |  Bit|   7    |   6    |   5    |   4    |   3    |   2    |   1    |   0    |
        |Byte |        |        |        |        |        |        |        |        |
        |=====+=======================================================================|
        | 0   |                           Operation code (25h)                        |
        |-----+-----------------------------------------------------------------------|
        | 1   | Logical unit number      |             Reserved              | RelAdr |
        |-----+-----------------------------------------------------------------------|
        | 2   | (MSB)                                                                 |
        |-----+---                                                                 ---|
        | 3   |                                                                       |
        |-----+---                        Logical block address                    ---|
        | 4   |                                                                       |
        |-----+---                                                                 ---|
        | 5   |                                                                 (LSB) |
        |-----+-----------------------------------------------------------------------|
        | 6   |                           Reserved                                    |
        |-----+-----------------------------------------------------------------------|
        | 7   |                           Reserved                                    |
        |-----+-----------------------------------------------------------------------|
        | 8   |                           Reserved                           |  PMI   |
        |-----+-----------------------------------------------------------------------|
        | 9   |                           Control                                     |
        +=============================================================================+
         READ CAPACITY DATA
        +=====-========-========-========-========-========-========-========-========+
        |  Bit|   7    |   6    |   5    |   4    |   3    |   2    |   1    |   0    |
        |Byte |        |        |        |        |        |        |        |        |
        |=====+=======================================================================|
        | 0   | (MSB)                                                                 |
        |- - -+---                        Returned logical block address           ---|
        | 3   |                                                                 (LSB) |
        |-----+-----------------------------------------------------------------------|
        | 4   | (MSB)                                                                 |
        |- - -+---                        Block length In bytes                    ---|
        | 7   |                                                                 (LSB) |
        +=============================================================================+
      */
      case 0x25 =>
        val data = Array.ofDim[Byte](8)
        val returnedLogicalBlockAddress = sizeInBytes / BLOCK_SIZE - 1
        data(0) = (returnedLogicalBlockAddress >> 24).toByte
        data(1) = (returnedLogicalBlockAddress >> 16).toByte
        data(2) = (returnedLogicalBlockAddress >> 8).toByte
        data(3) = (returnedLogicalBlockAddress & 0xFF).toByte
        data(4) = (BLOCK_SIZE >> 24).toByte
        data(5) = (BLOCK_SIZE >> 16).toByte
        data(6) = (BLOCK_SIZE >> 8).toByte
        data(7) = (BLOCK_SIZE & 0xFF).toByte
        Some(DataIn(data))
      case 0x15 =>
        val parameterListLen = cmd(4)
        //val data = Array.ofDim[Byte](parameterListLen)
        Some(DataOut(parameterListLen,data => {
          //println(s"MODE SELECT data: ${data.map(b => "%02X".format(b)).mkString(" ")}")
          // TODO what to do with these bytes ?
          STATUS_GOOD
        }))
      case _ =>
        println("Command not yet implemented: %02X".format(cmd(0)))
        None

  def readBlocks(block:Int,size:Int): Option[Array[Byte]]
  def writeBlocks(block:Int,data:Array[Byte]): Boolean

end DirectAccessSCSITarget