package ucesoft.mac.storage

import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
 * @author Alessandro Abbruzzetti
 *         Created on 29/10/2024 18:19  
 */
class Track(private var bitLength:Int = Int.MaxValue):
  private val bitset = if bitLength == Int.MaxValue then new collection.mutable.BitSet else new collection.mutable.BitSet(bitLength)
  private var currentPos = 0
  private var modified = false
  private var mark = -1
  private var markReached = false

  def asArrayOfByte: Array[Byte] =
    var pos = 0
    var sh = 0
    var bit = 0
    val buffer = new ListBuffer[Byte]
    while pos < bitLength do
      sh <<= 1
      sh |= (if bitset(pos) then 1 else 0)
      pos += 1
      bit += 1
      if bit == 8 then
        buffer += sh.toByte
        sh = 0
        bit = 0
    if bit > 0 then
      while bit < 8 do
        sh <<= 1
        bit += 1
      buffer += sh.toByte
    buffer.toArray
  
  def setMark(mark:Int = -1): Unit = 
    if mark == -1 then this.mark = currentPos else this.mark = mark
    markReached = false
  def clearMark(): Unit = mark = -1
  def isMarkReached: Boolean = markReached
  
  def isModified: Boolean = modified

  def finish(): Unit =
    if bitLength == Int.MaxValue then
      bitLength = currentPos
    currentPos = 0
    modified = false

  def getPos: Int = currentPos
  def getPosInByte: Int = currentPos >> 3
  def getBitSize: Int = bitLength

  inline private def incPos(): Unit =
    currentPos += 1
    if currentPos == bitLength then
      currentPos = 0
    
    markReached |= currentPos == mark

  def get: 0|1 = if bitset(currentPos) then 1 else 0
  def getAndMoveOn: 0|1 =
    val bit : 0|1 = if bitset(currentPos) then 1 else 0
    incPos()
    bit
  def getNextByte: Int =
    var b = 0
    var sh = 0
    while b < 8 do
      sh <<= 1
      sh |= (if bitset(currentPos) then 1 else 0)
      incPos()
      b += 1
    sh
  // Set the current position to 1 if bit > 0 otherwise set current position to 0
  def setAndMoveOn(bit:Int): Unit =
    modified = true
    if bit == 0 then bitset -= currentPos else bitset += currentPos
    incPos()
  // Set current position to 1
  def setAndMoveOn(): Unit =
    modified = true
    bitset += currentPos
    incPos()
  // Set current position to 0
  def clearAndMoveOn(): Unit =
    modified = true
    bitset -= currentPos
    incPos()
  // Clear all bits
  def clearAll(): Unit =
    modified = true
    bitset.clear()
  // Set current position to the given one % bitLength
  def resetPositionTo(toBitPos:Int = 0): Unit =
    currentPos = toBitPos % bitLength
  // Set the given bits starting from the current position
  def setInt(b:Int, bits:Int = 8): Unit =
    modified = true
    var i = bits - 1
    while i >= 0 do
      val bitOn = (b & (1 << i)) != 0
      if bitOn then bitset += currentPos else bitset -= currentPos
      incPos()
      i -= 1
  // Set the given bits starting from the current position
  def setInts(bs:Array[Int], elementsBits:Int = 8): Unit =
    modified = true
    var i = 0
    while i < bs.length do
      setInt(bs(i),elementsBits)
      i += 1

  def dump(): Unit =
    for i <- 0 until bitLength do
      print(s"${if bitset(i) then 1 else 0} ")
    println()
    
  def fillRandom(): Unit =
    val rnd = new Random()
    bitset.clear()
    var b = 0
    currentPos = 0
    while b < bitLength do
      if rnd.nextBoolean() then setAndMoveOn() else clearAndMoveOn()
      b += 1
    currentPos = 0
