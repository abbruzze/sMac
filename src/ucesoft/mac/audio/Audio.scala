package ucesoft.mac.audio

import ucesoft.mac.{MACComponent, MacModel}

/**
 * @author Alessandro Abbruzzetti
 *         Created on 18/11/2024 18:51  
 */
trait Audio extends MACComponent:
  def turn(on:Boolean): Unit
  def setVolumeLevel(level:Int): Unit
  def getVolumeLevel: Int
  def setAlternateAudioBuffer(alternateOn:Boolean): Unit
  def getAudioBufferAddress: Int
  def newSample(sample:Byte): Unit
