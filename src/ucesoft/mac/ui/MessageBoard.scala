package ucesoft.mac.ui

import java.awt.Color

/**
 * @author Alessandro Abbruzzetti
 *         Created on 27/12/2023 19:22  
 */
object MessageBoard:
  enum YPOS:
    case TOP, CENTER, BOTTOM

  enum XPOS:
    case LEFT, CENTER, RIGHT
  
  enum MessageLevel(val severity: Int):
    case ADMIN extends MessageLevel(1)
    case NORMAL extends MessageLevel(0)
    
    def accept(t:MessageLevel): Boolean =
      severity <= t.severity

  case class Message(text: String,
                     xpos: XPOS,
                     ypos: YPOS,
                     millis: Int,
                     color: Option[Color],
                     fadingMillis: Option[Int],
                     font: Option[String],
                     yoffset: Int,
                     bold:Boolean,
                     italic:Boolean,
                     messageLevel: MessageLevel)  
  
  trait MessageBoardListener:
    def addMessage(msg:Message): Unit
    def enableMessages(enabled: Boolean): Unit
    def setLevel(level:MessageLevel): Unit
    def interrupt(): Unit
    
  def builder: MessageBuilder = new MessageBuilder
    
  class MessageBuilder:
    private var text = ""
    private var xpos = XPOS.CENTER
    private var ypos = YPOS.CENTER
    private var color : Option[Color] = None
    private var fadingMillis : Option[Int] = None
    private var font : Option[String] = None
    private var delay = 0
    private var yoffset = 0
    private var boldFont = false
    private var italicFont = false
    private var messageType = MessageLevel.NORMAL
    
    def message(text:String): MessageBuilder =
      this.text = text ; this
    def xcenter(): MessageBuilder =
      xpos = XPOS.CENTER ; this
    def xleft(): MessageBuilder =
      xpos = XPOS.LEFT ; this
    def xright(): MessageBuilder =
      xpos = XPOS.RIGHT ; this
    def ycenter(): MessageBuilder =
      ypos = YPOS.CENTER;
      this
    def ytop(): MessageBuilder =
      ypos = YPOS.TOP
      this
    def ybottom(): MessageBuilder =
      ypos = YPOS.BOTTOM
      this
    def color(color:Color): MessageBuilder =
      this.color = Some(color) ; this
    def font(font:String): MessageBuilder =
      this.font = Some(font) ; this
    def fadingMilliseconds(millis:Int): MessageBuilder =
      this.fadingMillis = Some(millis) ; this  
    def delay(millis:Int = -1): MessageBuilder =
      this.delay = millis ; this
    def yoffset(offset:Int): MessageBuilder =
      yoffset = offset ; this
    def bold(): MessageBuilder =
      boldFont = true ; this
    def italic(): MessageBuilder =
      italicFont = true ; this
    def adminLevel(): MessageBuilder =
      messageType = MessageLevel.ADMIN ; this
    def build(): Message = Message(text,xpos,ypos,delay,color,fadingMillis,font,yoffset,boldFont,italicFont,messageType)
      
    
