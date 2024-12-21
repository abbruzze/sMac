package ucesoft.mac

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import scala.reflect.ClassTag

object StateBuilder:
  class StateBuilderException(msg:String,cause:Throwable = null) extends IllegalArgumentException(msg,cause):
    private var componentPath : List[String] = Nil
    def addPath(p:String): Unit = componentPath ::= p
    def getComponentPath: String = componentPath.mkString(".")
    
  sealed trait StateSimpleType[T]

  object StateSimpleType:
    given stringReadType: StateSimpleType[String] = new StateSimpleType[String] {}
    given byteReadType: StateSimpleType[Byte] = new StateSimpleType[Byte] {}
    given intReadType: StateSimpleType[Int] = new StateSimpleType[Int] {}
    given shortReadType: StateSimpleType[Short] = new StateSimpleType[Short] {}
    given longReadType: StateSimpleType[Long] = new StateSimpleType[Long] {}
    given floatReadType: StateSimpleType[Float] = new StateSimpleType[Float] {}
    given doubleReadType: StateSimpleType[Double] = new StateSimpleType[Double] {}
    given boolReadType: StateSimpleType[Boolean] = new StateSimpleType[Boolean] {}
/**
 * @author Alessandro Abbruzzetti
 *         Created on 12/02/2024 10:51
 */
class StateBuilder(map:java.util.Map[String,AnyRef] = new java.util.LinkedHashMap[String,AnyRef]):
  import StateBuilder.*

  private inline val STRING_CLASS = classOf[String]
  private inline val BYTE_CLASS = classOf[Byte]
  private inline val SHORT_CLASS = classOf[Short]
  private inline val INT_CLASS = classOf[Int]
  private inline val LONG_CLASS = classOf[Long]
  private inline val FLOAT_CLASS = classOf[Float]
  private inline val DOUBLE_CLASS = classOf[Double]
  private inline val BOOLEAN_CLASS = classOf[Boolean]

  private def checkNewName(n:String): Unit =
    if map.containsKey(n) then
      throw new StateBuilderException(s"Key $n already exists")

  private def checkName(n: String): Unit =
    if !map.containsKey(n) then
      throw new StateBuilderException(s"Key $n does not exist")
  private def guard[U](n:String)(v : => U): U =
    try
      v
    catch
      case t:Throwable =>
        throw new StateBuilderException(s"State error on attribute '$n': $t")

  def build(): java.util.Map[String,AnyRef] = map

  def w(n:String,b: Byte): StateBuilder =
    checkNewName(n)
    map.put(n,java.lang.Byte.valueOf(b))
    this
  def w(n: String, s: Short): StateBuilder =
    checkNewName(n)
    map.put(n, java.lang.Short.valueOf(s))
    this
  def w(n: String, i: Int): StateBuilder =
    checkNewName(n)
    map.put(n, java.lang.Integer.valueOf(i))
    this
  def w(n: String, l: Long): StateBuilder =
    checkNewName(n)
    map.put(n, l.toString) // Long is not supported in JSON
    this
  def w(n: String, f: Float): StateBuilder =
    checkNewName(n)
    map.put(n, java.lang.Float.valueOf(f))
    this
  def w(n: String, d: Double): StateBuilder =
    checkNewName(n)
    map.put(n, java.lang.Double.valueOf(d))
    this
  def w(n: String, b: Boolean): StateBuilder =
    checkNewName(n)
    map.put(n, java.lang.Boolean.valueOf(b))
    this
  def w(n: String, s: String): StateBuilder =
    checkNewName(n)
    map.put(n, s)
    this

  def w(n: String, m: java.util.Map[String,AnyRef]): StateBuilder =
    checkNewName(n)
    map.put(n, m)
    this
  def w[T](n: String, a: Array[T])(using ct: ClassTag[T], rt: StateSimpleType[T]): StateBuilder =
    checkNewName(n)
    map.put(n, a)
    this

  def w[T](n: String, a: Array[Array[T]])(using ct: ClassTag[T], rt: StateSimpleType[T]): StateBuilder =
    checkNewName(n)
    map.put(n, a)
    this

  def w[T](n: String, a: Array[Array[Array[T]]])(using ct: ClassTag[T], rt: StateSimpleType[T]): StateBuilder =
    checkNewName(n)
    map.put(n, a)
    this
  def serialize(n: String,a: AnyRef,zip:Boolean): StateBuilder =
    checkNewName(n)
    val buffer = new ByteArrayOutputStream()
    val stream = if zip then new ObjectOutputStream(new GZIPOutputStream(buffer)) else new ObjectOutputStream(buffer)
    stream.writeObject(a)
    stream.close()
    map.put(n, buffer.toByteArray)
    this
  // ==================================================
  def hasAttr(name:String): Boolean =
    map.containsKey(name)
  def subStateBuilder(n:String): Option[StateBuilder] =
    Option(map.get(n)) match
      case Some(m:java.util.Map[_,_]) =>
        Some(new StateBuilder(m.asInstanceOf[java.util.Map[String,AnyRef]]))
      case _ =>
        None
  def getSubStateBuilder(n:String): StateBuilder =
    subStateBuilder(n) match
      case Some(b) => b
      case None =>
        throw new StateBuilderException(s"Cannot find $n attribute to create sub-state builder")
  def deserialize[T](n:String,zip:Boolean): T =
    checkName(n)
    guard(n) {
      val buffer = new ByteArrayInputStream(map.get(n).asInstanceOf[Array[Byte]])
      val stream = if zip then new ObjectInputStream(new GZIPInputStream(buffer)) else new ObjectInputStream(buffer)
      stream.readObject().asInstanceOf[T]
    }

  def r[T](n:String)(using ct: ClassTag[T], rt: StateSimpleType[T]): T =
    checkName(n)
    guard(n) {
      val obj = map.get(n)
      ct.runtimeClass match
        case STRING_CLASS => obj.asInstanceOf[T]
        case BYTE_CLASS => obj.asInstanceOf[java.lang.Byte].byteValue().asInstanceOf[T]
        case SHORT_CLASS => obj.asInstanceOf[java.lang.Short].shortValue().asInstanceOf[T]
        case INT_CLASS => obj.asInstanceOf[java.lang.Integer].intValue().asInstanceOf[T]
        case LONG_CLASS => obj.toString.toLong.asInstanceOf[T]
        case FLOAT_CLASS => obj.asInstanceOf[java.lang.Float].floatValue().asInstanceOf[T]
        case DOUBLE_CLASS => obj.asInstanceOf[java.lang.Double].doubleValue().asInstanceOf[T]
        case BOOLEAN_CLASS => obj.asInstanceOf[java.lang.Boolean].booleanValue().asInstanceOf[T]
    }

  private def copyArrays[T](dst:Array[Array[T]],src:java.util.ArrayList[java.util.ArrayList[T]]): Boolean =
    if dst.length == src.size then
      var c = 0
      while c < dst.length do
        if dst(c).length != src.get(c).size() then return false
        var j = 0
        val src2 = src.get(c)
        while j < dst(c).length do
          dst(c)(j) = src2.get(j)
          j += 1
        c += 1
      true
    else
      false
  def r[T](n:String,target:Array[T])(using ct: ClassTag[T], rt: StateSimpleType[T]): Unit =
    checkName(n)
    guard(n) {
      map.get(n) match
        case a: java.util.ArrayList[T] if a.size == target.length =>
          var c = 0
          while c < target.length do
            target(c) = a.get(c)
            c += 1
        case _ =>
          throw StateBuilderException(s"State reading error: invalid array size or type for attribute '$n'")
    }

  def r[T](n: String, target: Array[Array[T]])(using ct: ClassTag[T], rt: StateSimpleType[T]): Unit =
    checkName(n)
    guard(n) {
      map.get(n) match
        case a: java.util.ArrayList[java.util.ArrayList[T]] if a.size == target.length =>
          for i <- target.indices do
            if !copyArrays(target,a) then
              throw StateBuilderException(s"State reading error: invalid array sub-size for attribute '$n'")
        case _ =>
          throw StateBuilderException(s"State reading error: invalid array size or type for attribute '$n'")
    }

  def r[T](n: String, target: Array[Array[Array[T]]])(using ct: ClassTag[T], rt: StateSimpleType[T]): Unit =
    checkName(n)
    guard(n) {
      map.get(n) match
        case a: java.util.ArrayList[java.util.ArrayList[java.util.ArrayList[T]]] if a.size == target.length =>
          for i <- target.indices do
            val dst = target(i)
            val src = a.get(i)
            if !copyArrays(dst,src) then
              throw StateBuilderException(s"State reading error: invalid array sub-size for attribute '$n'")
        case _ =>
          throw StateBuilderException(s"State reading error: invalid array size or type for attribute '$n'")
    }
