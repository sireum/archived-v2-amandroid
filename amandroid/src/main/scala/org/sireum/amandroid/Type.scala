package org.sireum.amandroid

abstract class Type {
	def typ : String
	def dimensions : Int
	def isArray : Boolean
	def name : String
}

final case class NormalType(val typ : String, val dimensions : Int) extends Type {
  def this(typ : String) = this(typ, 0)
  def isArray = dimensions > 0
  def name : String = {
    val sb = new StringBuilder
    sb.append(typ)
    for(i <- 0 to dimensions - 1) sb.append("[]")
    sb.toString.intern()
  }
  override def toString : String = {
    val sb = new StringBuilder
    sb.append("(" + typ)
    if(dimensions != 0) sb.append("," + dimensions)
    sb.append(")")
    sb.toString.intern()
  }
}

final case class TupleType(val left : Type, val right : Type) extends Type {
  def typ : String = {
    val sb = new StringBuilder
    sb.append("{" + left + "," + right + "}")
    sb.toString.intern()
  }
  def dimensions = 0
  def isArray = dimensions > 0
  def name : String = {
    val sb = new StringBuilder
    sb.append(typ)
    sb.toString.intern()
  }
  override def toString : String = {
    val sb = new StringBuilder
    sb.append("(" + typ)
    if(dimensions != 0) sb.append("," + dimensions)
    sb.append(")")
    sb.toString.intern()
  }
}

final case class NullType extends Type {
  def typ = "[|Null|]"
  def dimensions = 0
  def isArray = false
  def name : String = {
    val sb = new StringBuilder
    sb.append(typ)
    for(i <- 0 to dimensions - 1) sb.append("[]")
    sb.toString.intern()
  }
  override def toString : String = {
    val sb = new StringBuilder
    sb.append("(" + typ)
    sb.append(")")
    sb.toString.intern()
  }
}

final case class UnknownType extends Type {
  def typ = "[|Unknown|]"
  def dimensions = 0
  def isArray = false
  def name : String = {
    val sb = new StringBuilder
    sb.append(typ)
    for(i <- 0 to dimensions - 1) sb.append("[]")
    sb.toString.intern()
  }
  override def toString : String = {
    val sb = new StringBuilder
    sb.append("(" + typ)
    sb.append(")")
    sb.toString.intern()
  }
}