package org.sireum.amandroid

abstract class Type {
	def typ : String
	def dimensions : Int
	def isArray : Boolean
}

final case class NormalType(val typ : String, val dimensions : Int) extends Type {
  def this(typ : String) = this(typ, 0)
  def isArray = dimensions > 0
  override def toString : String = {
    val sb = new StringBuilder
    sb.append("(" + typ)
    if(dimensions != 0) sb.append("," + dimensions)
    sb.append(")")
    sb.toString.intern()
  }
}