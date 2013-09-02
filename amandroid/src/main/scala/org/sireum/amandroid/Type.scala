package org.sireum.amandroid

abstract class Type {
	def typ : String
	def dimensions : Int
}

class NormalType(val typ : String, val dimensions : Int) extends Type {
  override def toString : String = "NormalType(" + typ + "," + dimensions + ")"
}