package org.sireum.amandroid

abstract class Type {
	def typ : String
}

class NormalType(val typ : String, dimensions : Int) extends Type {
  override def toString : String = "NormalType(" + typ + "," + dimensions + ")"
}