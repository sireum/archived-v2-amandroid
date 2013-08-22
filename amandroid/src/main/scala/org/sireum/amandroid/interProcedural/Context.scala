package org.sireum.amandroid.interProcedural

import org.sireum.util._

class Context(var k : Int){
  def copy : Context = {
    val clone = new Context(k)
    clone.callStack ++= this.callStack
    clone
  }
  def copy(c : Context) = this.callStack = c.getContext
  private var callStack : List[(ResourceUri, String)] = List()
  def length : Int = this.callStack.size
  def setContext(pUri : ResourceUri, loc : String) = {
    if(length <= k){
      callStack = (pUri, loc) :: callStack
    } else {
      callStack = (pUri, loc) :: callStack.dropRight(1)
    }
  }
  def updateContext(context2 : Context) = {
    this.k = context2.k
    val size2 = context2.length
    val callStack2 = context2.getContext
    val size = length
    if(size + size2 <= k + 1){
      callStack = callStack ::: callStack2
    } else {
      callStack = callStack ::: callStack2.dropRight(size2 - (k + 1 - size))
    }
  }
  def getContext : List[(ResourceUri, String)] = this.callStack
  def isDiff(c : Context) : Boolean = !this.callStack.equals(c.getContext)
  override def equals(a : Any) : Boolean = {
    if(a.isInstanceOf[Context]) this.callStack.equals(a.asInstanceOf[Context].getContext)
    else false
  }
  override def hashCode() = if (this.callStack == null) 0 else this.callStack.hashCode
  override def toString = {
    var string = ""
    this.callStack.foreach{
      case(uri, str) =>
        if(str.contains("%7C%5D"))
          string += "(" + uri.substring(uri.lastIndexOf(".") + 1, uri.lastIndexOf("%7C%5D")) + "," + str.substring(str.lastIndexOf(".") + 1, str.lastIndexOf("%7C%5D")) + ")"
        else
          string += "(" + uri.substring(uri.lastIndexOf(".") + 1, uri.lastIndexOf("%7C%5D")) + "," + str + ")"
    }
    string
  }
}