package org.sireum.amandroid.objectFlowAnalysis

import org.sireum.util._

class Context(k : Int) extends Cloneable {
  def copy : Context = clone.asInstanceOf[Context]
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
  override def toString = this.callStack.toString
}