package org.sireum.amandroid.interProcedural.reachingFactsAnalysis

import org.sireum.alir.DefRef
import org.sireum.alir.Slot
import org.sireum.amandroid.Instance
import org.sireum.util._

final case class VarSlot(varName : String) extends Slot{
  def isGlobal : Boolean = varName.startsWith("@@")
  override def toString = varName
}

abstract class HeapSlot(ins : Instance) extends Slot{
  def matchWithInstance(ins : Instance) : Boolean = this.ins == ins
}

final case class FieldSlot(ins : Instance, fieldName : String) extends HeapSlot(ins){
  override def toString = ins.toString + "." + fieldName
}

final case class ArraySlot(ins : Instance) extends HeapSlot(ins){
  override def toString = ins.toString
}

//class Value{
//  protected var insts : ISet[Instance] = isetEmpty
//  def setInstance(ins : Instance) = this.insts += ins
//  def setInstances(insts : Set[Instance]) = this.insts ++= insts
//  def getInstances = this.insts
//  def update(v2 : Value) : Value = {
//    setInstances(v2.getInstances)
//    this
//  }
//  override def toString : String = {
//    val sb = new StringBuilder
//    insts.foreach{
//      ins=>
//        sb.append(ins)
//    }
//    sb.toString.intern()
//  }
//}