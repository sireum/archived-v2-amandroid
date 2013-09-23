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

final case class RFAFact(s : Slot, v : Instance)