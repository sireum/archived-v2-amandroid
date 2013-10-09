package org.sireum.amandroid.interProcedural.taintAnalysis

import org.sireum.alir.Slot

final case class VarSlot(varName : String) extends Slot{
  def isGlobal : Boolean = varName.startsWith("@@")
  override def toString = varName
}

final case class TAFact(s : Slot, v : Boolean)