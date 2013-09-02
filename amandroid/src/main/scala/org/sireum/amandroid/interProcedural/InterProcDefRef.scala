package org.sireum.amandroid.interProcedural

import org.sireum.alir.DefRef
import org.sireum.alir.Slot
import org.sireum.amandroid.Instance

final case class VarSlot(varName : String) extends Slot{
  override def toString = varName
}

final case class FieldSlot(ins : Instance, fieldName : String) extends Slot{
  override def toString = ins.toString + "." + fieldName
}

final case class ArraySlot(ins : Instance) extends Slot{
  override def toString = ins.toString
}

trait InterProcDefRef extends DefRef{
	
}