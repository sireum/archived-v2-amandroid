package org.sireum.amandroid

import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid.interProcedural.objectFlowAnalysis.NormalValueSet
import org.sireum.util._

abstract class Instance{
  def typ : Type
  def getType = typ
  def defSite : Context
  def getDefSite = defSite
  def clone(newDefSite : Context) : Instance
  var fieldsUnknownDefSites : ISet[Context] = isetEmpty
  def addFieldsUnknownDefSite(defSite : Context) = this.fieldsUnknownDefSites += defSite
  def setFieldsUnknownDefSites(defSites : ISet[Context]) = this.fieldsUnknownDefSites = defSites
  def getFieldsUnknownDefSites = this.fieldsUnknownDefSites
}

final case class PTAInstance(typ : Type, defSite : Context) extends Instance{
  override def clone(newDefSite : Context) : Instance = PTAInstance(typ, newDefSite)
  override def toString : String = "PTAInst(name:" + this.typ + ".defsite:" + this.defSite + ")"
}


final case class ClassInstance(name: String, defSite : Context) extends Instance{
  override def clone(newDefSite : Context) : Instance = ClassInstance(name, newDefSite)
  def typ = NormalType("[|java:lang:Class|]", 0)
  def getName = name
  override def toString : String = "ClassInst(classname:" + this.name + ".defsite:" + this.defSite + ")"
}

final case class NullInstance(defSite : Context) extends Instance{
  override def clone(newDefSite : Context) : Instance = NullInstance(newDefSite)
  def typ : Type = new NullType
  override def toString : String = "Null" + "@" + defSite
}

final case class UnknownInstance(defSite : Context) extends Instance{
  override def clone(newDefSite : Context) : Instance = UnknownInstance(newDefSite)
  def typ : Type = new UnknownType
  override def toString : String = "Unknown" + "@" + defSite
}