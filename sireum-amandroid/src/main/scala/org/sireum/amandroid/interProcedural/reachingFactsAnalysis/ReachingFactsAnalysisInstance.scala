package org.sireum.amandroid.interProcedural.reachingFactsAnalysis

import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid._

abstract class RFAAbstractInstance extends Instance

final case class RFAInstance(typ : Type, defSite : Context) extends RFAAbstractInstance{
  override def clone(newDefSite : Context) : Instance = RFAInstance(typ, newDefSite)
  override def toString : String = {
    val sb = new StringBuilder
    sb.append("<")
    sb.append(this.typ + ".")
    sb.append(this.defSite.getCurrentLocUri)
    sb.append(">")
    sb.toString.intern()
  }
}

final case class RFATupleInstance(left : Instance, right : Instance, defSite : Context) extends RFAAbstractInstance{
  override def clone(newDefSite : Context) : Instance = RFATupleInstance(left, right, newDefSite)
  def typ : Type = TupleType(left.typ, right.typ)
  override def toString : String = {
    val sb = new StringBuilder
    sb.append("<")
    sb.append(this.typ + ".")
    sb.append(this.defSite.getCurrentLocUri)
    sb.append(">")
    sb.toString.intern()
  }
}

abstract class RFAAbstractStringInstance(defSite : Context) extends RFAAbstractInstance{
  def typ : Type = NormalType("[|java:lang:String|]", 0) 
  override def toString : String = "AbstractStringInstance(name:" + this.typ + ". defsite:" + this.defSite + ")"   
}

// RFAPointStringInstance represents a general String instance whose content can be any string i.e. reg expression "*"
final case class RFAPointStringInstance(defSite : Context) extends RFAAbstractStringInstance(defSite){
  override def clone(newDefSite : Context) : Instance = RFAPointStringInstance(newDefSite)
  override def toString : String = "PointStringInstance(name:" + this.typ + ". defsite:" + this.defSite + ")" 
}

final case class RFAConcreteStringInstance(string : String, defSite : Context) extends RFAAbstractStringInstance(defSite){
  override def clone(newDefSite : Context) : Instance = RFAConcreteStringInstance(string, newDefSite)
  override def toString : String = {
    val sb = new StringBuilder
    sb.append("<")
    sb.append(this.typ + ".")
    sb.append("\"" + {if(this.string.length > 30)this.string.substring(0, 30) + ".." else this.string} + "\".")
    sb.append(this.defSite.getCurrentLocUri)
    sb.append(">")
    sb.toString.intern()
  }
}

