package org.sireum.amandroid.interProcedural.reachingFactsAnalysis

import org.sireum.amandroid.Instance
import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid.Type
import org.sireum.amandroid.NormalType
import org.sireum.amandroid.NullType

abstract class RFAAbstractInstance extends Instance

final case class RFAInstance(typ : Type, defSite : Context) extends RFAAbstractInstance{
  override def toString : String = {
    val sb = new StringBuilder
    sb.append("<")
    sb.append(this.typ + ".")
    sb.append(this.defSite.getCurrentLocUri)
    sb.append(">")
    sb.toString.intern()
  }
}

final case class RFANullInstance(defSite : Context) extends RFAAbstractInstance{
  def typ : Type = new NullType
  override def toString : String = "Null" + "@" + defSite
}

abstract class RFAAbstractStringInstance(defSite : Context) extends RFAAbstractInstance{
  def typ : Type = NormalType("[|java:lang:String|]", 0) 
  override def toString : String = "AbstractStringInstance(name:" + this.typ + ". defsite:" + this.defSite + ")"   
}

// RFAPointStringInstance represents a general String instance whose content can be any string i.e. reg expression "*"
final case class RFAPointStringInstance(defSite : Context) extends RFAAbstractStringInstance(defSite){
  override def toString : String = "PointStringInstance(name:" + this.typ + ". defsite:" + this.defSite + ")" 
}

final case class RFAConcreteStringInstance(string : String, defSite : Context) extends RFAAbstractStringInstance(defSite){
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

