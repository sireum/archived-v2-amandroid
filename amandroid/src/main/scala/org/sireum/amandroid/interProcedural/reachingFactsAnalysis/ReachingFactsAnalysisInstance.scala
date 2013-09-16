package org.sireum.amandroid.interProcedural.reachingFactsAnalysis

import org.sireum.amandroid.Instance
import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid.Type
import org.sireum.amandroid.NormalType

abstract class RFAAbstractInstance(typ : Type, defSite : Context) extends Instance(typ, defSite)

final case class RFAInstance(typ : Type, defSite : Context) extends RFAAbstractInstance(typ, defSite){
  override def toString : String = {
    val sb = new StringBuilder
    sb.append("<")
    sb.append(this.typ + ".")
    sb.append(this.defSite.getCurrentLocUri)
    sb.append(">")
    sb.toString.intern()
  }
}

abstract class RFAAbstractStringInstance(typ : Type, defSite : Context) extends RFAAbstractInstance(typ, defSite){
  override def toString : String = "AbstractStringInstance(name:" + this.typ + ". defsite:" + this.defSite + ")"   
}

// RFAPointStringInstance represents a general String instance whose content can be any string i.e. reg expression "*"
final case class RFAPointStringInstance(typ : Type, defSite : Context) extends RFAAbstractStringInstance(typ, defSite){
  override def toString : String = "PointStringInstance(name:" + this.typ + ". defsite:" + this.defSite + ")" 
}

final case class RFAConcreteStringInstance(typ : Type, var string : String, defSite : Context) extends RFAAbstractStringInstance(typ, defSite){
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

