package org.sireum.amandroid.objectFlowAnalysis

import org.sireum.util._

class NormalValueSet extends Cloneable{
  def copy : NormalValueSet = clone.asInstanceOf[NormalValueSet]
  protected var insts : Set[Instance] = Set()
  def instances = this.insts
  def addInstance(ins : Instance) = this.insts += (ins.copy)
  def addInstances(insts : Set[Instance]) = this.insts ++= insts.map{ins => ins.copy}
  def update(vs : NormalValueSet) = {
    vs.instances.foreach{
      ins =>
        if(!insts.contains(ins)){
          getSameInstance(ins) match{
            case Some(instance) => insts -= instance
            case None =>
          }
          insts += ins.copy
        }
    }
    this
  }
  protected def getSameInstance(ins : Instance) : Option[Instance] = {
    this.insts.foreach{
      instance =>
        if(instance.isSameInstance(ins) && instance.fieldLastDefSite == ins.fieldLastDefSite)return Some(instance)
    }
    None
  }
  def isEmpty() : Boolean = insts.isEmpty
  def getDiff(vsSucc : NormalValueSet) : NormalValueSet = {
    val d : NormalValueSet = new NormalValueSet
    d.addInstances(this.insts.diff(vsSucc.instances))
    d
  }
  def isStringInstanceType : Boolean = if(!instances.isEmpty)instances.head.isInstanceOf[StringInstance] else false
  def checkAndGetStrings : Option[Set[String]]= {
    if(isStringInstanceType) Some(this.instances.map{ins => ins.asInstanceOf[StringInstance].getStrings}.reduce((set1, set2) => set1 ++ set2))
    else None
  }
  protected def getMapDiff[K, V](map1 : Map[K, V], map2 : Map[K, V]) = {
    var d : Map[K, V] = Map()
    map1.keys.map{ case k => if(map2.contains(k)){if(!map1(k).equals(map2(k))){d += (k -> map1(k))}}else{d += (k -> map1(k))} }
    d
  }
  override def equals(a : Any) : Boolean = {
    a match{
      case vs : NormalValueSet => this.insts == vs.instances
      case _ => false
    }
  }
  override def hashCode() : Int = this.insts.hashCode
  override def toString() : String = "\n      ValueSet: \n        instances: " + insts + "\n"
}

abstract class Instance(className : String, defSite : Context) extends Cloneable{
  def copy : Instance = clone.asInstanceOf[Instance]
  def getClassName = className
  def getDefSite = defSite
  var fieldDefSiteRepo : Map[String, Map[Context, NormalValueSet]] = Map()
  var fieldLastDefSite : Map[String, Context] = Map()
  def setFieldLastDefSite(fieldName : String, defsitContext : Context) = fieldLastDefSite += (fieldName -> defsitContext)
  def updateFieldDefSite(fieldName : String, defsitContext : Context, vs : NormalValueSet) = {
    val defSiteMap = fieldDefSiteRepo.getOrElse(fieldName, Map()) + (defsitContext -> vs)
    fieldDefSiteRepo += (fieldName -> defSiteMap)
  }
  def getFieldValueSet(fieldName : String) : Option[NormalValueSet] = {
    if(fieldDefSiteRepo.contains(fieldName) && fieldLastDefSite.contains(fieldName) && fieldDefSiteRepo(fieldName).contains(fieldLastDefSite(fieldName)))
      Some(fieldDefSiteRepo(fieldName)(fieldLastDefSite(fieldName)))
    else None
  }
  def isSameInstance(ins : Instance) : Boolean = this.className == ins.getClassName && this.defSite == ins.getDefSite
  override def equals(a : Any) : Boolean = {
    a match{
      case ins : Instance => this.className == ins.getClassName && this.defSite == ins.getDefSite && this.fieldDefSiteRepo == ins.fieldDefSiteRepo
      case _ => false
    }
  }
  override def hashCode() : Int = (this.className + this.defSite + this.fieldDefSiteRepo).hashCode
  override def toString : String = "\n+++++++++++++++++++++\nInstance(\nname:" + this.className + ". \ndefsite:" + this.defSite + ". \nfieldLastDefSite:" + this.fieldLastDefSite + ". \nfieldDefRepo:" + this.fieldDefSiteRepo + ") \n-----------------------\n"
}

final case class StringInstance(className : String, defSite : Context) extends Instance(className, defSite){
  private var strings : Set[String] = Set()
  def getStrings = strings
  def addString(str : String) = strings += str
  def addStrings(strs : Set[String]) = strings ++= strs
  override def equals(a : Any) : Boolean = {
    a match{
      case ins : StringInstance => this.className == ins.getClassName && this.defSite == ins.getDefSite && this.fieldDefSiteRepo == ins.fieldDefSiteRepo && this.strings == ins.strings
      case _ => false
    }
  }
  override def hashCode() : Int = (this.className + this.defSite + this.fieldDefSiteRepo + this.strings).hashCode
  override def toString : String = "\n***********************\nStringInstance(\nstrings:" + this.strings + ". \nname:" + this.className + ". \ndefsite:" + this.defSite + ". \nfieldLastDefSite:" + this.fieldLastDefSite + ". \nfieldDefRepo:" + this.fieldDefSiteRepo + ") \n.............................\n"
}

final case class RegClassInstance(className : String, defSite : Context) extends Instance(className, defSite)