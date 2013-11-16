package org.sireum.amandroid.interProcedural.objectFlowAnalysis

import org.sireum.util._
import org.sireum.amandroid.interProcedural._

class NormalValueSet{
  def copy : NormalValueSet = {
    val clone = new NormalValueSet
    clone.addInstances(this.insts)
    clone
  }
  protected var insts : Set[OFAInstance] = Set()
  def instances = this.insts
  def addInstance(ins : OFAInstance) = this.insts += ins.copy
  def addInstances(insts : Set[OFAInstance]) = this.insts ++= insts.map{ins => ins.copy}
  def removeInstance(ins : OFAInstance) = this.insts -= (ins)
  def removeInstances(insts : Set[OFAInstance]) = this.insts --= insts.map{ins => ins}
  /**
   * merge valueset with predecessor's valueset vs
   */
  def merge(vs : NormalValueSet) = {
    vs.instances.foreach{
      ins =>
        mergeInstance(ins)
    }
    this
  }
  
  def mergeInstance(ins : OFAInstance) = {
    if(!insts.contains(ins)){
      getSameInstanceFromSamePred(ins) match{
        case Some(instance) => instance.merge(ins)
        case None => addInstance(ins)
      }
    }
  }
  
  protected def getSameInstanceFromSamePred(ins : OFAInstance) : Option[OFAInstance] = {
    var sins : Set[OFAInstance]= Set()
    this.insts.foreach{
      instance =>
        if(isSameInstanceFromSamePred(instance, ins))sins += instance
    }
    require(sins.size <= 1)
    this.insts.foreach{
      instance =>
        if(isSameInstanceFromSamePred(instance, ins))return Some(instance)
    }
    None
  }
  
  protected def isSameInstanceFromSamePred(ins1 : OFAInstance, ins2 : OFAInstance) : Boolean =
    ins1.isSameInstance(ins2) && ins1.fieldLastDefSite == ins2.fieldLastDefSite
  
  def isEmpty() : Boolean = insts.isEmpty
  def getDiff(vsSucc : NormalValueSet) : NormalValueSet = {
    val d : NormalValueSet = new NormalValueSet
    d.addInstances(this.insts.diff(vsSucc.instances))
    d
  }
  def isStringInstanceType : Boolean = if(!instances.isEmpty)instances.head.isInstanceOf[OFAStringInstance] else false
  def checkAndGetStrings : Option[Set[String]]= {
    if(isStringInstanceType) Some(this.instances.map{ins => ins.asInstanceOf[OFAStringInstance].getStrings}.reduce((set1, set2) => set1 ++ set2))
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

