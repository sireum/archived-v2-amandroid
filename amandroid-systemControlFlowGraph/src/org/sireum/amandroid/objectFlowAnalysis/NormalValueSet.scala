package org.sireum.amandroid.objectFlowAnalysis

import org.sireum.util._

class NormalValueSet{
  def copy : NormalValueSet = {
    val clone = new NormalValueSet
    clone.addInstances(this.insts)
    clone
  }
  protected var insts : Set[Instance] = Set()
  def instances = this.insts
  def addInstance(ins : Instance) = this.insts += (ins.copy)
  def addInstances(insts : Set[Instance]) = this.insts ++= insts.map{ins => ins.copy}
  def removeInstance(ins : Instance) = this.insts -= (ins)
  def removeInstances(insts : Set[Instance]) = this.insts --= insts.map{ins => ins}
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
  
  def mergeInstance(ins : Instance) = {
    if(!insts.contains(ins)){
      getSameInstanceFromSamePred(ins) match{
        case Some(instance) => instance.merge(ins)
        case None => addInstance(ins)
      }
    }
  }
  
  protected def getSameInstanceFromSamePred(ins : Instance) : Option[Instance] = {
    var sins : Set[Instance]= Set()
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
  
  protected def isSameInstanceFromSamePred(ins1 : Instance, ins2 : Instance) : Boolean =
    ins1.isSameInstance(ins2) && ins1.fieldLastDefSite == ins2.fieldLastDefSite
  
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

abstract class Instance(className : String, defSite : Context){
  def copy : Instance
  def getClassName = className
  def getDefSite = defSite
  var fieldDefSiteRepo : Map[String, Map[Context, NormalValueSet]] = Map()
  var fieldLastDefSite : Map[String, Context] = Map()
  def setFieldLastDefSite(fieldName : String, defsitContext : Context) = fieldLastDefSite += (fieldName -> defsitContext)
  def updateFieldDefSite(fieldName : String, defsitContext : Context, vs : NormalValueSet) = {
    val defSiteMap = fieldDefSiteRepo.getOrElse(fieldName, Map()) + (defsitContext -> vs)
    fieldDefSiteRepo += (fieldName -> defSiteMap)
  }
  
  def merge(instance : Instance) = {
    if(isSameInstance(instance) && this.fieldLastDefSite == instance.fieldLastDefSite){
    	instance.fieldDefSiteRepo foreach{
    	  case (fieldName, map) =>
    	    this.fieldDefSiteRepo += (fieldName -> copyMap(map))
    	}
    }
    this
  }
  
  def copyMap(map : Map[Context, NormalValueSet]) : Map[Context, NormalValueSet] = {
    map map{
      case(k, v) =>
        val copyV = v.copy
      	(k.copy -> copyV) 
    }
  }
  
  def getFieldValueSet(fieldName : String) : Option[NormalValueSet] = {
    if(fieldDefSiteRepo.contains(fieldName) && fieldLastDefSite.contains(fieldName) && fieldDefSiteRepo(fieldName).contains(fieldLastDefSite(fieldName)))
      Some(fieldDefSiteRepo(fieldName)(fieldLastDefSite(fieldName)))
    else None
  }
  // get diff : map1 - map2
  protected def getMapDiff[K, V](map1 : Map[K, V], map2 : Map[K, V]) = {
    var d : Map[K, V] = Map()
    map1.keys.map{ case k => if(map2.contains(k)){if(!map1(k).equals(map2(k))){d += (k -> map1(k))}}else{d += (k -> map1(k))} }
    d
  }
  def getDiff(ins : Instance) : Instance = {
  	var className : String = null
  	var defSite : Context = null
    if(this.className != ins.getClassName) className = this.className
    if(this.defSite != ins.getDefSite) defSite = this.defSite
    val d = RegClassInstance(className, defSite)
    if(this.fieldDefSiteRepo != ins.fieldDefSiteRepo) d.fieldDefSiteRepo = getMapDiff(this.fieldDefSiteRepo, ins.fieldDefSiteRepo)
    if(this.fieldLastDefSite != ins.fieldLastDefSite) d.fieldLastDefSite = getMapDiff(this.fieldLastDefSite, ins.fieldLastDefSite)
    d
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

final case class StringInstance(className : String, var defSite : Context, var k : Int) extends Instance(className, defSite){
  override def copy : StringInstance  = {
    val clone = new StringInstance(className, defSite, k)
    this.fieldDefSiteRepo foreach{
  	  case (fieldName, map) =>
  	    clone.fieldDefSiteRepo += (fieldName -> copyMap(map))
  	}
    this.fieldLastDefSite.foreach{
      case (fieldName, context)=>
        clone.fieldLastDefSite += (fieldName -> context.copy)
    }
    clone.operationCalculator ++= this.operationCalculator
    clone.strings ++= this.strings
    clone
  }
  def setK(i : Int) = k = i
  private var strings : Set[String] = Set()
  def getStrings = strings
  def addString(str : String) = strings += str
  def addStrings(strs : Set[String], operation : Context) = 
    if(checkOperation(operation)){
      updateOperation(operation)
      strs foreach{str => addString(str)}
    }
  def setStrings(strs : Set[String], operation : Context) = 
    if(checkOperation(operation)){
      updateOperation(operation)
      this.strings = strs
    }
  private var operationCalculator : Map[Int, Int] = Map()
  def updateOperation(operation : Context) = {
    this.operationCalculator += (operation.hashCode -> (this.operationCalculator.getOrElse(operation.hashCode, 0) + 1))
  }
  def checkOperation(operation : Context) : Boolean = {
    if(this.operationCalculator.getOrElse(operation.hashCode, 0) < k) true
    else false
  }
  override def merge(instance : Instance) = {
    require(instance.isInstanceOf[StringInstance])
    if(isSameInstance(instance) && this.fieldLastDefSite == instance.fieldLastDefSite){
    	super.merge(instance)
    	instance.asInstanceOf[StringInstance].operationCalculator foreach{
    	  case(op, time) =>
    	    this.operationCalculator += (op -> (this.operationCalculator.getOrElse(op, 0) + time))
    	}
    	this.strings = instance.asInstanceOf[StringInstance].getStrings
    }
    this
  }
  override def equals(a : Any) : Boolean = {
    a match{
      case ins : StringInstance => this.className == ins.getClassName && this.defSite == ins.getDefSite && this.fieldDefSiteRepo == ins.fieldDefSiteRepo && this.strings == ins.strings
      case _ => false
    }
  }
  override def hashCode() : Int = (this.className + this.defSite + this.fieldDefSiteRepo + this.strings).hashCode
  override def toString : String = "\n***********************\nStringInstance(\nstrings:" + this.strings + ". \nname:" + this.className + ". \ndefsite:" + this.defSite + ". \nfieldLastDefSite:" + this.fieldLastDefSite + ". \nfieldDefRepo:" + this.fieldDefSiteRepo + ") \n.............................\n"
}

final case class RegClassInstance(className : String, defSite : Context) extends Instance(className, defSite){
  override def copy : RegClassInstance  = {
    val clone = new RegClassInstance(className, defSite)
    this.fieldDefSiteRepo foreach{
  	  case (fieldName, map) =>
  	    clone.fieldDefSiteRepo += (fieldName -> copyMap(map))
  	}
    this.fieldLastDefSite.foreach{
      case (fieldName, context)=>
        clone.fieldLastDefSite += (fieldName -> context.copy)
    }
    clone
  }
}