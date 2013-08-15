package org.sireum.amandroid.instance

import org.sireum.amandroid.contextProvider.Context
import org.sireum.amandroid.objectFlowAnalysis.NormalValueSet

abstract class Instance(className : String, defSite : Context){
  def getClassName = className
  def getDefSite = defSite
  override def toString : String = "Instance(name:" + this.className + ". defsite:" + this.defSite + ")"
}

final case class PTAInstance(className : String, defSite : Context) extends Instance(className, defSite){
  override def toString : String = "PTAInst(name:" + this.className + ".defsite:" + this.defSite + ")"
}

//final case class PTARegClassInstance(className : String, defSite : Context) extends PTAInstance(className, defSite){
//  override def toString : String = "PTARegClassInstance(name:" + this.className + ". defsite:" + this.defSite + ")"
//}
//
//final case class PTAStringInstance(className : String, var defSite : Context, var k : Int) extends PTAInstance(className, defSite){
//  def setK(i : Int) = k = i
//  private var strings : Set[String] = Set()
//  def getStrings = strings
//  def addString(str : String) = strings += str
//  def addStrings(strs : Set[String], operation : Context) = 
//    if(checkOperation(operation)){
//      updateOperation(operation)
//      strs foreach{str => addString(str)}
//    }
//  def setStrings(strs : Set[String], operation : Context) = 
//    if(checkOperation(operation)){
//      updateOperation(operation)
//      this.strings = strs
//    }
//  private var operationCalculator : Map[Int, Int] = Map()
//  def updateOperation(operation : Context) = {
//    this.operationCalculator += (operation.hashCode -> (this.operationCalculator.getOrElse(operation.hashCode, 0) + 1))
//  }
//  def checkOperation(operation : Context) : Boolean = {
//    if(this.operationCalculator.getOrElse(operation.hashCode, 0) < k) true
//    else false
//  }
//  override def toString : String = "PTAStringInstance(string:" + this.strings + ". name:" + this.className + ". defsite:" + this.defSite + ")"
//}

abstract class OFAInstance(className : String, defSite : Context) extends Instance(className, defSite){
  def copy : OFAInstance
  var fieldDefSiteRepo : Map[String, Map[Context, NormalValueSet]] = Map()
  var fieldLastDefSite : Map[String, Context] = Map()
  def setFieldLastDefSite(fieldName : String, defsitContext : Context) = fieldLastDefSite += (fieldName -> defsitContext)
  def getFieldLastDefSite(fieldName : String) = if(fieldLastDefSite.contains(fieldName)) Some(fieldLastDefSite(fieldName)) else None
  def updateFieldDefSite(fieldName : String, defsitContext : Context, vs : NormalValueSet) = {
    val defSiteMap = fieldDefSiteRepo.getOrElse(fieldName, Map()) + (defsitContext -> vs)
    fieldDefSiteRepo += (fieldName -> defSiteMap)
  }
  
  def merge(instance : OFAInstance) = {
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
  
  def isSameInstance(ins : OFAInstance) : Boolean = this.className == ins.getClassName && this.defSite == ins.getDefSite
  override def equals(a : Any) : Boolean = {
    a match{
      case ins : OFAInstance => this.className == ins.getClassName && this.defSite == ins.getDefSite && this.fieldDefSiteRepo == ins.fieldDefSiteRepo
      case _ => false
    }
  }
  override def hashCode() : Int = (this.className + this.defSite + this.fieldDefSiteRepo).hashCode
  override def toString : String = "\n+++++++++++++++++++++\nInstance(\nname:" + this.className + ". \ndefsite:" + this.defSite + ". \nfieldLastDefSite:" + this.fieldLastDefSite + ". \nfieldDefRepo:" + this.fieldDefSiteRepo + ") \n-----------------------\n"

}

final case class OFAStringInstance(className : String, var defSite : Context, var k : Int) extends OFAInstance(className, defSite){
  override def copy : OFAStringInstance  = {
    val clone = new OFAStringInstance(className, defSite, k)
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
  override def merge(instance : OFAInstance) = {
    require(instance.isInstanceOf[OFAStringInstance])
    if(isSameInstance(instance) && this.fieldLastDefSite == instance.fieldLastDefSite){
    	super.merge(instance)
    	instance.asInstanceOf[OFAStringInstance].operationCalculator foreach{
    	  case(op, time) =>
    	    this.operationCalculator += (op -> (this.operationCalculator.getOrElse(op, 0) + time))
    	}
    	this.strings = instance.asInstanceOf[OFAStringInstance].getStrings
    }
    this
  }
  override def equals(a : Any) : Boolean = {
    a match{
      case ins : OFAStringInstance => this.className == ins.getClassName && this.defSite == ins.getDefSite && this.fieldDefSiteRepo == ins.fieldDefSiteRepo && this.strings == ins.strings
      case _ => false
    }
  }
  override def hashCode() : Int = (this.className + this.defSite + this.fieldDefSiteRepo + this.strings).hashCode
  override def toString : String = "\n***********************\nStringInstance(\nstrings:" + this.strings + ". \nname:" + this.className + ". \ndefsite:" + this.defSite + ". \nfieldLastDefSite:" + this.fieldLastDefSite + ". \nfieldDefRepo:" + this.fieldDefSiteRepo + ") \n.............................\n"
}

final case class OFARegClassInstance(className : String, defSite : Context) extends OFAInstance(className, defSite){
  override def copy : OFARegClassInstance  = {
    val clone = new OFARegClassInstance(className, defSite)
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
  
  def getDiff(ins : OFAInstance) : OFAInstance = {
  	var className : String = null
  	var defSite : Context = null
    if(this.className != ins.getClassName) className = this.className
    if(this.defSite != ins.getDefSite) defSite = this.defSite
    val d = OFARegClassInstance(className, defSite)
    if(this.fieldDefSiteRepo != ins.fieldDefSiteRepo) d.fieldDefSiteRepo = getMapDiff(this.fieldDefSiteRepo, ins.fieldDefSiteRepo)
    if(this.fieldLastDefSite != ins.fieldLastDefSite) d.fieldLastDefSite = getMapDiff(this.fieldLastDefSite, ins.fieldLastDefSite)
    d
  }
}