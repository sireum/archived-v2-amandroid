package org.sireum.amandroid.objectFlowAnalysis

import org.sireum.util._

trait ObjectFlowRepo[ValueSet <: NormalValueSet] {
	/**
   * tracking a class's instance field definitions
   */ 
  final val iFieldDefRepo = new FieldRepo[ValueSet]
  
  /**
   * tracking global variables 
   */ 
  final val globalDefRepo : MMap[String, (MSet[OfaGlobalVarNode], ValueSet)] = mmapEmpty
  
  /**
   * tracking all array variables
   */ 
  final val arrayRepo : MMap[ResourceUri, Int] = mmapEmpty
}

class FieldRepo[ValueSet <: NormalValueSet]{
  private final val iFieldDefRepo : MMap[Instance, MMap[Context, MMap[String, ValueSet]]] = mmapEmpty
  def getRepo = this.iFieldDefRepo
  def getValueSet(ins: Instance, defSite : Context, f: String) : Option[ValueSet] = {
    println("ifrepo--->" + getRepo)
    if(iFieldDefRepo.contains(ins) && iFieldDefRepo(ins).contains(defSite) && iFieldDefRepo(ins)(defSite).contains(f)){
      Some(iFieldDefRepo(ins)(defSite)(f))
    } else None
  }
  
  def setValueSet(ins: Instance, baseDefSite : Context, f: String, fieldVs : ValueSet) ={
    iFieldDefRepo.getOrElseUpdate(ins, mmapEmpty).getOrElseUpdate(baseDefSite, mmapEmpty)(f) = fieldVs
  }
  def ++=(repo2 : FieldRepo[ValueSet]) = {
    this.iFieldDefRepo ++= repo2.iFieldDefRepo
  }
  override def toString() = this.iFieldDefRepo.toString
}