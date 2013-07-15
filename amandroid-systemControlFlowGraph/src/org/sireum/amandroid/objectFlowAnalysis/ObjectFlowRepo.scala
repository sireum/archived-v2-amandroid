package org.sireum.amandroid.objectFlowAnalysis

import org.sireum.util._

trait ObjectFlowRepo[ValueSet <: NormalValueSet] {
	/**
   * tracking a class's instance field definitions
   */ 
  final val iFieldDefRepo : FieldRepo[ValueSet] = new FieldRepo
  
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
  private final val iFieldDefRepo : MMap[PointO, MMap[Context, MMap[String, ValueSet]]] = mmapEmpty
  def getRepo = this.iFieldDefRepo
  def getValueSet(ins: PointO, defSite : Context, f: String) : Option[ValueSet] = {
    if(iFieldDefRepo.contains(ins) && iFieldDefRepo(ins).contains(defSite) && iFieldDefRepo(ins)(defSite).contains(f)){
      Some(iFieldDefRepo(ins)(defSite)(f))
    } else None
  }
  
  def setValueSet(ins: PointO, baseDefSite : Context, f: String, fieldVs : ValueSet) ={
    iFieldDefRepo.getOrElseUpdate(ins, mmapEmpty).getOrElseUpdate(baseDefSite, mmapEmpty)(f) = fieldVs
  }
  def ++=(repo2 : FieldRepo[ValueSet]) = {
    this.iFieldDefRepo ++= repo2.iFieldDefRepo
  }
  override def toString() = iFieldDefRepo.toString
}