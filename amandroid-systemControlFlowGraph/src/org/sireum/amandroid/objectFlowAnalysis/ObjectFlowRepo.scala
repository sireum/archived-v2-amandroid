package org.sireum.amandroid.objectFlowAnalysis

import org.sireum.util._

trait ObjectFlowRepo[ValueSet <: NormalValueSet] {
	/**
   * tracking a class's instance field definitions
   */ 
  final val iFieldDefRepo : MMap[PointO, MMap[String, (MSet[OfaFieldNode], ValueSet)]] = mmapEmpty
  
  /**
   * tracking global variables 
   */ 
  final val globalDefRepo : MMap[String, (MSet[OfaGlobalVarNode], ValueSet)] = mmapEmpty
  
  /**
   * tracking all array variables
   */ 
  final val arrayRepo : MMap[ResourceUri, Int] = mmapEmpty
}