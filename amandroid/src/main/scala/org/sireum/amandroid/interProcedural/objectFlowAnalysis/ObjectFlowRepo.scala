package org.sireum.amandroid.interProcedural.objectFlowAnalysis

import org.sireum.util._

trait ObjectFlowRepo[ValueSet <: NormalValueSet] {
  
  /**
   * tracking global variables 
   */ 
  final val globalDefRepo : MMap[String, (MSet[OfaGlobalVarNode], ValueSet)] = mmapEmpty
  
  /**
   * tracking all array variables
   */ 
  final val arrayRepo : MMap[ResourceUri, Int] = mmapEmpty
  
  /**
   * tracking all field variables
   */ 
  final var fieldVarRepo : Set[ResourceUri] = Set()
}