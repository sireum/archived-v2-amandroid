package org.sireum.amandroid.objectflowanalysis

import org.sireum.util._

trait ObjectFlowRepo {
	/**
   * tracking a class's instance field definitions
   */ 
  final val iFieldDefRepo : MMap[ResourceUri, MMap[ResourceUri, (MSet[OfaFieldNode], MMap[ResourceUri, ResourceUri])]] = mmapEmpty
  
  /**
   * tracking global variables 
   */ 
  final val globalDefRepo : MMap[ResourceUri, (MSet[OfaGlobalVarNode], MMap[ResourceUri, ResourceUri])] = mmapEmpty
  
  /**
   * tracking all array variables
   */ 
  final val arrayRepo : MMap[ResourceUri, Int] = mmapEmpty
}