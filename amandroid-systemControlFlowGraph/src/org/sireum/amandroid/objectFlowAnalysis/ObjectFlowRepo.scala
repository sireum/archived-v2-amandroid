package org.sireum.amandroid.objectFlowAnalysis

import org.sireum.util._

trait ObjectFlowRepo[ValueSet <: NormalValueSet] {
	/**
   * tracking a class's instance field definitions
   */ 
//  final val iFieldDefRepo = new FieldRepo[ValueSet]
  
  /**
   * tracking global variables 
   */ 
  final val globalDefRepo : MMap[String, (MSet[OfaGlobalVarNode], ValueSet)] = mmapEmpty
  
  /**
   * tracking all array variables
   */ 
  final val arrayRepo : MMap[ResourceUri, Int] = mmapEmpty
}

//class FieldRepo[ValueSet <: NormalValueSet]{
//  private final val iFieldDefRepo : MMap[Instance, MMap[Context, MMap[String, ValueSet]]] = mmapEmpty
//  private final val associateNodeRepo : MMap[Instance, MMap[Context, MMap[String, MSet[OfaFieldNode]]]] = mmapEmpty
//  def getRepo = this.iFieldDefRepo
//  
//  def getValueSet(ins: Instance, f: String) : Option[ValueSet] = {
//    def combine(vs1 : ValueSet, vs2 : ValueSet) : ValueSet = vs1.update(vs2).asInstanceOf[ValueSet]
//    var vsSet : Set[ValueSet] = Set()
//    if(ins.fieldDefSiteRepo.contains(f))
//	    ins.getFieldDefSite(f) match {
//	      case Right(defSites) =>
//	        defSites.foreach{
//	          defSite => 
//	            if(iFieldDefRepo.contains(ins) && iFieldDefRepo(ins).contains(defSite) && iFieldDefRepo(ins)(defSite).contains(f)){
//	            	vsSet+=iFieldDefRepo(ins)(defSite)(f)
//	            }
//	        }
//	      case Left(defSite) => 
//	        if(iFieldDefRepo.contains(ins) && iFieldDefRepo(ins).contains(defSite) && iFieldDefRepo(ins)(defSite).contains(f)){
//	        	vsSet+=iFieldDefRepo(ins)(defSite)(f)
//	        }
////	      println("ins-->" + ins + "\nf-->" + f + "\niFieldDefRepo(ins)-->" + iFieldDefRepo(ins) + "\ngetFieldDefSite-->" + ins.getFieldDefSite(f))
//	    }
//    
//    if(vsSet.isEmpty) None
//    else Some(vsSet.map(vs => vs).reduce(combine))
//  }
//  
//  def getValueSet(ins: Instance, defSite : Context, f: String) : Option[ValueSet] = {
//    if(iFieldDefRepo.contains(ins) && iFieldDefRepo(ins).contains(defSite) && iFieldDefRepo(ins)(defSite).contains(f)){
//      Some(iFieldDefRepo(ins)(defSite)(f))
//    } else None
//  }
//  def getAssociateNodes(ins: Instance, defSite : Context, f: String) : Option[MSet[OfaFieldNode]] = {
//    if(associateNodeRepo.contains(ins) && associateNodeRepo(ins).contains(defSite) && associateNodeRepo(ins)(defSite).contains(f)){
//      Some(associateNodeRepo(ins)(defSite)(f))
//    } else None
//  }
//  def setValueSet(ins: Instance, baseDefSite : Context, f: String, fieldVs : ValueSet) ={
//    iFieldDefRepo.getOrElseUpdate(ins, mmapEmpty).getOrElseUpdate(baseDefSite, mmapEmpty)(f) = fieldVs
//  }
//  def setAssociateNode(ins: Instance, baseDefSite : Context, f: String, node : OfaFieldNode) ={
//    associateNodeRepo.getOrElseUpdate(ins, mmapEmpty).getOrElseUpdate(baseDefSite, mmapEmpty).getOrElseUpdate(f, msetEmpty) += node
//  }
//  def ++=(repo2 : FieldRepo[ValueSet]) = {
//    this.iFieldDefRepo ++= repo2.iFieldDefRepo
//  }
//  override def toString() : String = {
//    var str = ""
//    this.iFieldDefRepo.foreach{
//      case (ins, repo) =>
//        str += "instance: " + ins + "\n"
//        repo.foreach{
//          case (cont, fMap) =>
//            str += "  context: " + cont + "\n"
//            fMap.foreach{
//              case (field, vs) =>
//                str += "    field: " + field + "\n" + vs
//            }
//        }
//    }
//    str
//  }
//}