package org.sireum.amandroid.objectFlowAnalysis

import org.sireum.util._
import org.sireum.amandroid.util.CombinationIterator
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.pilar.ast.NameExp
import org.sireum.amandroid.util.SignatureParser

trait NativeMethodModel {
  /**
   * contain all native operation signatures and involved operation parameter nodes and return nodes
   */
  val nativeOperationTracker : MMap[String, (MList[OfaNode], Option[OfaNode])] = mmapEmpty
  
  def checkNativeOperation(pUri : ResourceUri) : Boolean = {
    var flag : Boolean = true
    nativeOperationTracker(pUri)._1.foreach{
      node =>
    		if(node.getProperty[MMap[ResourceUri, ResourceUri]]("ValueSet").isEmpty){
    		  flag = false
    		}
    }
    flag
  }
  
  def doNativeOperation() = {
    val result : MMap[OfaNode, MMap[ResourceUri, ResourceUri]] = mmapEmpty
    nativeOperationTracker.map{
      case (k, v) =>
        if(checkNativeOperation(k)){
          val retTyp = (new SignatureParser(k).getParamSig.getReturnObjectType).getOrElse(null)
	        val values : MList[ResourceUri] = mlistEmpty
	        val valueSets : MMap[Int, MMap[ResourceUri, ResourceUri]] = mmapEmpty
	        for(i <- 0 to v._1.size - 1){
	          valueSets(i) = mmapEmpty
	          valueSets(i) ++= v._1(i).getProperty[MMap[ResourceUri, ResourceUri]]("ValueSet")
	        }
	        val strsList = getValueSetList(valueSets)
	        val strs = if(retTyp != null && !strsList.isEmpty && !strsList(0).isEmpty)strsList.map{l => (applyNativeOperation(k, values ++ l), if(retTyp.equals("[|java:lang:String|]"))"STRING" else retTyp)}.toMap
	        					 else Map()
	        v._2 match{
	          case Some(r) =>
	            result(r) = mmapEmpty
	            result(r) ++= strs
	          case None =>
	        }
        }
    }
//    println("result---->" + result)
    result
  }
  
  def getValueSetList(argsValueSets : MMap[Int, MMap[ResourceUri, ResourceUri]]) : List[List[String]] = {
    val lists = argsValueSets.toList.sortBy(_._1).map{case (k, v) => v.map{case (uri, typ) => uri}.toList}
    CombinationIterator.combinationIterator[ResourceUri](lists).toList
  }
  
  def isNativeOperation(pst : ProcedureSymbolTable) : Boolean = {
    val accessExp = pst.procedure.getValueAnnotation("Access").getOrElse(null)
    val access = accessExp match {
      case ne : NameExp =>
        ne.name.name
      case _ => null
    }
    isNativeOperation(access)
  }
  
  def isNativeOperation(access : String) : Boolean = {
    if(access != null && access.contains("NATIVE")){
      true
    } else false
  }
  
	def applyNativeOperation(sig : ResourceUri, values : MList[String]) : String = {
	  val size = values.size
	  //println(values)
	  sig match {
	    case "[|Ljava/lang/Class;.getNameNative:()Ljava/lang/String;|]" =>
	      require(size > 0)
	      values(0).replaceFirst("new:", "").split("@L")(0)  //before: new:[|de:mobinauten:smsspy:EmergencyService|]@L000bc4 after: [|de:mobinauten:smsspy:EmergencyService|]
	    case "[|Ljava/lang/String;.intern:()Ljava/lang/String;|]"  =>
	      require(size > 0)
	      values(0) 
	      
	    case _ =>
	      require(size > 0)
	      values(0)
	  }
  }
}