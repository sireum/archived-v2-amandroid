package org.sireum.amandroid.objectFlowAnalysis

import org.sireum.util._
import org.sireum.amandroid.util.CombinationIterator
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.pilar.ast.NameExp
import org.sireum.amandroid.util.SignatureParser

trait NativeMethodModel[Node <: OfaNode, ValueSet <: NormalValueSet] {
  /**
   * contain all native operation signatures and involved operation parameter nodes and return nodes
   */
  var nativeOperationTracker : Map[String, ProcedurePointNodes[Node]] = Map()
  
  def checkNativeOperation(pUri : ResourceUri) : Boolean = {
    var flag : Boolean = true
    nativeOperationTracker(pUri).paramEntryNodes.foreach{
      case(i, node) =>
    		if(node.getProperty[ValueSet]("ValueSet").isEmpty){
    		  flag = false
    		}
    }
    flag
  }
  
  def doNativeOperation(fac :() => ValueSet) = {
    var result : Map[Node, ValueSet] = Map()
    nativeOperationTracker.map{
      case (k, v) =>
        if(checkNativeOperation(k)){
          val retTyp = (new SignatureParser(k).getParamSig.getReturnObjectType).getOrElse(null)
	        val values : MList[ResourceUri] = mlistEmpty
	        val valueSets : Map[Int, ValueSet] =
		        v.paramEntryNodes.map{
	            case(i, node) =>
		          	(i, node.getProperty[ValueSet]("ValueSet"))
		        }.toMap
	        val strsList = getValueSetList(valueSets)
	        val strs : Set[String] = if(retTyp != null && !strsList.isEmpty && !strsList(0).isEmpty)strsList.map{l => applyNativeOperation(k, values ++ l)}.toSet
	        					 else Set()
	        v.retNodeOpt match{
	          case Some(r) =>
	            result += (r -> fac())
	            result(r).setInstance(Instance("[|java:lang:String|]", v.procPoint.getLoc), r.getContext)
	            result(r).setStrings(strs)
	          case None =>
	        }
        }
    }
//    println("result---->" + result)
    result
  }
  
  def getValueSetList(argsValueSets : Map[Int, ValueSet]) : List[List[String]] = {
    val lists = argsValueSets.toList.sortBy(_._1).map{case (k, v) => v.strings.toList}
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