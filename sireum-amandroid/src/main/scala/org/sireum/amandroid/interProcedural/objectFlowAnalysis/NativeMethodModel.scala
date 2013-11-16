package org.sireum.amandroid.interProcedural.objectFlowAnalysis

import org.sireum.util._
import org.sireum.amandroid.util.CombinationIterator
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.pilar.ast.NameExp

trait NativeMethodModel[Node <: OfaNode, ValueSet <: NormalValueSet] {
  /**
   * contain all native operation signatures and involved operation parameter nodes and return nodes
   */
  var nativeOperationTracker : Set[InvokePointNode[Node]] = Set()
  
  def checkNativeOperation(ipN : InvokePointNode[Node]) : Boolean = {
    var flag : Boolean = true
    ipN.recvCallNodeOpt match{
      case Some(node) =>
        if(node.getProperty[ValueSet]("ValueSet").isEmpty){
    		  flag = false
    		}
      case None =>
    }
    ipN.argCallNodes.foreach{
      case(i, node) =>
    		if(node.getProperty[ValueSet]("ValueSet").isEmpty){
    		  flag = false
    		}
    }
    flag
  }
  
  def getInvokePointNodeInNativeTrackerFromCallComponent(node : Node) : Option[InvokePointNode[Node]] = {
    var ipNOpt : Option[InvokePointNode[Node]]  = None
    if(node.isInstanceOf[OfaRecvCallNode] || node.isInstanceOf[OfaArgCallNode])
	    nativeOperationTracker.foreach{
	      ipN =>
	        if(ipN.contains(node)) ipNOpt = Some(ipN)
	    }
    ipNOpt
  }
  
  def doNativeOperation(ipN : InvokePointNode[Node], fac :() => ValueSet) = {
    var result : Map[Node, ValueSet] = Map()
    if(checkNativeOperation(ipN)){
//      val retTyp = (new SignatureParser(ipN.getCalleeSig).getParamSig.getReturnObjectType).getOrElse(null)
//      val values : MList[ResourceUri] = mlistEmpty
//      var valueSets : Map[Int, ValueSet] = Map()
//      ipN.recvCallNodeOpt match {
//      	case Some(thisEntryNode) => valueSets += (0 -> thisEntryNode.getProperty[ValueSet]("ValueSet"))
//      	case None =>
//    	}
//      ipN.argCallNodes.toList.sortBy(_._1).foreach{case (k, v) => valueSets += (k + 1 -> v.getProperty[ValueSet]("ValueSet"))}
//      val strsList = getValueSetList(valueSets)
//      val strs : Set[String] = if(!strsList.isEmpty && !strsList(0).isEmpty)strsList.map{l => applyNativeOperation(ipN.getCalleeSig, values ++ l)}.toSet
//      					               else Set()
//      result += (ipN.piNode -> fac())
//      val ins = StringInstance("[|java:lang:String|]", ipN.piNode.getContext.copy)
//      ins.addStrings(strs)
//      result(ipN.piNode).addInstance(ins)
    }
    result
  }
  
  def getValueSetList(argsValueSets : Map[Int, ValueSet]) : List[List[String]] = {
    val lists = argsValueSets.toList.sortBy(_._1).map{case (k, v) => v.checkAndGetStrings.get.toList}
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