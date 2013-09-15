package org.sireum.amandroid.interProcedural.pointsToAnalysis

import org.sireum.util._
import org.sireum.amandroid.interProcedural.objectFlowAnalysis.InvokePointNode
import org.sireum.amandroid.PTAInstance
import org.sireum.amandroid._

trait JavaObjectModelForPta[Node <: PtaNode] {
	/**
   * contain all string operations and involved operation string parameter nodes and return nodes
   */
  var modelOperationTracker : Set[InvokePointNode[Node]] = Set()
  def checkModelOperation(ipN : InvokePointNode[Node], ptm : PointsToMap) : Boolean = {
    var flag : Boolean = true
    ipN.recvCallNodeOpt match{
      case Some(node) =>
        if(ptm.pointsToSet(node).isEmpty){
    		  flag = false
    		}
      case None =>
    }
    ipN.argCallNodes.foreach{
      case(i, node) =>
    		if(ptm.pointsToSet(node).isEmpty){
    		  flag = false
    		}
    }
    flag
  }
  
  def getInvokePointNodeInModelOperationTrackerFromCallComponent(node : Node) : Option[InvokePointNode[Node]] = {
    var ipNOpt : Option[InvokePointNode[Node]]  = None
    if(node.isInstanceOf[PtaRecvCallNode] || node.isInstanceOf[PtaArgCallNode])
	    modelOperationTracker.foreach{
	      ipN =>
	        if(ipN.contains(node)) ipNOpt = Some(ipN)
	    }
    ipNOpt
  }
  
  def doModelOperation(ipN : InvokePointNode[Node], pag : PointerAssignmentGraph[Node]) : Map[Node, MSet[PTAInstance]] = {
    if(checkModelOperation(ipN, pag.pointsToMap)){
      var valueSets : Map[Int, MSet[PTAInstance]] = Map()
      ipN.recvCallNodeOpt match {
      	case Some(thisEntryNode) => 
      	  valueSets += (0 -> pag.pointsToMap.pointsToSet(thisEntryNode))
      	case None =>
    	}
      ipN.argCallNodes.toList.sortBy(_._1).foreach{case (k, v) => valueSets += (k + 1 -> pag.pointsToMap.pointsToSet(v))}
      if(isStringOperation(ipN.getCalleeSig)) doStringOperation(ipN, valueSets, pag)
      else if(isStringBuilderOperation(ipN.getCalleeSig)) doStringBuilderOperation(ipN, valueSets, pag)
      else if(isHashSetOperation(ipN.getCalleeSig)) doHashSetOperation(ipN, valueSets, pag)
      else Map()
    } else Map()
  }
  
  private def doStringOperation(ipN : InvokePointNode[Node], valueSets : Map[Int, MSet[PTAInstance]], pag : PointerAssignmentGraph[Node]) = {
    var result : Map[Node, MSet[PTAInstance]] = Map()
    if(ipN.piNodeOpt.isDefined 
//        && checkOperationTimes(pag.pointsToMap.pointsToSet(ipN.piNodeOpt.get), ipN.getContext)
        ){
//	    val strsList = getStringList(valueSets)
//	    val strs : Set[String] = if(!strsList.isEmpty && !strsList(0).isEmpty)strsList.map{l => applyStringOperation(ipN.getCalleeSig, l)}.toSet
//    					               else Set()				             
//      println("strs-->" + strs)
      result += (ipN.piNodeOpt.get -> msetEmpty)
      val ins = PTAInstance(new NormalType("[|java:lang:String|]"), ipN.piNodeOpt.get.getContext.copy)
//      ins.addStrings(strs, ipN.getContext)
      result(ipN.piNodeOpt.get).add(ins)
  	}
//    flowRecvAndArgs(ipN, pag.pointsToMap)		
    result
  }
  
//  private def checkOperationTimes(insts : MSet[PTAInstance], operationContext : Context) : Boolean = {
//    var flag : Boolean = true
//    insts.foreach{
//      ins =>
//        ins match{
//          case strIns : PTAStringInstance => flag = strIns.checkOperation(operationContext)
//          case _ =>
//        }
//    }
//    flag
//  }
  
  private def doStringBuilderOperation(ipN : InvokePointNode[Node], valueSets : Map[Int, MSet[PTAInstance]], pag : PointerAssignmentGraph[Node]) = {
    var result : Map[Node, MSet[PTAInstance]] = Map()
    require(valueSets.contains(0))
    val sbInsts = valueSets(0)
//    val insts : MSet[PTAInstance] = sbInsts.map{
//      ins => 
//        pag.pointsToMap.pointsToSet(ins.toString + "[|java:lang:StringBuilder.value|]")
//    }.reduce((set1, set2) => set1 ++ set2)
//    if(checkOperationTimes(insts, ipN.getContext)){
//	    val strings : Set[String] = if(valueSets.contains(1) && checkAndGetStrings(valueSets(1)).isDefined) checkAndGetStrings(valueSets(1)).get else Set()
	    val insts : MSet[PTAInstance] = applyStringBuilderOperation(ipN, sbInsts, pag)
//	    flowRecvAndArgs(ipN, pag.pointsToMap)
	    require(ipN.recvReturnNodeOpt.isDefined)
	    pag.worklist += ipN.recvReturnNodeOpt.get
	    ipN.piNodeOpt match{
	      case Some(node) => result += (node -> insts)
	      case None =>
	    }
//    }
    result
  }
  
//  private def isStringInstanceType(insts : MSet[PTAInstance]) : Boolean = !insts.isEmpty && insts.head.isInstanceOf[PTAStringInstance]
  
//  private def checkAndGetStrings(insts : MSet[PTAInstance]) : Option[Set[String]] = {
//    if(isStringInstanceType(insts)) Some(insts.map{ins => ins.asInstanceOf[PTAStringInstance].getStrings}.reduce((set1, set2) => set1 ++ set2))
//    else None
//  }
  
  private def doHashSetOperation(ipN : InvokePointNode[Node], valueSets : Map[Int, MSet[PTAInstance]], pag : PointerAssignmentGraph[Node]) = {
    var result : Map[Node, MSet[PTAInstance]] = Map()
    require(valueSets.contains(0))
    val hsInsts = valueSets(0)
    val instsOpt : Option[MSet[PTAInstance]] = applyHashSetOperation(ipN, hsInsts, valueSets, pag)
//    flowRecvAndArgs(ipN, pag.pointsToMap)
    require(ipN.recvReturnNodeOpt.isDefined)
    pag.worklist += ipN.recvReturnNodeOpt.get
    ipN.piNodeOpt match{
      case Some(node) => result += (node -> instsOpt.getOrElse(msetEmpty))
      case None =>
    }
    result
  }
  
//  private def flowRecvAndArgs(ipN : InvokePointNode[Node], ptm : PointsToMap) = {
//    ipN.recvCallNodeOpt match{
//      case Some(recvCallNode) =>
//        ptm.propagatePointsToSet(recvCallNode, ipN.recvReturnNodeOpt.get)
//      case None =>
//    }
//    ipN.argCallNodes foreach{
//      case(i, argCallNode) =>
//        ptm.propagatePointsToSet(argCallNode, ipN.argReturnNodes(i))
//    }
//  }
  
//  def getStringList(argsValueSets : Map[Int, MSet[PTAInstance]]) : List[List[String]] = {
//    val lists = argsValueSets.toList.sortBy(_._1).map{case(k, v) => require(checkAndGetStrings(v).isDefined); checkAndGetStrings(v).get.toList}
//    CombinationIterator.combinationIterator[ResourceUri](lists).toList
//  }
    
  
  def isModelOperation(sig : String) : Boolean = isStringOperation(sig) || isStringBuilderOperation(sig) || isHashSetOperation(sig)
  
  def isStringBuilderOperation(sig : String) : Boolean = sig.contains("[|Ljava/lang/StringBuilder;.")
  
  def isStringOperation(sig : String) : Boolean = sig.contains("[|Ljava/lang/String;.")
  
  def isHashSetOperation(sig : String) : Boolean = sig.contains("[|Ljava/util/HashSet;.")
  
//  private def applyStringOperation(sig : String, strings : List[String]) : String = {
//    val size = strings.size
//	  sig match {
//      case "[|Ljava/lang/String;.<init>:(Ljava/lang/String;Ljava/lang/String;)V|]" =>
//	      require(strings.size == 3)
//	      strings(0) + strings(1) + strings(2)
//	    case "[|Ljava/lang/String;.<init>:(Ljava/lang/String;)V|]" =>
//	      require(strings.size == 2)
//	      strings(0) + strings(1)
//	    case "[|Ljava/lang/String;.<init>:(Ljava/lang/StringBuilder;)V|]" =>
//	      require(strings.size == 2)
//	      strings(0) + strings(1)
//	    case "[|Ljava/lang/String;.<init>:(Ljava/lang/String;C)V|]" =>
//	      require(strings.size == 2)
//	      strings(0) + strings(1)
//	    case "[|Ljava/lang/String;.<init>:([B)V|]" =>
//	      require(strings.size == 1)
//	      strings(0) + "bytes"
//	    case "[|Ljava/lang/String;.<init>:([BLjava/nio/charset/Charset;)V|]" =>
//	      require(strings.size == 2)
//	      strings(0) + strings(1)
//	    case "[|Ljava/lang/String;.<init>:([BIILjava/nio/charset/Charset;)V|]" =>
//	      require(strings.size == 2)
//	      strings(0) + strings(1)
//	    case "[|Ljava/lang/String;.<init>:(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V|]" =>
//	      require(strings.size == 4)
//	      strings(0) + strings(1) + strings(2) + strings(3)
//	    case "[|Ljava/lang/String;.<init>:([BIII)V|]" =>
//	      require(strings.size == 1)
//	      strings(0) + "bytes"
//	    case "[|Ljava/lang/String;.<init>:(Ljava/lang/String;I)V|]" =>
//	      require(strings.size == 2)
//	      strings(0) + strings(1)
//	    case "[|Ljava/lang/String;.<init>:([BIILjava/lang/String;)V|]" =>
//	      require(strings.size == 2)
//	      strings(0) + strings(1)
//	    case "[|Ljava/lang/String;.<init>:([BLjava/lang/String;)V|]" =>
//	      require(strings.size == 2)
//	      strings(0) + strings(1)
//	    case "[|Ljava/lang/String;.<init>:()V|]" =>
//	      require(strings.size == 1)
//	      strings(0)
//	    case "[|Ljava/lang/String;.<init>:([CII)V|]" =>
//	      require(strings.size == 1)
//	      strings(0) + "chars"
//	    case "[|Ljava/lang/String;.<init>:(Ljava/lang/StringBuffer;)V|]" =>
//	      require(strings.size == 2)
//	      strings(0) + strings(1)
//	    case "[|Ljava/lang/String;.<init>:(II[C)V|]" =>
//	      require(strings.size == 1)
//	      strings(0) + "chars"
//	    case "[|Ljava/lang/String;.<init>:([III)V|]" =>
//	      require(strings.size == 1)
//	      strings(0) + "ints"
//	    case "[|Ljava/lang/String;.<init>:([C)V|]" =>
//	      require(strings.size == 1)
//	      strings(0) + "chars"
//	    case "[|Ljava/lang/String;.<init>:([BI)V|]" =>
//	      require(strings.size == 1)
//	      strings(0) + "bytes"
//	    case "[|Ljava/lang/String;.<init>:([BII)V|]" =>
//	      require(strings.size == 1)
//	      strings(0) + "bytes"
//	    case "[|Ljava/lang/String;.<clinit>:()V|]" =>
//	      require(strings.size == 1)
//	      strings(0)
////	    case "[|Ljava/lang/String;.split:(Ljava/lang/String;I)[Ljava/lang/String;|]" =>
////	      require(strings.size == 2)
////	      strings(0)).split(strings(1))
////	    case "[|Ljava/lang/String;.split:(Ljava/lang/String;)[Ljava/lang/String;|]" =>
//	    case "[|Ljava/lang/String;.trim:()Ljava/lang/String;|]" => 
//	      require(strings.size == 1)
//	      strings(0).trim()
//	    case "[|Ljava/lang/String;.intern:()Ljava/lang/String;|]" =>
//	      require(strings.size == 1)
//	      strings(0).intern()
//	    case "[|Ljava/lang/String;.toLowerCase:()Ljava/lang/String;|]" =>
//	      require(strings.size == 1)
//	      strings(0).toLowerCase()
//	    case "[|Ljava/lang/String;.toUpperCase:()Ljava/lang/String;|]" =>
//	      require(strings.size == 1)
//	      strings(0).toUpperCase()
//	    case "[|Ljava/lang/String;.replaceAll:(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;|]" =>
//	      require(strings.size == 3)
//	      strings(0).replaceAll(strings(1), strings(2))
//	    case "[|Ljava/lang/String;.toString:()Ljava/lang/String;|]" =>
//	      require(strings.size == 1)
//	      strings(0)
//	    case "[|Ljava/lang/String;.concat:(Ljava/lang/String;)Ljava/lang/String;|]" =>
//	      require(strings.size == 2)
////	      println("1:" + strings(0) + "2:" + strings(1))
//	      strings(0).concat(strings(1))
//	    case "[|Ljava/lang/String;.replaceFirst:(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;|]" => 
//	      require(strings.size == 3)
//	      strings(0).replaceFirst(strings(1), strings(2))
//	    /*
//	     * [|Ljava/lang/String;.toLowerCase:(Ljava/util/Locale;)Ljava/lang/String;|]
//	     * [|Ljava/lang/String;.toUpperCase:(Ljava/util/Locale;)Ljava/lang/String;|]
//	     * [|Ljava/lang/String;.format:(Ljava/util/Locale;Ljava/lang/String;[Ljava/lang/Object;)Ljava/lang/String;|]
//	     * [|Ljava/lang/String;.format:(Ljava/lang/String;[Ljava/lang/Object;)Ljava/lang/String;|]
//	     * [|Ljava/lang/String;.replace:(Ljava/lang/CharSequence;Ljava/lang/CharSequence;)Ljava/lang/String;|]
//	     */
//	    /*ends here*/
//	    case _ =>
//	      strings(0)
//    }
//  }
  
	def applyStringBuilderOperation(ipN : InvokePointNode[Node], sbInsts : MSet[PTAInstance], pag : PointerAssignmentGraph[Node]) : MSet[PTAInstance] = {
	  
	  ipN.getCalleeSig match {
	    case "[|Ljava/lang/StringBuilder;.<init>:()V|]" =>
	      sbInsts.map{
	        ins =>
	          val sIns = PTAInstance(new NormalType("[|java:lang:String|]"), ipN.getContext.copy)
	          pag.pointsToMap.addInstanceInternal(ins.toString + "[|java:lang:StringBuilder.value|]", sIns)
	      }
	      msetEmpty
	    case "[|Ljava/lang/StringBuilder;.<init>:(I)V|]" =>
	      sbInsts.map{
	        ins =>
	          val sIns = PTAInstance(new NormalType("[|java:lang:String|]"), ipN.getContext.copy)
	          pag.pointsToMap.addInstanceInternal(ins.toString + "[|java:lang:StringBuilder.value|]", sIns)
	      }
	      msetEmpty
	    case "[|Ljava/lang/StringBuilder;.<init>:(Ljava/lang/CharSequence;)V|]" =>
	      sbInsts.map{
	        ins =>
	          val sIns = PTAInstance(new NormalType("[|java:lang:String|]"), ipN.getContext.copy)
	          pag.pointsToMap.addInstanceInternal(ins.toString + "[|java:lang:StringBuilder.value|]", sIns)
	      }
	      msetEmpty
	    case "[|Ljava/lang/StringBuilder;.<init>:(Ljava/lang/String;)V|]" =>
	      sbInsts.map{
	        ins =>
	          val sIns = PTAInstance(new NormalType("[|java:lang:String|]"), ipN.getContext.copy)
	          pag.pointsToMap.addInstanceInternal(ins.toString + "[|java:lang:StringBuilder.value|]", sIns)
	      }
	      msetEmpty
	    case "[|Ljava/lang/StringBuilder;.append:([C)Ljava/lang/StringBuilder;|]" =>
	      sbInsts.map{
	        ins =>
	          val sIns = PTAInstance(new NormalType("[|java:lang:String|]"), ipN.getContext.copy)
	          pag.pointsToMap.addInstanceInternal(ins.toString + "[|java:lang:StringBuilder.value|]", sIns)
	      }
	      sbInsts
	    case "[|Ljava/lang/StringBuilder;.append:(C)Ljava/lang/Appendable;|]" =>
	      sbInsts.map{
	        ins =>
	          val sIns = PTAInstance(new NormalType("[|java:lang:String|]"), ipN.getContext.copy)
	          pag.pointsToMap.addInstanceInternal(ins.toString + "[|java:lang:StringBuilder.value|]", sIns)
	      }
	      sbInsts
	    case "[|Ljava/lang/StringBuilder;.append:(Ljava/lang/CharSequence;II)Ljava/lang/StringBuilder;|]" =>
	      sbInsts.map{
	        ins =>
	          val sIns = PTAInstance(new NormalType("[|java:lang:String|]"), ipN.getContext.copy)
	          pag.pointsToMap.addInstanceInternal(ins.toString + "[|java:lang:StringBuilder.value|]", sIns)
	      }
	      sbInsts
	    case "[|Ljava/lang/StringBuilder;.append:(Ljava/lang/String;)Ljava/lang/StringBuilder;|]" =>
	      sbInsts.map{
	        ins =>
	          val sIns = PTAInstance(new NormalType("[|java:lang:String|]"), ipN.getContext.copy)
	          pag.pointsToMap.addInstanceInternal(ins.toString + "[|java:lang:StringBuilder.value|]", sIns)
	      }
//	      sbInsts.map{
//	        ins =>
//	          val strInsts = pag.pointsToMap.pointsToSet(ins.toString + "[|java:lang:StringBuilder.value|]")
//            require(isStringInstanceType(strInsts) && !strings.isEmpty)
//            strInsts.foreach{
//			        strIns =>
//			          val strs = 
//			            strIns.asInstanceOf[PTAStringInstance].getStrings.map{
//			            	str1 => 
//			            	  strings map{
//			            	    str2 =>
//			            	      println("1:" + str1 + " 2:" + str2)
//			            	      str1 + str2
//			            	  }
//			            }.reduce((set1, set2) => set1 ++ set2)
//			          strIns.asInstanceOf[PTAStringInstance].defSite = ipN.getContext.copy
//			          strIns.asInstanceOf[PTAStringInstance].setStrings(strs, ipN.getContext)
//			      }
//	      }
	      sbInsts
	    case "[|Ljava/lang/StringBuilder;.append:(Z)Ljava/lang/StringBuilder;|]" =>
	      sbInsts.map{
	        ins =>
	          val sIns = PTAInstance(new NormalType("[|java:lang:String|]"), ipN.getContext.copy)
	          pag.pointsToMap.addInstanceInternal(ins.toString + "[|java:lang:StringBuilder.value|]", sIns)
	      }
	      sbInsts
	    case "[|Ljava/lang/StringBuilder;.append:(Ljava/lang/CharSequence;)Ljava/lang/StringBuilder;|]" =>
	      sbInsts.map{
	        ins =>
	          val sIns = PTAInstance(new NormalType("[|java:lang:String|]"), ipN.getContext.copy)
	          pag.pointsToMap.addInstanceInternal(ins.toString + "[|java:lang:StringBuilder.value|]", sIns)
	      }
	      sbInsts
	    case "[|Ljava/lang/StringBuilder;.append:(Ljava/lang/Object;)Ljava/lang/StringBuilder;|]" =>
	      sbInsts.map{
	        ins =>
	          val sIns = PTAInstance(new NormalType("[|java:lang:String|]"), ipN.getContext.copy)
	          pag.pointsToMap.addInstanceInternal(ins.toString + "[|java:lang:StringBuilder.value|]", sIns)
	      }
	      sbInsts
	    case "[|Ljava/lang/StringBuilder;.append:(D)Ljava/lang/StringBuilder;|]" =>
	      sbInsts.map{
	        ins =>
	          val sIns = PTAInstance(new NormalType("[|java:lang:String|]"), ipN.getContext.copy)
	          pag.pointsToMap.addInstanceInternal(ins.toString + "[|java:lang:StringBuilder.value|]", sIns)
	      }
	      sbInsts
	    case "[|Ljava/lang/StringBuilder;.append:(J)Ljava/lang/StringBuilder;|]" =>
	      sbInsts.map{
	        ins =>
	          val sIns = PTAInstance(new NormalType("[|java:lang:String|]"), ipN.getContext.copy)
	          pag.pointsToMap.addInstanceInternal(ins.toString + "[|java:lang:StringBuilder.value|]", sIns)
	      }
	      sbInsts
	    case "[|Ljava/lang/StringBuilder;.append:(Ljava/lang/CharSequence;)Ljava/lang/Appendable;|]" =>
	      sbInsts.map{
	        ins =>
	          val sIns = PTAInstance(new NormalType("[|java:lang:String|]"), ipN.getContext.copy)
	          pag.pointsToMap.addInstanceInternal(ins.toString + "[|java:lang:StringBuilder.value|]", sIns)
	      }
	      sbInsts
	    case "[|Ljava/lang/StringBuilder;.append:(C)Ljava/lang/StringBuilder;|]" =>
	      sbInsts.map{
	        ins =>
	          val sIns = PTAInstance(new NormalType("[|java:lang:String|]"), ipN.getContext.copy)
	          pag.pointsToMap.addInstanceInternal(ins.toString + "[|java:lang:StringBuilder.value|]", sIns)
	      }
	      sbInsts
	    case "[|Ljava/lang/StringBuilder;.append:(I)Ljava/lang/StringBuilder;|]" =>
	      sbInsts.map{
	        ins =>
	          val sIns = PTAInstance(new NormalType("[|java:lang:String|]"), ipN.getContext.copy)
	          pag.pointsToMap.addInstanceInternal(ins.toString + "[|java:lang:StringBuilder.value|]", sIns)
	      }
	      sbInsts
	    case "[|Ljava/lang/StringBuilder;.append:(Ljava/lang/CharSequence;II)Ljava/lang/Appendable;|]" =>
	      sbInsts.map{
	        ins =>
	          val sIns = PTAInstance(new NormalType("[|java:lang:String|]"), ipN.getContext.copy)
	          pag.pointsToMap.addInstanceInternal(ins.toString + "[|java:lang:StringBuilder.value|]", sIns)
	      }
	      sbInsts
	    case "[|Ljava/lang/StringBuilder;.reverse:()Ljava/lang/StringBuilder;|]" =>
//	      sbInsts.map{
//	        ins =>
//	          val strInsts = pag.pointsToMap.pointsToSet(ins.toString + "[|java:lang:StringBuilder.value|]")
//            require(isStringInstanceType(strInsts))
//            strInsts.foreach{
//			        strIns =>
//			          val strings = strIns.asInstanceOf[PTAStringInstance].getStrings.map{str => str.reverse}
//			          strIns.asInstanceOf[PTAStringInstance].setStrings(strings, ipN.getContext)
//			          strIns.asInstanceOf[PTAStringInstance].defSite = ipN.getContext.copy
//	          }
//	      }
	      sbInsts
	    case "[|Ljava/lang/StringBuilder;.toString:()Ljava/lang/String;|]" =>
	      val insts : MSet[PTAInstance] = msetEmpty
//	      sbInsts.foreach{
//	        ins =>
//	          val strInsts = pag.pointsToMap.pointsToSet(ins.toString + "[|java:lang:StringBuilder.value|]")
////            require(isStringInstanceType(strInsts))
//            strInsts.foreach{
//			        strIns =>
//			          val sIns = PTAInstance(new NormalType("[|java:lang:String|]"), ipN.getContext.copy)
////			          val strs = strIns.asInstanceOf[PTAStringInstance].getStrings
////			          sIns.setStrings(strs, ipN.getContext)
//			          insts += sIns
//	          }
//	      }
	      val sIns = PTAInstance(new NormalType("[|java:lang:String|]"), ipN.getContext.copy)
	      insts += sIns
	      insts
	    case _ => msetEmpty
  	}
  }
	
	def applyHashSetOperation(ipN : InvokePointNode[Node], hsInsts : MSet[PTAInstance], argsInsts : Map[Int, MSet[PTAInstance]], pag : PointerAssignmentGraph[Node]) : Option[MSet[PTAInstance]] = {
	  
	  ipN.getCalleeSig match {
	    case "[|Ljava/util/HashSet;.<init>:()V|]" =>
	      Some(hsInsts)
	    case "[|Ljava/util/HashSet;.<init>:(I)V|]" =>
	      Some(hsInsts)
	    case "[|Ljava/util/HashSet;.<init>:(IF)V|]" =>
	      Some(hsInsts)
	    case "[|Ljava/util/HashSet;.<init>:(Ljava/util/Collection;)V|]" =>
//	      hsValueSet.instances.map{
//	        ins =>
//	          val vs = fac()
//	          ins.updateFieldDefSite("[|java:util:HashSet.items|]", ipN.getContext.copy, vs.asInstanceOf[NormalValueSet])
//	          ins.setFieldLastDefSite("[|java:util:HashSet.items|]", ipN.getContext.copy)
//	      }
	      Some(hsInsts)
	    case "[|Ljava/util/HashSet;.<init>:(Ljava/util/HashMap;)V|]" =>
//	      hsValueSet.instances.map{
//	        ins =>
//	          val vs = fac()
//	          require(argsValueSets.contains(1))
//	          val hpVs = argsValueSets(1)
//	          vs.addInstances(argsValueSets(1).instances)
//	          ins.updateFieldDefSite("[|java:util:HashSet.items|]", ipN.getContext.copy, vs.asInstanceOf[NormalValueSet])
//	          ins.setFieldLastDefSite("[|java:util:HashSet.items|]", ipN.getContext.copy)
//	      }
	      Some(hsInsts)
	    case "[|Ljava/util/HashSet;.readObject:(Ljava/io/ObjectInputStream;)V|]" =>
	      None
	    case "[|Ljava/util/HashSet;.writeObject:(Ljava/io/ObjectOutputStream;)V|]" =>
	      None
	    case "[|Ljava/util/HashSet;.add:(Ljava/lang/Object;)Z|]" =>
	      hsInsts.map{
	        ins =>
	          require(argsInsts.contains(1))
	          pag.pointsToMap.addInstancesInternal(ins.toString + "[|java:util:HashSet.items|]", argsInsts(1))
	      }
	      Some(hsInsts)
	    case "[|Ljava/util/HashSet;.clear:()V|]" => 
//	      hsInsts.map{
//	        ins =>
//	          val insts = pag.pointsToMap.pointsToSet(ins.toString + "[|java:util:HashSet.items|]")
//	          insts.clear
//	      }
	      Some(hsInsts)
	    case "[|Ljava/util/HashSet;.clone:()Ljava/lang/Object;|]" =>
	      val newHs = PTAInstance(new NormalType("[|java:util:HashSet|]"), ipN.getContext)
	       hsInsts.map{
	        ins =>
	          val insts = pag.pointsToMap.pointsToSet(ins.toString + "[|java:util:HashSet.items|]")
	          pag.pointsToMap.addInstancesInternal(newHs + "[|java:util:HashSet.items|]", insts.clone)
	      }
	      Some(msetEmpty + newHs)
	    case "[|Ljava/util/HashSet;.contains:(Ljava/lang/Object;)Z|]" =>
	      None
	    case "[|Ljava/util/HashSet;.createBackingMap:(IF)Ljava/util/HashMap;|]" =>
	      None
	    case "[|Ljava/util/HashSet;.isEmpty:()Z|]" =>
	      None
	    case "[|Ljava/util/HashSet;.iterator:()Ljava/util/Iterator;|]" =>
	      None
	    case "[|Ljava/util/HashSet;.remove:(Ljava/lang/Object;)Z|]" =>
//	      hsInsts.map{
//	        ins =>
//	          val insts = 
//	          require(argsInsts.contains(1))
//	          pag.pointsToMap.removeInstancesInternal(ins.toString + "[|java:util:HashSet.items|]", argsInsts(1))
//	      }
	      None
	    case "[|Ljava/util/HashSet;.size:()I|]" =>
	      None
	    case _ =>
	      None
	  }
	}
}