package org.sireum.amandroid.interProcedural.objectFlowAnalysis

import org.sireum.amandroid.util.CombinationIterator
import org.sireum.util.ResourceUri
import org.sireum.amandroid._
import org.sireum.amandroid.interProcedural.Context

trait JavaObjectModelForOfa[Node <: OfaNode, ValueSet <: NormalValueSet] {
	/**
   * contain all string operations and involved operation string parameter nodes and return nodes
   */
  var modelOperationTracker : Set[InvokePointNode[Node]] = Set()
  def checkModelOperation(ipN : InvokePointNode[Node]) : Boolean = {
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
  
  def getInvokePointNodeInModelOperationTrackerFromCallComponent(node : Node) : Option[InvokePointNode[Node]] = {
    var ipNOpt : Option[InvokePointNode[Node]]  = None
    if(node.isInstanceOf[OfaRecvCallNode] || node.isInstanceOf[OfaArgCallNode])
	    modelOperationTracker.foreach{
	      ipN =>
	        if(ipN.contains(node)) ipNOpt = Some(ipN)
	    }
    ipNOpt
  }
  
  def doModelOperation(ipN : InvokePointNode[Node], fac :() => ValueSet, ofg : ObjectFlowGraph[Node, ValueSet]) : Map[Node, ValueSet] = {
    if(checkModelOperation(ipN)){
      
      var valueSets : Map[Int, ValueSet] = Map()
      ipN.recvCallNodeOpt match {
      	case Some(thisEntryNode) => 
      	  valueSets += (0 -> thisEntryNode.getProperty[ValueSet]("ValueSet").copy.asInstanceOf[ValueSet])
      	case None =>
    	}
      ipN.argCallNodes.toList.sortBy(_._1).foreach{case (k, v) => valueSets += (k + 1 -> v.getProperty[ValueSet]("ValueSet").copy.asInstanceOf[ValueSet])}
      if(isStringOperation(ipN.getCalleeSig)) doStringOperation(ipN, valueSets, fac, ofg.K_STRING)
      else if(isStringBuilderOperation(ipN.getCalleeSig)) doStringBuilderOperation(ipN, valueSets, fac, ofg)
      else if(isHashSetOperation(ipN.getCalleeSig)) doHashSetOperation(ipN, valueSets, fac, ofg)
      else Map()
    } else Map()
  }
  
  private def doStringOperation(ipN : InvokePointNode[Node], valueSets : Map[Int, ValueSet], fac :() => ValueSet, k_string : Int) = {
    var result : Map[Node, ValueSet] = Map()
    if(ipN.piNodeOpt.isDefined && checkOperationTimes(ipN.piNodeOpt.get.getProperty[ValueSet]("ValueSet"), ipN.piNodeOpt.get.getContext)){
	    val strsList = getStringList(valueSets)
	    val strs : Set[String] = if(!strsList.isEmpty && !strsList(0).isEmpty)strsList.map{l => applyStringOperation(ipN.getCalleeSig, l)}.toSet
    					               else Set()				             
//      println("strs-->" + strs)
      result += (ipN.piNodeOpt.get -> fac())
      val ins = OFAStringInstance(new NormalType("[|java:lang:String|]"), ipN.piNodeOpt.get.getContext.copy, k_string)
      ins.addStrings(strs, ipN.getContext)
      result(ipN.piNodeOpt.get).addInstance(ins)
  	}
    flowRecvAndArgs(ipN, valueSets)		
    result
  }
  
  private def checkOperationTimes(vs : ValueSet, operationContext : Context) : Boolean = {
  	checkOperationTimes(vs.instances, operationContext)
  }
  
  private def checkOperationTimes(insts : Set[OFAInstance], operationContext : Context) : Boolean = {
    var flag : Boolean = true
    insts.foreach{
      ins =>
        ins match{
          case strIns : OFAStringInstance => flag = strIns.checkOperation(operationContext)
          case _ =>
        }
    }
    flag
  }
  
  private def doStringBuilderOperation(ipN : InvokePointNode[Node], valueSets : Map[Int, ValueSet], fac :() => ValueSet, ofg : ObjectFlowGraph[Node, ValueSet]) = {
    var result : Map[Node, ValueSet] = Map()
    require(valueSets.contains(0))
    val sbVs = valueSets(0)
    val insts : Set[OFAInstance] = sbVs.instances.map{
      ins => 
        ins.getFieldValueSet("[|java:lang:StringBuilder.value|]") match{
          case Some(vs) => vs.instances
          case None => Set[OFAInstance]()
        }
    }.reduce((set1, set2) => set1 ++ set2)
    if(checkOperationTimes(insts, ipN.getContext)){
	    val strings : Set[String] = if(valueSets.contains(1) && valueSets(1).checkAndGetStrings.isDefined) valueSets(1).checkAndGetStrings.get else Set()
	    val vs : ValueSet = applyStringBuilderOperation(ipN, sbVs, strings, fac, ofg.K_STRING)
	    flowRecvAndArgs(ipN, valueSets)
	    require(ipN.recvReturnNodeOpt.isDefined)
	    ofg.worklist += ipN.recvReturnNodeOpt.get
	    ipN.piNodeOpt match{
	      case Some(node) => result += (node -> vs.copy.asInstanceOf[ValueSet])
	      case None =>
	    }
    }
    result
  }
  
  private def doHashSetOperation(ipN : InvokePointNode[Node], valueSets : Map[Int, ValueSet], fac :() => ValueSet, ofg : ObjectFlowGraph[Node, ValueSet]) = {
    var result : Map[Node, ValueSet] = Map()
    require(valueSets.contains(0))
    val hsVs = valueSets(0)
    val vsOpt : Option[ValueSet] = applyHashSetOperation(ipN, hsVs, valueSets, fac)
    flowRecvAndArgs(ipN, valueSets)
    require(ipN.recvReturnNodeOpt.isDefined)
    ofg.worklist += ipN.recvReturnNodeOpt.get
    ipN.piNodeOpt match{
      case Some(node) => result += (node -> vsOpt.getOrElse(fac()).copy.asInstanceOf[ValueSet])
      case None =>
    }
    result
  }
  
  private def flowRecvAndArgs(ipN : InvokePointNode[Node], valueSets : Map[Int, ValueSet]) = {
    valueSets.foreach{
      case(i, vs) =>
        if(i == 0){
          require(ipN.recvReturnNodeOpt.isDefined)
          ipN.recvReturnNodeOpt.get.getProperty[ValueSet]("ValueSet").merge(vs.copy)
        } else {
          ipN.argReturnNodes(i-1).getProperty[ValueSet]("ValueSet").merge(vs.copy)
        }
    }
  }
  
  def getStringList(argsValueSets : Map[Int, ValueSet]) : List[List[String]] = {
    val lists = argsValueSets.toList.sortBy(_._1).map{case(k, v) => require(v.checkAndGetStrings.isDefined); v.checkAndGetStrings.get.toList}
    CombinationIterator.combinationIterator[ResourceUri](lists).toList
  }
    
  
  def isModelOperation(sig : String) : Boolean = isStringOperation(sig) || isStringBuilderOperation(sig) || isHashSetOperation(sig)
  
  def isStringBuilderOperation(sig : String) : Boolean = sig.contains("[|Ljava/lang/StringBuilder;.")
  
  def isStringOperation(sig : String) : Boolean = sig.contains("[|Ljava/lang/String;.")
  
  def isHashSetOperation(sig : String) : Boolean = sig.contains("[|Ljava/util/HashSet;.")
  
  private def applyStringOperation(sig : String, strings : List[String]) : String = {
    val size = strings.size
	  sig match {
      case "[|Ljava/lang/String;.<init>:(Ljava/lang/String;Ljava/lang/String;)V|]" =>
	      require(strings.size == 3)
	      strings(0) + strings(1) + strings(2)
	    case "[|Ljava/lang/String;.<init>:(Ljava/lang/String;)V|]" =>
	      require(strings.size == 2)
	      strings(0) + strings(1)
	    case "[|Ljava/lang/String;.<init>:(Ljava/lang/StringBuilder;)V|]" =>
	      require(strings.size == 2)
	      strings(0) + strings(1)
	    case "[|Ljava/lang/String;.<init>:(Ljava/lang/String;C)V|]" =>
	      require(strings.size == 2)
	      strings(0) + strings(1)
	    case "[|Ljava/lang/String;.<init>:([B)V|]" =>
	      require(strings.size == 1)
	      strings(0) + "bytes"
	    case "[|Ljava/lang/String;.<init>:([BLjava/nio/charset/Charset;)V|]" =>
	      require(strings.size == 2)
	      strings(0) + strings(1)
	    case "[|Ljava/lang/String;.<init>:([BIILjava/nio/charset/Charset;)V|]" =>
	      require(strings.size == 2)
	      strings(0) + strings(1)
	    case "[|Ljava/lang/String;.<init>:(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V|]" =>
	      require(strings.size == 4)
	      strings(0) + strings(1) + strings(2) + strings(3)
	    case "[|Ljava/lang/String;.<init>:([BIII)V|]" =>
	      require(strings.size == 1)
	      strings(0) + "bytes"
	    case "[|Ljava/lang/String;.<init>:(Ljava/lang/String;I)V|]" =>
	      require(strings.size == 2)
	      strings(0) + strings(1)
	    case "[|Ljava/lang/String;.<init>:([BIILjava/lang/String;)V|]" =>
	      require(strings.size == 2)
	      strings(0) + strings(1)
	    case "[|Ljava/lang/String;.<init>:([BLjava/lang/String;)V|]" =>
	      require(strings.size == 2)
	      strings(0) + strings(1)
	    case "[|Ljava/lang/String;.<init>:()V|]" =>
	      require(strings.size == 1)
	      strings(0)
	    case "[|Ljava/lang/String;.<init>:([CII)V|]" =>
	      require(strings.size == 1)
	      strings(0) + "chars"
	    case "[|Ljava/lang/String;.<init>:(Ljava/lang/StringBuffer;)V|]" =>
	      require(strings.size == 2)
	      strings(0) + strings(1)
	    case "[|Ljava/lang/String;.<init>:(II[C)V|]" =>
	      require(strings.size == 1)
	      strings(0) + "chars"
	    case "[|Ljava/lang/String;.<init>:([III)V|]" =>
	      require(strings.size == 1)
	      strings(0) + "ints"
	    case "[|Ljava/lang/String;.<init>:([C)V|]" =>
	      require(strings.size == 1)
	      strings(0) + "chars"
	    case "[|Ljava/lang/String;.<init>:([BI)V|]" =>
	      require(strings.size == 1)
	      strings(0) + "bytes"
	    case "[|Ljava/lang/String;.<init>:([BII)V|]" =>
	      require(strings.size == 1)
	      strings(0) + "bytes"
	    case "[|Ljava/lang/String;.<clinit>:()V|]" =>
	      require(strings.size == 1)
	      strings(0)
//	    case "[|Ljava/lang/String;.split:(Ljava/lang/String;I)[Ljava/lang/String;|]" =>
//	      require(strings.size == 2)
//	      strings(0)).split(strings(1))
//	    case "[|Ljava/lang/String;.split:(Ljava/lang/String;)[Ljava/lang/String;|]" =>
	    case "[|Ljava/lang/String;.trim:()Ljava/lang/String;|]" => 
	      require(strings.size == 1)
	      strings(0).trim()
	    case "[|Ljava/lang/String;.intern:()Ljava/lang/String;|]" =>
	      require(strings.size == 1)
	      strings(0).intern()
	    case "[|Ljava/lang/String;.toLowerCase:()Ljava/lang/String;|]" =>
	      require(strings.size == 1)
	      strings(0).toLowerCase()
	    case "[|Ljava/lang/String;.toUpperCase:()Ljava/lang/String;|]" =>
	      require(strings.size == 1)
	      strings(0).toUpperCase()
	    case "[|Ljava/lang/String;.replaceAll:(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;|]" =>
	      require(strings.size == 3)
	      strings(0).replaceAll(strings(1), strings(2))
	    case "[|Ljava/lang/String;.toString:()Ljava/lang/String;|]" =>
	      require(strings.size == 1)
	      strings(0)
	    case "[|Ljava/lang/String;.concat:(Ljava/lang/String;)Ljava/lang/String;|]" =>
	      require(strings.size == 2)
//	      println("1:" + strings(0) + "2:" + strings(1))
	      strings(0).concat(strings(1))
	    case "[|Ljava/lang/String;.replaceFirst:(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;|]" => 
	      require(strings.size == 3)
	      strings(0).replaceFirst(strings(1), strings(2))
	    /*
	     * [|Ljava/lang/String;.toLowerCase:(Ljava/util/Locale;)Ljava/lang/String;|]
	     * [|Ljava/lang/String;.toUpperCase:(Ljava/util/Locale;)Ljava/lang/String;|]
	     * [|Ljava/lang/String;.format:(Ljava/util/Locale;Ljava/lang/String;[Ljava/lang/Object;)Ljava/lang/String;|]
	     * [|Ljava/lang/String;.format:(Ljava/lang/String;[Ljava/lang/Object;)Ljava/lang/String;|]
	     * [|Ljava/lang/String;.replace:(Ljava/lang/CharSequence;Ljava/lang/CharSequence;)Ljava/lang/String;|]
	     */
	    /*ends here*/
	    case _ =>
	      strings(0)
    }
  }
  
	def applyStringBuilderOperation(ipN : InvokePointNode[Node], sbValueSet : ValueSet, strings : Set[String], fac :() => ValueSet, k_string : Int) : ValueSet = {
	  
	  ipN.getCalleeSig match {
	    case "[|Ljava/lang/StringBuilder;.<init>:()V|]" =>
	      sbValueSet.instances.map{
	        ins =>
	          val sIns = OFAStringInstance(new NormalType("[|java:lang:String|]"), ipN.getContext.copy, k_string)
	          sIns.setStrings(Set(""), ipN.getContext)
	          val strVs = fac()
	          strVs.addInstance(sIns)
	          ins.updateFieldDefSite("[|java:lang:StringBuilder.value|]", ipN.getContext.copy, strVs.asInstanceOf[NormalValueSet])
	          ins.setFieldLastDefSite("[|java:lang:StringBuilder.value|]", ipN.getContext.copy)
	          sbValueSet.addInstance(ins)
	      }
	      sbValueSet
	    case "[|Ljava/lang/StringBuilder;.<init>:(I)V|]" =>
	      sbValueSet.instances.map{
	        ins =>
	          val sIns = OFAStringInstance(new NormalType("[|java:lang:String|]"), ipN.getContext.copy, k_string)
	          sIns.setStrings(Set(""), ipN.getContext)
	          val strVs = fac()
	          strVs.addInstance(sIns)
	          ins.updateFieldDefSite("[|java:lang:StringBuilder.value|]", ipN.getContext.copy, strVs.asInstanceOf[NormalValueSet])
	          ins.setFieldLastDefSite("[|java:lang:StringBuilder.value|]", ipN.getContext.copy)
	          sbValueSet.addInstance(ins)
	      }
	      sbValueSet
	    case "[|Ljava/lang/StringBuilder;.<init>:(Ljava/lang/CharSequence;)V|]" =>
	      sbValueSet.instances.map{
	        ins =>
	          val sIns = OFAStringInstance(new NormalType("[|java:lang:String|]"), ipN.getContext.copy, k_string)
	          sIns.setStrings(Set(""), ipN.getContext)
	          val strVs = fac()
	          strVs.addInstance(sIns)
	          ins.updateFieldDefSite("[|java:lang:StringBuilder.value|]", ipN.getContext.copy, strVs.asInstanceOf[NormalValueSet])
	          ins.setFieldLastDefSite("[|java:lang:StringBuilder.value|]", ipN.getContext.copy)
	          sbValueSet.addInstance(ins)
	      }
	      sbValueSet
	    case "[|Ljava/lang/StringBuilder;.<init>:(Ljava/lang/String;)V|]" =>
	      sbValueSet.instances.map{
	        ins =>
	          val sIns = OFAStringInstance(new NormalType("[|java:lang:String|]"), ipN.getContext.copy, k_string)
	          sIns.setStrings(strings, ipN.getContext)
	          val strVs = fac()
	          strVs.addInstance(sIns)
	          ins.updateFieldDefSite("[|java:lang:StringBuilder.value|]", ipN.getContext.copy, strVs.asInstanceOf[NormalValueSet])
	          ins.setFieldLastDefSite("[|java:lang:StringBuilder.value|]", ipN.getContext.copy)
	          sbValueSet.addInstance(ins)
	      }
	      sbValueSet
	    case "[|Ljava/lang/StringBuilder;.append:([C)Ljava/lang/StringBuilder;|]" =>
	      sbValueSet
	    case "[|Ljava/lang/StringBuilder;.append:(C)Ljava/lang/Appendable;|]" =>
	      sbValueSet
	    case "[|Ljava/lang/StringBuilder;.append:(Ljava/lang/CharSequence;II)Ljava/lang/StringBuilder;|]" =>
	      sbValueSet
	    case "[|Ljava/lang/StringBuilder;.append:(Ljava/lang/String;)Ljava/lang/StringBuilder;|]" =>
	      sbValueSet.instances.map{
	        ins =>
	          ins.getFieldValueSet("[|java:lang:StringBuilder.value|]") match{
	            case Some(vs) =>
	              require(vs.isStringInstanceType && !strings.isEmpty)
	              val tmpVs = vs.copy
	              tmpVs.instances.foreach{
					        strIns =>
					          val strs = 
					            strIns.asInstanceOf[OFAStringInstance].getStrings.map{
					            	str1 => 
					            	  strings map{
					            	    str2 =>
					            	      println("1:" + str1 + " 2:" + str2)
					            	      str1 + str2
					            	  }
					            }.reduce((set1, set2) => set1 ++ set2)
					          strIns.asInstanceOf[OFAStringInstance].defSite = ipN.getContext.copy
					          strIns.asInstanceOf[OFAStringInstance].setStrings(strs, ipN.getContext)
					      }
	              ins.updateFieldDefSite("[|java:lang:StringBuilder.value|]", ipN.getContext.copy, tmpVs.asInstanceOf[NormalValueSet])
					      ins.setFieldLastDefSite("[|java:lang:StringBuilder.value|]", ipN.getContext.copy)
	            case None =>
	          }
	          sbValueSet.addInstance(ins)
	      }
	      sbValueSet
	    case "[|Ljava/lang/StringBuilder;.append:(Z)Ljava/lang/StringBuilder;|]" =>
	      sbValueSet
	    case "[|Ljava/lang/StringBuilder;.append:(Ljava/lang/CharSequence;)Ljava/lang/StringBuilder;|]" =>
	      sbValueSet
	    case "[|Ljava/lang/StringBuilder;.append:(Ljava/lang/Object;)Ljava/lang/StringBuilder;|]" =>
	      sbValueSet
	    case "[|Ljava/lang/StringBuilder;.append:(D)Ljava/lang/StringBuilder;|]" =>
	      sbValueSet
	    case "[|Ljava/lang/StringBuilder;.append:(J)Ljava/lang/StringBuilder;|]" =>
	      sbValueSet
	    case "[|Ljava/lang/StringBuilder;.append:(Ljava/lang/CharSequence;)Ljava/lang/Appendable;|]" =>
	      sbValueSet
	    case "[|Ljava/lang/StringBuilder;.append:(C)Ljava/lang/StringBuilder;|]" =>
	      sbValueSet
	    case "[|Ljava/lang/StringBuilder;.append:(Ljava/lang/CharSequence;II)Ljava/lang/Appendable;|]" =>
	      sbValueSet
	    case "[|Ljava/lang/StringBuilder;.reverse:()Ljava/lang/StringBuilder;|]" =>
	      sbValueSet.instances.map{
	        ins =>
	          ins.getFieldValueSet("[|java:lang:StringBuilder.value|]") match{
	            case Some(vs) =>
	              require(vs.isStringInstanceType)
	              
	              vs.instances.foreach{
					        strIns =>
					          val strings = strIns.asInstanceOf[OFAStringInstance].getStrings.map{str => str.reverse}
					          strIns.asInstanceOf[OFAStringInstance].setStrings(strings, ipN.getContext)
					          strIns.asInstanceOf[OFAStringInstance].defSite = ipN.getContext.copy
					      }
	              ins.updateFieldDefSite("[|java:lang:StringBuilder.value|]", ipN.getContext.copy, vs.asInstanceOf[NormalValueSet])
					      ins.setFieldLastDefSite("[|java:lang:StringBuilder.value|]", ipN.getContext.copy)
	            case None =>
	          }
	      }
	      sbValueSet
	    case "[|Ljava/lang/StringBuilder;.toString:()Ljava/lang/String;|]" =>
	      var insts : Set[OFAInstance] = Set()
	      sbValueSet.instances.foreach{
	        ins =>
	          ins.getFieldValueSet("[|java:lang:StringBuilder.value|]") match{
	            case Some(vs) =>
	              require(vs.isStringInstanceType)
	              vs.instances.foreach{
					        strIns =>
					          val sIns = OFAStringInstance(new NormalType("[|java:lang:String|]"), ipN.getContext.copy, k_string)
					          val strs = strIns.asInstanceOf[OFAStringInstance].getStrings
					          sIns.setStrings(strs, ipN.getContext)
					          insts += sIns
					      }
	            case None =>
	          }
	      }
	      val vs = fac()
	      vs.addInstances(insts)
	      vs
  	}
  }
	
	def applyHashSetOperation(ipN : InvokePointNode[Node], hsValueSet : ValueSet, argsValueSets : Map[Int, ValueSet], fac :() => ValueSet) : Option[ValueSet] = {
	  
	  ipN.getCalleeSig match {
	    case "[|Ljava/util/HashSet;.<init>:()V|]" =>
	      hsValueSet.instances.map{
	        ins =>
	          val vs = fac()
	          ins.updateFieldDefSite("[|java:util:HashSet.items|]", ipN.getContext.copy, vs.asInstanceOf[NormalValueSet])
	          ins.setFieldLastDefSite("[|java:util:HashSet.items|]", ipN.getContext.copy)
	      }
	      Some(hsValueSet)
	    case "[|Ljava/util/HashSet;.<init>:(I)V|]" =>
	      hsValueSet.instances.map{
	        ins =>
	          val vs = fac()
	          ins.updateFieldDefSite("[|java:util:HashSet.items|]", ipN.getContext.copy, vs.asInstanceOf[NormalValueSet])
	          ins.setFieldLastDefSite("[|java:util:HashSet.items|]", ipN.getContext.copy)
	      }
	      Some(hsValueSet)
	    case "[|Ljava/util/HashSet;.<init>:(IF)V|]" =>
	      hsValueSet.instances.map{
	        ins =>
	          val vs = fac()
	          ins.updateFieldDefSite("[|java:util:HashSet.items|]", ipN.getContext.copy, vs.asInstanceOf[NormalValueSet])
	          ins.setFieldLastDefSite("[|java:util:HashSet.items|]", ipN.getContext.copy)
	      }
	      Some(hsValueSet)
	    case "[|Ljava/util/HashSet;.<init>:(Ljava/util/Collection;)V|]" =>
//	      hsValueSet.instances.map{
//	        ins =>
//	          val vs = fac()
//	          ins.updateFieldDefSite("[|java:util:HashSet.items|]", ipN.getContext.copy, vs.asInstanceOf[NormalValueSet])
//	          ins.setFieldLastDefSite("[|java:util:HashSet.items|]", ipN.getContext.copy)
//	      }
	      Some(hsValueSet)
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
	      Some(hsValueSet)
	    case "[|Ljava/util/HashSet;.readObject:(Ljava/io/ObjectInputStream;)V|]" =>
	      None
	    case "[|Ljava/util/HashSet;.writeObject:(Ljava/io/ObjectOutputStream;)V|]" =>
	      None
	    case "[|Ljava/util/HashSet;.add:(Ljava/lang/Object;)Z|]" =>
	      hsValueSet.instances.map{
	        ins =>
	          val vs = ins.getFieldValueSet("[|java:util:HashSet.items|]") match{
	            				 case Some(fieldVs) => fieldVs.copy
	            				 case None => fac()
	          				 }
	          require(argsValueSets.contains(1))
	          vs.addInstances(argsValueSets(1).instances)
	          ins.updateFieldDefSite("[|java:util:HashSet.items|]", ipN.getContext.copy, vs.asInstanceOf[NormalValueSet])
	          ins.setFieldLastDefSite("[|java:util:HashSet.items|]", ipN.getContext.copy)
	      }
	      Some(hsValueSet)
	    case "[|Ljava/util/HashSet;.clear:()V|]" => 
	      hsValueSet.instances.map{
	        ins =>
	          val vs = fac()
	          ins.updateFieldDefSite("[|java:util:HashSet.items|]", ipN.getContext.copy, vs.asInstanceOf[NormalValueSet])
	          ins.setFieldLastDefSite("[|java:util:HashSet.items|]", ipN.getContext.copy)
	      }
	      Some(hsValueSet)
	    case "[|Ljava/util/HashSet;.clone:()Ljava/lang/Object;|]" =>
	       hsValueSet.instances.map{
	        ins =>
	          val vs = ins.getFieldValueSet("[|java:util:HashSet.items|]") match{
	            				 case Some(fieldVs) => fieldVs.copy
	            				 case None => fac()
	          				 }
	          ins.updateFieldDefSite("[|java:util:HashSet.items|]", ipN.getContext.copy, vs.asInstanceOf[NormalValueSet])
	          ins.setFieldLastDefSite("[|java:util:HashSet.items|]", ipN.getContext.copy)
	      }
	      Some(hsValueSet)
	    case "[|Ljava/util/HashSet;.contains:(Ljava/lang/Object;)Z|]" =>
	      None
	    case "[|Ljava/util/HashSet;.createBackingMap:(IF)Ljava/util/HashMap;|]" =>
	      None
	    case "[|Ljava/util/HashSet;.isEmpty:()Z|]" =>
	      None
	    case "[|Ljava/util/HashSet;.iterator:()Ljava/util/Iterator;|]" =>
	      None
	    case "[|Ljava/util/HashSet;.remove:(Ljava/lang/Object;)Z|]" =>
	      hsValueSet.instances.map{
	        ins =>
	          val vs = ins.getFieldValueSet("[|java:util:HashSet.items|]") match{
	            				 case Some(fieldVs) => fieldVs.copy
	            				 case None => fac()
	          				 }
	          require(argsValueSets.contains(1))
	          vs.removeInstances(argsValueSets(1).instances)
	          ins.updateFieldDefSite("[|java:util:HashSet.items|]", ipN.getContext.copy, vs.asInstanceOf[NormalValueSet])
	          ins.setFieldLastDefSite("[|java:util:HashSet.items|]", ipN.getContext.copy)
	      }
	      Some(hsValueSet)
	    case "[|Ljava/util/HashSet;.size:()I|]" =>
	      None
	  }
	}
}