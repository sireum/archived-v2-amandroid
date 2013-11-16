package org.sireum.amandroid.interProcedural.reachingFactsAnalysis.model

import org.sireum.amandroid._
import org.sireum.util._
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis._
import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.ReachingFactsAnalysisHelper

object HashMapModel {
	def isHashMap(r : AmandroidRecord) : Boolean = r.getName == "[|java:util:HashMap|]"
	
	private def getPointStringToRet(retVar : String, currentContext : Context): RFAFact = {
    val newThisValue = RFAPointStringInstance(currentContext.copy)
    RFAFact(VarSlot(retVar), newThisValue)	 
	}
	  
	private def cloneHashMap(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >0)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  thisValue.map{s => RFAFact(VarSlot(retVar), s.clone(currentContext))}
  }
	
	private def getHashMapEntrySetFactToRet(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
	  var result = isetEmpty[RFAFact]
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >0)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val strValue = thisValue.map{ins => factMap.getOrElse(FieldSlot(ins, "[|java:util:HashMap.entrys|]"), isetEmpty)}.reduce(iunion[Instance])
	  val rf = ReachingFactsAnalysisHelper.getReturnFact(NormalType("[|java:util:HashSet|]", 0), retVar, currentContext).get
	  result += rf
	  result ++= strValue.map{s => RFAFact(FieldSlot(rf.v, "[|java:util:HashSet.items|]"), s)}
	  result
  }
	
	private def getHashMapKeySetToRet(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
	  var result = isetEmpty[RFAFact]
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >0)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val strValue = thisValue.map{ins => factMap.getOrElse(FieldSlot(ins, "[|java:util:HashMap.entrys|]"), isetEmpty)}.reduce(iunion[Instance])
	  val rf = ReachingFactsAnalysisHelper.getReturnFact(NormalType("[|java:util:HashSet|]", 0), retVar, currentContext).get
	  result += rf
	  result ++= strValue.map{
	    s => 
	      require(s.isInstanceOf[RFATupleInstance])
	      RFAFact(FieldSlot(rf.v, "[|java:util:HashSet.items|]"), s.asInstanceOf[RFATupleInstance].left)
	  }
	  result
  }
	
	private def getHashMapValuesToRet(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
	  var result = isetEmpty[RFAFact]
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >0)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val strValue = thisValue.map{ins => factMap.getOrElse(FieldSlot(ins, "[|java:util:HashMap.entrys|]"), isetEmpty)}.reduce(iunion[Instance])
	  val rf = ReachingFactsAnalysisHelper.getReturnFact(NormalType("[|java:util:HashSet|]", 0), retVar, currentContext).get
	  result += rf
	  result ++= strValue.map{
	    s => 
	      require(s.isInstanceOf[RFATupleInstance])
	      RFAFact(FieldSlot(rf.v, "[|java:util:HashSet.items|]"), s.asInstanceOf[RFATupleInstance].right)
	  }
	  result
  }
	
	private def getHashMapValue(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
	  var result = isetEmpty[RFAFact]
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >1)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val keySlot = VarSlot(args(1))
	  val keyValue = factMap.getOrElse(keySlot, isetEmpty)
	  val entValue = thisValue.map{ins => factMap.getOrElse(FieldSlot(ins, "[|java:util:HashMap.entrys|]"), isetEmpty)}.reduce(iunion[Instance])
	  entValue.foreach{
	    v =>
	      require(v.isInstanceOf[RFATupleInstance])
	      if(keyValue.contains(v.asInstanceOf[RFATupleInstance].left)){
	        result += (RFAFact(VarSlot(retVar), v.asInstanceOf[RFATupleInstance].right))
	      }
	  }
	  result
  } 
	
	private def putHashMapValue(s : ISet[RFAFact], args : List[String], currentContext : Context) : ISet[RFAFact] ={
	  var result = isetEmpty[RFAFact]
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >2)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val keySlot = VarSlot(args(1))
	  val keyValue = factMap.getOrElse(keySlot, isetEmpty)
	  val valueSlot = VarSlot(args(2))
	  val valueValue = factMap.getOrElse(valueSlot, isetEmpty)
	  var entrys = isetEmpty[Instance]
	  keyValue.foreach{
	    kv =>
	      valueValue.foreach{
	        vv =>
	          entrys += RFATupleInstance(kv, vv, currentContext)
	      }
	  }
	  thisValue.foreach{
	    ins =>
	      result ++= entrys.map(e => RFAFact(FieldSlot(ins, "[|java:util:HashMap.entrys|]"), e))
	  }
	  result
  }
	
	private def putAllHashMapValues(s : ISet[RFAFact], args : List[String], currentContext : Context) : ISet[RFAFact] ={
	  var result = isetEmpty[RFAFact]
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >1)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val slot2 = VarSlot(args(1))
	  val value2 = factMap.getOrElse(slot2, isetEmpty)
	  thisValue.foreach{
	    ins =>
	      result ++= value2.map(e => RFAFact(FieldSlot(ins, "[|java:util:HashMap.entrys|]"), e))
	  }
	  result
  }
	
	def doHashMapCall(s : ISet[RFAFact], p : AmandroidProcedure, args : List[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact], Boolean) = {
	  var newFacts = isetEmpty[RFAFact]
	  var delFacts = isetEmpty[RFAFact]
	  var byPassFlag = true
	  p.getSignature match{
	    case "[|Ljava/util/HashMap;.<clinit>:()V|]" =>
		  case "[|Ljava/util/HashMap;.<init>:()V|]" =>
		  case "[|Ljava/util/HashMap;.<init>:(I)V|]" =>
		  case "[|Ljava/util/HashMap;.<init>:(IF)V|]" =>
		  case "[|Ljava/util/HashMap;.<init>:(Ljava/util/Map;)V|]" =>
		  case "[|Ljava/util/HashMap;.access$600:(Ljava/util/HashMap;Ljava/lang/Object;Ljava/lang/Object;)Z|]" =>
		  case "[|Ljava/util/HashMap;.access$700:(Ljava/util/HashMap;Ljava/lang/Object;Ljava/lang/Object;)Z|]" =>
		  case "[|Ljava/util/HashMap;.addNewEntry:(Ljava/lang/Object;Ljava/lang/Object;II)V|]" =>
		  case "[|Ljava/util/HashMap;.addNewEntryForNullKey:(Ljava/lang/Object;)V|]" =>
		  case "[|Ljava/util/HashMap;.capacityForInitSize:(I)I|]" =>
		  case "[|Ljava/util/HashMap;.clear:()V|]" =>
		  case "[|Ljava/util/HashMap;.clone:()Ljava/lang/Object;|]" =>
		    require(retVars.size == 1)
		    newFacts ++= cloneHashMap(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "[|Ljava/util/HashMap;.constructorNewEntry:(Ljava/lang/Object;Ljava/lang/Object;ILjava/util/HashMap$HashMapEntry;)Ljava/util/HashMap$HashMapEntry;|]" =>
		  case "[|Ljava/util/HashMap;.constructorPut:(Ljava/lang/Object;Ljava/lang/Object;)V|]" =>
		  case "[|Ljava/util/HashMap;.constructorPutAll:(Ljava/util/Map;)V|]" =>
		  case "[|Ljava/util/HashMap;.containsKey:(Ljava/lang/Object;)Z|]" =>
		  case "[|Ljava/util/HashMap;.containsMapping:(Ljava/lang/Object;Ljava/lang/Object;)Z|]" =>
		  case "[|Ljava/util/HashMap;.containsValue:(Ljava/lang/Object;)Z|]" =>
		  case "[|Ljava/util/HashMap;.doubleCapacity:()[Ljava/util/HashMap$HashMapEntry;|]" =>
		  case "[|Ljava/util/HashMap;.ensureCapacity:(I)V|]" =>
		  case "[|Ljava/util/HashMap;.entrySet:()Ljava/util/Set;|]" =>
		    require(retVars.size == 1)
		    newFacts ++= getHashMapEntrySetFactToRet(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "[|Ljava/util/HashMap;.get:(Ljava/lang/Object;)Ljava/lang/Object;|]" =>
		    require(retVars.size == 1)
		    newFacts ++= getHashMapValue(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "[|Ljava/util/HashMap;.init:()V|]" =>
		  case "[|Ljava/util/HashMap;.isEmpty:()Z|]" =>
		  case "[|Ljava/util/HashMap;.keySet:()Ljava/util/Set;|]" =>
		    require(retVars.size == 1)
		    newFacts ++= getHashMapKeySetToRet(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "[|Ljava/util/HashMap;.makeTable:(I)[Ljava/util/HashMap$HashMapEntry;|]" =>
		  case "[|Ljava/util/HashMap;.newEntryIterator:()Ljava/util/Iterator;|]" =>
		  case "[|Ljava/util/HashMap;.newKeyIterator:()Ljava/util/Iterator;|]" =>
		  case "[|Ljava/util/HashMap;.newValueIterator:()Ljava/util/Iterator;|]" =>
		  case "[|Ljava/util/HashMap;.postRemove:(Ljava/util/HashMap$HashMapEntry;)V|]" =>
		  case "[|Ljava/util/HashMap;.preModify:(Ljava/util/HashMap$HashMapEntry;)V|]" =>
		  case "[|Ljava/util/HashMap;.put:(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;|]" =>
		    newFacts ++= putHashMapValue(s, args, currentContext)
		    byPassFlag = false
		  case "[|Ljava/util/HashMap;.putAll:(Ljava/util/Map;)V|]" =>
		    newFacts ++= putAllHashMapValues(s, args, currentContext)
		    byPassFlag = false
		  case "[|Ljava/util/HashMap;.putValueForNullKey:(Ljava/lang/Object;)Ljava/lang/Object;|]" =>
		  case "[|Ljava/util/HashMap;.readObject:(Ljava/io/ObjectInputStream;)V|]" =>
		  case "[|Ljava/util/HashMap;.remove:(Ljava/lang/Object;)Ljava/lang/Object;|]" =>
		    require(retVars.size == 1)
		    newFacts ++= getHashMapValue(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "[|Ljava/util/HashMap;.removeMapping:(Ljava/lang/Object;Ljava/lang/Object;)Z|]" =>
		  case "[|Ljava/util/HashMap;.removeNullKey:()Ljava/lang/Object;|]" =>
		  case "[|Ljava/util/HashMap;.secondaryHash:(Ljava/lang/Object;)I|]" =>
		  case "[|Ljava/util/HashMap;.size:()I|]" =>
		  case "[|Ljava/util/HashMap;.values:()Ljava/util/Collection;|]" =>
		    require(retVars.size == 1)
		    newFacts ++= getHashMapValuesToRet(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "[|Ljava/util/HashMap;.writeObject:(Ljava/io/ObjectOutputStream;)V|]" =>
	  }
	  (newFacts, delFacts, byPassFlag)
	}
}