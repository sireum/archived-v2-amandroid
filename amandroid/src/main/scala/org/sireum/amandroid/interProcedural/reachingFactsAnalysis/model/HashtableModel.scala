package org.sireum.amandroid.interProcedural.reachingFactsAnalysis.model

import org.sireum.amandroid.AmandroidRecord
import org.sireum.amandroid.AmandroidProcedure
import org.sireum.util._
import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis._
import org.sireum.amandroid._

/**
 * @author Fengguo Wei & Sankardas Roy
 */
object HashtableModel {
	def isHashtable(r : AmandroidRecord) : Boolean = r.getName == "[|java:util:Hashtable|]"
	
	private def getPointStringToRet(retVar : String, currentContext : Context): RFAFact = {
    val newThisValue = RFAPointStringInstance(currentContext.copy)
    RFAFact(VarSlot(retVar), newThisValue)	 
	}
	  
	private def getInstanceFromType(typ : Type, currentContext : Context) : Option[Instance] = {
	  if(Center.isJavaPrimitiveType(typ) || typ.typ == "[|void|]") None
	  else if(typ.typ == "[|java:lang:String|]") Some(RFAPointStringInstance(currentContext))
	  else Some(RFAInstance(typ, currentContext))
	}
	  
	private def getReturnFact(rType : Type, retVar : String, currentContext : Context) : Option[RFAFact] = {
	  val insOpt = getInstanceFromType(rType, currentContext)
	  if(insOpt.isDefined){
	    Some(RFAFact(VarSlot(retVar), insOpt.get))
	  } else None
	}
	  
	private def cloneHashTable(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >0)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  thisValue.map{s => RFAFact(VarSlot(retVar), s.clone(currentContext))}
  }
	
	private def getHashTableEntrySetFactToRet(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
	  var result = isetEmpty[RFAFact]
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >0)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val strValue = thisValue.map{ins => factMap.getOrElse(FieldSlot(ins, "[|java:util:Hashtable.entrys|]"), isetEmpty)}.reduce(iunion[Instance])
	  val rf = getReturnFact(NormalType("[|java:util:HashSet|]", 0), retVar, currentContext).get
	  result += rf
	  result ++= strValue.map{s => RFAFact(FieldSlot(rf.v, "[|java:util:HashSet.items|]"), s)}
	  result
  }
	
	private def getHashTableKeySetToRet(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
	  var result = isetEmpty[RFAFact]
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >0)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val strValue = thisValue.map{ins => factMap.getOrElse(FieldSlot(ins, "[|java:util:Hashtable.entrys|]"), isetEmpty)}.reduce(iunion[Instance])
	  val rf = getReturnFact(NormalType("[|java:util:HashSet|]", 0), retVar, currentContext).get
	  result += rf
	  result ++= strValue.map{
	    s => 
	      require(s.isInstanceOf[RFATupleInstance])
	      RFAFact(FieldSlot(rf.v, "[|java:util:HashSet.items|]"), s.asInstanceOf[RFATupleInstance].left)
	  }
	  result
  }
	
	private def getHashTableValuesToRet(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
	  var result = isetEmpty[RFAFact]
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >0)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val strValue = thisValue.map{ins => factMap.getOrElse(FieldSlot(ins, "[|java:util:Hashtable.entrys|]"), isetEmpty)}.reduce(iunion[Instance])
	  val rf = getReturnFact(NormalType("[|java:util:HashSet|]", 0), retVar, currentContext).get
	  result += rf
	  result ++= strValue.map{
	    s => 
	      require(s.isInstanceOf[RFATupleInstance])
	      RFAFact(FieldSlot(rf.v, "[|java:util:HashSet.items|]"), s.asInstanceOf[RFATupleInstance].right)
	  }
	  result
  }
	
	private def getHashTableValue(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
	  var result = isetEmpty[RFAFact]
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >1)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val keySlot = VarSlot(args(1))
	  val keyValue = factMap.getOrElse(keySlot, isetEmpty)
	  val entValue = thisValue.map{ins => factMap.getOrElse(FieldSlot(ins, "[|java:util:Hashtable.entrys|]"), isetEmpty)}.reduce(iunion[Instance])
	  entValue.foreach{
	    v =>
	      require(v.isInstanceOf[RFATupleInstance])
	      if(keyValue.contains(v.asInstanceOf[RFATupleInstance].left)){
	        result += (RFAFact(VarSlot(retVar), v.asInstanceOf[RFATupleInstance].right))
	      }
	  }
	  result
  }
	
	private def putHashTableValue(s : ISet[RFAFact], args : List[String], currentContext : Context) : ISet[RFAFact] ={
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
	      result ++= entrys.map(e => RFAFact(FieldSlot(ins, "[|java:util:Hashtable.entrys|]"), e))
	  }
	  result
  }
	
	private def putAllHashTableValues(s : ISet[RFAFact], args : List[String], currentContext : Context) : ISet[RFAFact] ={
	  var result = isetEmpty[RFAFact]
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >1)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val slot2 = VarSlot(args(1))
	  val value2 = factMap.getOrElse(slot2, isetEmpty)
	  thisValue.foreach{
	    ins =>
	      result ++= value2.map(e => RFAFact(FieldSlot(ins, "[|java:util:Hashtable.entrys|]"), e))
	  }
	  result
  }
	  
	def doHashtableCall(s : ISet[RFAFact], p : AmandroidProcedure, args : List[String], retVarOpt : Option[String], currentContext : Context) : ISet[RFAFact] = {
	  var newFacts = isetEmpty[RFAFact]
	  p.getSignature match{
	    case "[|Ljava/util/Hashtable;.<clinit>:()V|]" =>
		  case "[|Ljava/util/Hashtable;.<init>:()V|]" =>
		  case "[|Ljava/util/Hashtable;.<init>:(I)V|]" =>
		  case "[|Ljava/util/Hashtable;.<init>:(IF)V|]" =>
		  case "[|Ljava/util/Hashtable;.<init>:(Ljava/util/Map;)V|]" =>
		  case "[|Ljava/util/Hashtable;.access$1100:(Ljava/util/Hashtable;Ljava/lang/Object;Ljava/lang/Object;)Z|]" =>
		  case "[|Ljava/util/Hashtable;.access$1200:(Ljava/util/Hashtable;Ljava/lang/Object;Ljava/lang/Object;)Z|]" =>
		  case "[|Ljava/util/Hashtable;.access$500:(Ljava/util/Hashtable;)I|]" =>
		  case "[|Ljava/util/Hashtable;.access$600:(Ljava/util/Hashtable;)[Ljava/util/Hashtable$HashtableEntry;|]" =>
		  case "[|Ljava/util/Hashtable;.access$800:(Ljava/util/Hashtable;)I|]" =>
		  case "[|Ljava/util/Hashtable;.capacityForInitSize:(I)I|]" =>
		  case "[|Ljava/util/Hashtable;.clear:()V|]" =>
		  case "[|Ljava/util/Hashtable;.clone:()Ljava/lang/Object;|]" =>
		    require(retVarOpt.isDefined)
		    newFacts ++= cloneHashTable(s, args, retVarOpt.get, currentContext)
		  case "[|Ljava/util/Hashtable;.constructorPut:(Ljava/lang/Object;Ljava/lang/Object;)V|]" =>
		  case "[|Ljava/util/Hashtable;.constructorPutAll:(Ljava/util/Map;)V|]" =>
		  case "[|Ljava/util/Hashtable;.contains:(Ljava/lang/Object;)Z|]" =>
		  case "[|Ljava/util/Hashtable;.containsKey:(Ljava/lang/Object;)Z|]" =>
		  case "[|Ljava/util/Hashtable;.containsMapping:(Ljava/lang/Object;Ljava/lang/Object;)Z|]" =>
		  case "[|Ljava/util/Hashtable;.containsValue:(Ljava/lang/Object;)Z|]" =>
		  case "[|Ljava/util/Hashtable;.doubleCapacity:()[Ljava/util/Hashtable$HashtableEntry;|]" =>
		  case "[|Ljava/util/Hashtable;.elements:()Ljava/util/Enumeration;|]" =>
		  case "[|Ljava/util/Hashtable;.ensureCapacity:(I)V|]" =>
		  case "[|Ljava/util/Hashtable;.entrySet:()Ljava/util/Set;|]" =>
		    require(retVarOpt.isDefined)
		    newFacts ++= getHashTableEntrySetFactToRet(s, args, retVarOpt.get, currentContext)
		  case "[|Ljava/util/Hashtable;.equals:(Ljava/lang/Object;)Z|]" =>
		  case "[|Ljava/util/Hashtable;.get:(Ljava/lang/Object;)Ljava/lang/Object;|]" =>
		    require(retVarOpt.isDefined)
		    newFacts ++= getHashTableValue(s, args, retVarOpt.get, currentContext)
		  case "[|Ljava/util/Hashtable;.hashCode:()I|]" =>
		  case "[|Ljava/util/Hashtable;.isEmpty:()Z|]" =>
		  case "[|Ljava/util/Hashtable;.keySet:()Ljava/util/Set;|]" =>
		    require(retVarOpt.isDefined)
		    newFacts ++= getHashTableKeySetToRet(s, args, retVarOpt.get, currentContext)
		  case "[|Ljava/util/Hashtable;.keys:()Ljava/util/Enumeration;|]" =>
		  case "[|Ljava/util/Hashtable;.makeTable:(I)[Ljava/util/Hashtable$HashtableEntry;|]" =>
		  case "[|Ljava/util/Hashtable;.put:(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;|]" =>
		    newFacts ++= putHashTableValue(s, args, currentContext)
		  case "[|Ljava/util/Hashtable;.putAll:(Ljava/util/Map;)V|]" =>
		    newFacts ++= putAllHashTableValues(s, args, currentContext)
		  case "[|Ljava/util/Hashtable;.readObject:(Ljava/io/ObjectInputStream;)V|]" =>
		  case "[|Ljava/util/Hashtable;.rehash:()V|]" =>
		  case "[|Ljava/util/Hashtable;.remove:(Ljava/lang/Object;)Ljava/lang/Object;|]" =>
		    require(retVarOpt.isDefined)
		    newFacts ++= getHashTableValue(s, args, retVarOpt.get, currentContext)
		  case "[|Ljava/util/Hashtable;.removeMapping:(Ljava/lang/Object;Ljava/lang/Object;)Z|]" =>
		  case "[|Ljava/util/Hashtable;.size:()I|]" =>
		  case "[|Ljava/util/Hashtable;.toString:()Ljava/lang/String;|]" =>
		    require(retVarOpt.isDefined)
		    newFacts += getPointStringToRet(retVarOpt.get, currentContext)
		  case "[|Ljava/util/Hashtable;.values:()Ljava/util/Collection;|]" =>
		    require(retVarOpt.isDefined)
		    newFacts ++= getHashTableValuesToRet(s, args, retVarOpt.get, currentContext)
		  case "[|Ljava/util/Hashtable;.writeObject:(Ljava/io/ObjectOutputStream;)V|]" =>
	  }
	  s ++ newFacts
	}
}