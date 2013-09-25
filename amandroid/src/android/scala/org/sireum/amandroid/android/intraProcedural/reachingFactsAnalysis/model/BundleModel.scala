package org.sireum.amandroid.android.intraProcedural.reachingFactsAnalysis.model

import org.sireum.amandroid._
import org.sireum.util._
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAFact
import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.ReachingFactsAnalysisHelper
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.VarSlot
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.FieldSlot
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFATupleInstance
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAPointStringInstance
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAInstance
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAConcreteStringInstance
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAPointStringInstance

/**
 * @author Fengguo Wei & Sankardas Roy
 */
object BundleModel {
	def isBundle(r : AmandroidRecord) : Boolean = r.getName == "[|android:os:Bundle|]"
	  
	def doBundleCall(s : ISet[RFAFact], p : AmandroidProcedure, args : List[String], retVarOpt : Option[String], currentContext : Context) : ISet[RFAFact] = {
	  var newFacts = isetEmpty[RFAFact]
	  p.getSignature match{
	    case "[|Landroid/os/Bundle;.<clinit>:()V|]" =>  //static constructor
		  case "[|Landroid/os/Bundle;.<init>:()V|]" =>  //public constructor
		  case "[|Landroid/os/Bundle;.<init>:(I)V|]" =>  //public constructor
		  case "[|Landroid/os/Bundle;.<init>:(Landroid/os/Bundle;)V|]" =>  //public constructor
		    require(retVarOpt.isDefined)
		    newFacts ++= initBundleFromBundle(s, args, retVarOpt.get, currentContext)
		  case "[|Landroid/os/Bundle;.<init>:(Landroid/os/Parcel;)V|]" =>  //constructor
		  case "[|Landroid/os/Bundle;.<init>:(Landroid/os/Parcel;I)V|]" =>  //constructor
		  case "[|Landroid/os/Bundle;.<init>:(Ljava/lang/ClassLoader;)V|]" =>  //public constructor
		  case "[|Landroid/os/Bundle;.clear:()V|]" =>  //public
		  case "[|Landroid/os/Bundle;.clone:()Ljava/lang/Object;|]" =>  //public
		    require(retVarOpt.isDefined)
		    newFacts ++= cloneBundle(s, args, retVarOpt.get, currentContext)
		  case "[|Landroid/os/Bundle;.containsKey:(Ljava/lang/String;)Z|]" =>  //public
		  case "[|Landroid/os/Bundle;.describeContents:()I|]" =>  //public
		  case "[|Landroid/os/Bundle;.forPair:(Ljava/lang/String;Ljava/lang/String;)Landroid/os/Bundle;|]" =>  //public static
		    require(retVarOpt.isDefined)
		    newFacts ++= forPair(s, args, retVarOpt.get, currentContext)
		  case "[|Landroid/os/Bundle;.get:(Ljava/lang/String;)Ljava/lang/Object;|]" =>  //public
		    require(retVarOpt.isDefined)
		    newFacts ++= getBundleValue(s, args, retVarOpt.get, currentContext)
		  case "[|Landroid/os/Bundle;.getBoolean:(Ljava/lang/String;)Z|]" =>  //public
		  case "[|Landroid/os/Bundle;.getBoolean:(Ljava/lang/String;Z)Z|]" =>  //public
		  case "[|Landroid/os/Bundle;.getBooleanArray:(Ljava/lang/String;)[Z|]" =>  //public
		  case "[|Landroid/os/Bundle;.getBundle:(Ljava/lang/String;)Landroid/os/Bundle;|]" =>  //public
		    require(retVarOpt.isDefined)
		    newFacts ++= getBundleValue(s, args, retVarOpt.get, currentContext)
		  case "[|Landroid/os/Bundle;.getByte:(Ljava/lang/String;)B|]" =>  //public
		  case "[|Landroid/os/Bundle;.getByte:(Ljava/lang/String;B)Ljava/lang/Byte;|]" =>  //public
		  case "[|Landroid/os/Bundle;.getByteArray:(Ljava/lang/String;)[B|]" =>  //public
		  case "[|Landroid/os/Bundle;.getChar:(Ljava/lang/String;)C|]" =>  //public
		  case "[|Landroid/os/Bundle;.getChar:(Ljava/lang/String;C)C|]" =>  //public
		  case "[|Landroid/os/Bundle;.getCharArray:(Ljava/lang/String;)[C|]" =>  //public
		  case "[|Landroid/os/Bundle;.getCharSequence:(Ljava/lang/String;)Ljava/lang/CharSequence;|]" =>  //public
		    require(retVarOpt.isDefined)
		    newFacts ++= getBundleValue(s, args, retVarOpt.get, currentContext)
		  case "[|Landroid/os/Bundle;.getCharSequence:(Ljava/lang/String;Ljava/lang/CharSequence;)Ljava/lang/CharSequence;|]" =>  //public
		    require(retVarOpt.isDefined)
		    newFacts ++= getBundleValueWithDefault(s, args, retVarOpt.get, currentContext)
		  case "[|Landroid/os/Bundle;.getCharSequenceArray:(Ljava/lang/String;)[Ljava/lang/CharSequence;|]" =>  //public
		    require(retVarOpt.isDefined)
		    newFacts ++= getBundleValue(s, args, retVarOpt.get, currentContext)
		  case "[|Landroid/os/Bundle;.getCharSequenceArrayList:(Ljava/lang/String;)Ljava/util/ArrayList;|]" =>  //public
		    require(retVarOpt.isDefined)
		    newFacts ++= getBundleValue(s, args, retVarOpt.get, currentContext)
		  case "[|Landroid/os/Bundle;.getClassLoader:()Ljava/lang/ClassLoader;|]" =>  //public
		  case "[|Landroid/os/Bundle;.getDouble:(Ljava/lang/String;)D|]" =>  //public
		  case "[|Landroid/os/Bundle;.getDouble:(Ljava/lang/String;D)D|]" =>  //public
		  case "[|Landroid/os/Bundle;.getDoubleArray:(Ljava/lang/String;)[D|]" =>  //public
		  case "[|Landroid/os/Bundle;.getFloat:(Ljava/lang/String;)F|]" =>  //public
		  case "[|Landroid/os/Bundle;.getFloat:(Ljava/lang/String;F)F|]" =>  //public
		  case "[|Landroid/os/Bundle;.getFloatArray:(Ljava/lang/String;)[F|]" =>  //public
		  case "[|Landroid/os/Bundle;.getIBinder:(Ljava/lang/String;)Landroid/os/IBinder;|]" =>  //public
		    require(retVarOpt.isDefined)
		    newFacts ++= getBundleValue(s, args, retVarOpt.get, currentContext)
		  case "[|Landroid/os/Bundle;.getInt:(Ljava/lang/String;)I|]" =>  //public
		  case "[|Landroid/os/Bundle;.getInt:(Ljava/lang/String;I)I|]" =>  //public
		  case "[|Landroid/os/Bundle;.getIntArray:(Ljava/lang/String;)[I|]" =>  //public
		  case "[|Landroid/os/Bundle;.getIntegerArrayList:(Ljava/lang/String;)Ljava/util/ArrayList;|]" =>  //public
		    require(retVarOpt.isDefined)
		    newFacts ++= getBundleValue(s, args, retVarOpt.get, currentContext)
		  case "[|Landroid/os/Bundle;.getLong:(Ljava/lang/String;)J|]" =>  //public
		  case "[|Landroid/os/Bundle;.getLong:(Ljava/lang/String;J)J|]" =>  //public
		  case "[|Landroid/os/Bundle;.getLongArray:(Ljava/lang/String;)[J|]" =>  //public
		  case "[|Landroid/os/Bundle;.getPairValue:()Ljava/lang/String;|]" =>  //public
		  case "[|Landroid/os/Bundle;.getParcelable:(Ljava/lang/String;)Landroid/os/Parcelable;|]" =>  //public
		    require(retVarOpt.isDefined)
		    newFacts ++= getBundleValue(s, args, retVarOpt.get, currentContext)
		  case "[|Landroid/os/Bundle;.getParcelableArray:(Ljava/lang/String;)[Landroid/os/Parcelable;|]" =>  //public
		    require(retVarOpt.isDefined)
		    newFacts ++= getBundleValue(s, args, retVarOpt.get, currentContext)
		  case "[|Landroid/os/Bundle;.getParcelableArrayList:(Ljava/lang/String;)Ljava/util/ArrayList;|]" =>  //public
		    require(retVarOpt.isDefined)
		    newFacts ++= getBundleValue(s, args, retVarOpt.get, currentContext)
		  case "[|Landroid/os/Bundle;.getSerializable:(Ljava/lang/String;)Ljava/io/Serializable;|]" =>  //public
		    require(retVarOpt.isDefined)
		    newFacts ++= getBundleValue(s, args, retVarOpt.get, currentContext)
		  case "[|Landroid/os/Bundle;.getShort:(Ljava/lang/String;)S|]" =>  //public
		  case "[|Landroid/os/Bundle;.getShort:(Ljava/lang/String;S)S|]" =>  //public
		  case "[|Landroid/os/Bundle;.getShortArray:(Ljava/lang/String;)[S|]" =>  //public
		  case "[|Landroid/os/Bundle;.getSparseParcelableArray:(Ljava/lang/String;)Landroid/util/SparseArray;|]" =>  //public
		    require(retVarOpt.isDefined)
		    newFacts ++= getBundleValue(s, args, retVarOpt.get, currentContext)
		  case "[|Landroid/os/Bundle;.getString:(Ljava/lang/String;)Ljava/lang/String;|]" =>  //public
		    require(retVarOpt.isDefined)
		    newFacts ++= getBundleValue(s, args, retVarOpt.get, currentContext)
		  case "[|Landroid/os/Bundle;.getString:(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;|]" =>  //public
		    require(retVarOpt.isDefined)
		    newFacts ++= getBundleValueWithDefault(s, args, retVarOpt.get, currentContext)
		  case "[|Landroid/os/Bundle;.getStringArray:(Ljava/lang/String;)[Ljava/lang/String;|]" =>  //public
		    require(retVarOpt.isDefined)
		    newFacts ++= getBundleValue(s, args, retVarOpt.get, currentContext)
		  case "[|Landroid/os/Bundle;.getStringArrayList:(Ljava/lang/String;)Ljava/util/ArrayList;|]" =>  //public
		    require(retVarOpt.isDefined)
		    newFacts ++= getBundleValue(s, args, retVarOpt.get, currentContext)
		  case "[|Landroid/os/Bundle;.hasFileDescriptors:()Z|]" =>  //public
		  case "[|Landroid/os/Bundle;.isEmpty:()Z|]" =>  //public
		  case "[|Landroid/os/Bundle;.isParcelled:()Z|]" =>  //public
		  case "[|Landroid/os/Bundle;.keySet:()Ljava/util/Set;|]" =>  //public
		    require(retVarOpt.isDefined)
		    newFacts ++= getBundleKeySetToRet(s, args, retVarOpt.get, currentContext)
		  case "[|Landroid/os/Bundle;.putAll:(Landroid/os/Bundle;)V|]" =>  //public
		    newFacts ++= putAllBundleValues(s, args, currentContext)
		  case "[|Landroid/os/Bundle;.putBoolean:(Ljava/lang/String;Z)V|]" =>  //public
		  case "[|Landroid/os/Bundle;.putBooleanArray:(Ljava/lang/String;[Z)V|]" =>  //public
		  case "[|Landroid/os/Bundle;.putBundle:(Ljava/lang/String;Landroid/os/Bundle;)V|]" =>  //public
		    newFacts ++= putBundleValue(s, args, currentContext)
		  case "[|Landroid/os/Bundle;.putByte:(Ljava/lang/String;B)V|]" =>  //public
		  case "[|Landroid/os/Bundle;.putByteArray:(Ljava/lang/String;[B)V|]" =>  //public
		  case "[|Landroid/os/Bundle;.putChar:(Ljava/lang/String;C)V|]" =>  //public
		  case "[|Landroid/os/Bundle;.putCharArray:(Ljava/lang/String;[C)V|]" =>  //public
		  case "[|Landroid/os/Bundle;.putCharSequence:(Ljava/lang/String;Ljava/lang/CharSequence;)V|]" =>  //public
		    newFacts ++= putBundleValue(s, args, currentContext)
		  case "[|Landroid/os/Bundle;.putCharSequenceArray:(Ljava/lang/String;[Ljava/lang/CharSequence;)V|]" =>  //public
		    newFacts ++= putBundleValue(s, args, currentContext)
		  case "[|Landroid/os/Bundle;.putCharSequenceArrayList:(Ljava/lang/String;Ljava/util/ArrayList;)V|]" =>  //public
		    newFacts ++= putBundleValue(s, args, currentContext)
		  case "[|Landroid/os/Bundle;.putDouble:(Ljava/lang/String;D)V|]" =>  //public
		  case "[|Landroid/os/Bundle;.putDoubleArray:(Ljava/lang/String;[D)V|]" =>  //public
		  case "[|Landroid/os/Bundle;.putFloat:(Ljava/lang/String;F)V|]" =>  //public
		  case "[|Landroid/os/Bundle;.putFloatArray:(Ljava/lang/String;[F)V|]" =>  //public
		  case "[|Landroid/os/Bundle;.putIBinder:(Ljava/lang/String;Landroid/os/IBinder;)V|]" =>  //public
		    newFacts ++= putBundleValue(s, args, currentContext)
		  case "[|Landroid/os/Bundle;.putInt:(Ljava/lang/String;I)V|]" =>  //public
		  case "[|Landroid/os/Bundle;.putIntArray:(Ljava/lang/String;[I)V|]" =>  //public
		  case "[|Landroid/os/Bundle;.putIntegerArrayList:(Ljava/lang/String;Ljava/util/ArrayList;)V|]" =>  //public
		    newFacts ++= putBundleValue(s, args, currentContext)
		  case "[|Landroid/os/Bundle;.putLong:(Ljava/lang/String;J)V|]" =>  //public
		  case "[|Landroid/os/Bundle;.putLongArray:(Ljava/lang/String;[J)V|]" =>  //public
		  case "[|Landroid/os/Bundle;.putParcelable:(Ljava/lang/String;Landroid/os/Parcelable;)V|]" =>  //public
		    newFacts ++= putBundleValue(s, args, currentContext)
		  case "[|Landroid/os/Bundle;.putParcelableArray:(Ljava/lang/String;[Landroid/os/Parcelable;)V|]" =>  //public
		    newFacts ++= putBundleValue(s, args, currentContext)
		  case "[|Landroid/os/Bundle;.putParcelableArrayList:(Ljava/lang/String;Ljava/util/ArrayList;)V|]" =>  //public
		    newFacts ++= putBundleValue(s, args, currentContext)
		  case "[|Landroid/os/Bundle;.putSerializable:(Ljava/lang/String;Ljava/io/Serializable;)V|]" =>  //public
		    newFacts ++= putBundleValue(s, args, currentContext)
		  case "[|Landroid/os/Bundle;.putShort:(Ljava/lang/String;S)V|]" =>  //public
		  case "[|Landroid/os/Bundle;.putShortArray:(Ljava/lang/String;[S)V|]" =>  //public
		  case "[|Landroid/os/Bundle;.putSparseParcelableArray:(Ljava/lang/String;Landroid/util/SparseArray;)V|]" =>  //public
		    newFacts ++= putBundleValue(s, args, currentContext)
		  case "[|Landroid/os/Bundle;.putString:(Ljava/lang/String;Ljava/lang/String;)V|]" =>  //public
		    newFacts ++= putBundleValue(s, args, currentContext)
		  case "[|Landroid/os/Bundle;.putStringArray:(Ljava/lang/String;[Ljava/lang/String;)V|]" =>  //public
		    newFacts ++= putBundleValue(s, args, currentContext)
		  case "[|Landroid/os/Bundle;.putStringArrayList:(Ljava/lang/String;Ljava/util/ArrayList;)V|]" =>  //public
		    newFacts ++= putBundleValue(s, args, currentContext)
		  case "[|Landroid/os/Bundle;.readFromParcel:(Landroid/os/Parcel;)V|]" =>  //public
		  case "[|Landroid/os/Bundle;.readFromParcelInner:(Landroid/os/Parcel;I)V|]" =>  //
		  case "[|Landroid/os/Bundle;.remove:(Ljava/lang/String;)V|]" =>  //public
		  case "[|Landroid/os/Bundle;.setAllowFds:(Z)Z|]" =>  //public
		  case "[|Landroid/os/Bundle;.setClassLoader:(Ljava/lang/ClassLoader;)V|]" =>  //public
		  case "[|Landroid/os/Bundle;.size:()I|]" =>  //public
		  case "[|Landroid/os/Bundle;.toString:()Ljava/lang/String;|]" =>  //public declared_synchronized
		    require(retVarOpt.isDefined)
		    newFacts += getPointStringToRet(retVarOpt.get, currentContext)
		  case "[|Landroid/os/Bundle;.typeWarning:(Ljava/lang/String;Ljava/lang/Object;Ljava/lang/String;Ljava/lang/ClassCastException;)V|]" =>  //private
		  case "[|Landroid/os/Bundle;.typeWarning:(Ljava/lang/String;Ljava/lang/Object;Ljava/lang/String;Ljava/lang/Object;Ljava/lang/ClassCastException;)V|]" =>  //private
		  case "[|Landroid/os/Bundle;.unparcel:()V|]" =>  //declared_synchronized
		  case "[|Landroid/os/Bundle;.writeToParcel:(Landroid/os/Parcel;I)V|]" =>  //public
		  case _ =>
	  }
	  s ++ newFacts
	}
	
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
	  
	private def initBundleFromBundle(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >1)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val paramSlot = VarSlot(args(1))
	  val paramValue = factMap.getOrElse(paramSlot, isetEmpty)
	  if(!paramValue.isEmpty && !thisValue.isEmpty){
	    val pvs = paramValue.map{ins => factMap(FieldSlot(ins, "[|android:os:Bundle.entries|]"))}.reduce(iunion[Instance])
	    thisValue.map{
	      tv =>
	        pvs.map{s => RFAFact(FieldSlot(tv, "[|android:os:Bundle.entries|]"), s)}
	    }.reduce(iunion[RFAFact])
	  } else {
	    isetEmpty
	  }
  }
	
	private def cloneBundle(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >0)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  thisValue.map{s => RFAFact(VarSlot(retVar), s.clone(currentContext))}
  }
	
	private def forPair(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    val rf = getReturnFact(NormalType("[|android:os:Bundle|]", 0), retVar, currentContext).get
    require(args.size >1)
    val param1Slot = VarSlot(args(0))
	  val param1Value = factMap.getOrElse(param1Slot, isetEmpty)
	  val param2Slot = VarSlot(args(1))
	  val param2Value = factMap.getOrElse(param2Slot, isetEmpty)
	  var entries = isetEmpty[Instance]
	  param1Value.foreach{
	    kv =>
	      param2Value.foreach{
	        vv =>
	          entries += RFATupleInstance(kv, vv, currentContext)
	      }
	  }
	  entries.map{s => RFAFact(FieldSlot(rf.v, "[|android:os:Bundle|]"), s)}
  }
	
	private def getBundleKeySetToRet(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
	  var result = isetEmpty[RFAFact]
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >0)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val strValue = thisValue.map{ins => factMap(FieldSlot(ins, "[|android:os:Bundle.entries|]"))}.reduce(iunion[Instance])
	  val rf = getReturnFact(NormalType("[|java:util:HashSet|]", 0), retVar, currentContext).get
	  result += rf
	  result ++= strValue.map{
	    s => 
	      require(s.isInstanceOf[RFATupleInstance])
	      RFAFact(FieldSlot(rf.v, "[|java:util:HashSet.items|]"), s.asInstanceOf[RFATupleInstance].left)
	  }
	  result
  }
	
	private def getBundleValue(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
	  var result = isetEmpty[RFAFact]
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >1)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val keySlot = VarSlot(args(1))
	  val keyValue = factMap.getOrElse(keySlot, isetEmpty)
	  val entValue = thisValue.map{ins => factMap.getOrElse(FieldSlot(ins, "[|android:os:Bundle.entries|]"), isetEmpty)}.reduce(iunion[Instance])
	  if(keyValue.filter(_.isInstanceOf[RFAPointStringInstance]).isEmpty){
		  entValue.foreach{
		    v =>
		      require(v.isInstanceOf[RFATupleInstance])
		      if(keyValue.contains(v.asInstanceOf[RFATupleInstance].left)){
		        result += (RFAFact(VarSlot(retVar), v.asInstanceOf[RFATupleInstance].right))
		      }
		  }
	  } else {
	    entValue.foreach{
		    v =>
		      require(v.isInstanceOf[RFATupleInstance])
		      result += (RFAFact(VarSlot(retVar), v.asInstanceOf[RFATupleInstance].right))
		  }
	  }
	  result
  }
	
	private def getBundleValueWithDefault(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
	  var result = isetEmpty[RFAFact]
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >2)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val keySlot = VarSlot(args(1))
	  val keyValue = factMap.getOrElse(keySlot, isetEmpty)
	  val defaultSlot = VarSlot(args(1))
	  val defaultValue = factMap.getOrElse(defaultSlot, isetEmpty)
	  val entValue = thisValue.map{ins => factMap.getOrElse(FieldSlot(ins, "[|android:os:Bundle.entries|]"), isetEmpty)}.reduce(iunion[Instance])
	  if(keyValue.filter(_.isInstanceOf[RFAPointStringInstance]).isEmpty){
		  entValue.foreach{
		    v =>
		      require(v.isInstanceOf[RFATupleInstance])
		      if(keyValue.contains(v.asInstanceOf[RFATupleInstance].left)){
		        result += (RFAFact(VarSlot(retVar), v.asInstanceOf[RFATupleInstance].right))
		      }
		  }
	  } else {
	    entValue.foreach{
		    v =>
		      require(v.isInstanceOf[RFATupleInstance])
		      result += (RFAFact(VarSlot(retVar), v.asInstanceOf[RFATupleInstance].right))
		  }
	  }
	  if(result.isEmpty){
	    result ++= defaultValue.map(RFAFact(VarSlot(retVar), _))
	  }
	  result
  }
	
	private def putBundleValue(s : ISet[RFAFact], args : List[String], currentContext : Context) : ISet[RFAFact] ={
	  var result = isetEmpty[RFAFact]
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >2)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val keySlot = VarSlot(args(1))
	  val keyValue = factMap.getOrElse(keySlot, isetEmpty)
	  val valueSlot = VarSlot(args(2))
	  val valueValue = factMap.getOrElse(valueSlot, isetEmpty)
	  var entries = isetEmpty[Instance]
	  keyValue.foreach{
	    kv =>
	      valueValue.foreach{
	        vv =>
	          entries += RFATupleInstance(kv, vv, currentContext)
	      }
	  }
	  thisValue.foreach{
	    ins =>
	      result ++= entries.map(e => RFAFact(FieldSlot(ins, "[|android:os:Bundle.entries|]"), e))
	  }
	  result
  }
	
	
	
	private def putAllBundleValues(s : ISet[RFAFact], args : List[String], currentContext : Context) : ISet[RFAFact] ={
	  var result = isetEmpty[RFAFact]
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >1)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  val slot2 = VarSlot(args(1))
	  val value2 = factMap.getOrElse(slot2, isetEmpty)
	  thisValue.foreach{
	    ins =>
	      result ++= value2.map(e => RFAFact(FieldSlot(ins, "[|android:os:Bundle.entries|]"), e))
	  }
	  result
  }
}