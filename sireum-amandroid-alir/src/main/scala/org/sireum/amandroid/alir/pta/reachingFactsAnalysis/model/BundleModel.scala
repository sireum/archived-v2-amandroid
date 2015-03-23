/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.alir.pta.reachingFactsAnalysis.model

import org.sireum.jawa._
import org.sireum.util._
import org.sireum.jawa.alir.Context
import org.sireum.jawa.alir.pta.reachingFactsAnalysis._
import org.sireum.amandroid.AndroidConstants
import org.sireum.jawa.alir.pta.Instance
import org.sireum.jawa.alir.pta.PTAPointStringInstance
import org.sireum.jawa.alir.pta.PTATupleInstance
import org.sireum.jawa.alir.pta.PTAResult
import org.sireum.jawa.alir.pta.VarSlot
import org.sireum.jawa.alir.pta.FieldSlot

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object BundleModel {
	def isBundle(r : JawaRecord) : Boolean = r.getName == AndroidConstants.BUNDLE
	  
	def doBundleCall(s : PTAResult, p : JawaProcedure, args : List[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact], Boolean) = {
	  var newFacts = isetEmpty[RFAFact]
	  var delFacts = isetEmpty[RFAFact]
	  var byPassFlag = true
	  p.getSignature match{
	    case "Landroid/os/Bundle;.<clinit>:()V" =>  //static constructor
		  case "Landroid/os/Bundle;.<init>:()V" =>  //public constructor
		  case "Landroid/os/Bundle;.<init>:(I)V" =>  //public constructor
		  case "Landroid/os/Bundle;.<init>:(Landroid/os/Bundle;)V" =>  //public constructor
		    require(retVars.size == 1)
		    newFacts ++= initBundleFromBundle(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.<init>:(Landroid/os/Parcel;)V" =>  //constructor
		  case "Landroid/os/Bundle;.<init>:(Landroid/os/Parcel;I)V" =>  //constructor
		  case "Landroid/os/Bundle;.<init>:(Ljava/lang/ClassLoader;)V" =>  //public constructor
		  case "Landroid/os/Bundle;.clear:()V" =>  //public
		  case "Landroid/os/Bundle;.clone:()Ljava/lang/Object;" =>  //public
		    require(retVars.size == 1)
		    newFacts ++= cloneBundle(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.containsKey:(Ljava/lang/String;)Z" =>  //public
		  case "Landroid/os/Bundle;.describeContents:()I" =>  //public
		  case "Landroid/os/Bundle;.forPair:(Ljava/lang/String;Ljava/lang/String;)Landroid/os/Bundle;" =>  //public static
		    require(retVars.size == 1)
		    newFacts ++= forPair(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.get:(Ljava/lang/String;)Ljava/lang/Object;" =>  //public
		    require(retVars.size == 1)
		    newFacts ++= getBundleValue(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.getBoolean:(Ljava/lang/String;)Z" =>  //public
		  case "Landroid/os/Bundle;.getBoolean:(Ljava/lang/String;Z)Z" =>  //public
		  case "Landroid/os/Bundle;.getBooleanArray:(Ljava/lang/String;)[Z" =>  //public
		  case "Landroid/os/Bundle;.getBundle:(Ljava/lang/String;)Landroid/os/Bundle;" =>  //public
		    require(retVars.size == 1)
		    newFacts ++= getBundleValue(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.getByte:(Ljava/lang/String;)B" =>  //public
		  case "Landroid/os/Bundle;.getByte:(Ljava/lang/String;B)Ljava/lang/Byte;" =>  //public
		  case "Landroid/os/Bundle;.getByteArray:(Ljava/lang/String;)[B" =>  //public
		  case "Landroid/os/Bundle;.getChar:(Ljava/lang/String;)C" =>  //public
		  case "Landroid/os/Bundle;.getChar:(Ljava/lang/String;C)C" =>  //public
		  case "Landroid/os/Bundle;.getCharArray:(Ljava/lang/String;)[C" =>  //public
		  case "Landroid/os/Bundle;.getCharSequence:(Ljava/lang/String;)Ljava/lang/CharSequence;" =>  //public
		    require(retVars.size == 1)
		    newFacts ++= getBundleValue(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.getCharSequence:(Ljava/lang/String;Ljava/lang/CharSequence;)Ljava/lang/CharSequence;" =>  //public
		    require(retVars.size == 1)
		    newFacts ++= getBundleValueWithDefault(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.getCharSequenceArray:(Ljava/lang/String;)[Ljava/lang/CharSequence;" =>  //public
		    require(retVars.size == 1)
		    newFacts ++= getBundleValue(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.getCharSequenceArrayList:(Ljava/lang/String;)Ljava/util/ArrayList;" =>  //public
		    require(retVars.size == 1)
		    newFacts ++= getBundleValue(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.getClassLoader:()Ljava/lang/ClassLoader;" =>  //public
		  case "Landroid/os/Bundle;.getDouble:(Ljava/lang/String;)D" =>  //public
		  case "Landroid/os/Bundle;.getDouble:(Ljava/lang/String;D)D" =>  //public
		  case "Landroid/os/Bundle;.getDoubleArray:(Ljava/lang/String;)[D" =>  //public
		  case "Landroid/os/Bundle;.getFloat:(Ljava/lang/String;)F" =>  //public
		  case "Landroid/os/Bundle;.getFloat:(Ljava/lang/String;F)F" =>  //public
		  case "Landroid/os/Bundle;.getFloatArray:(Ljava/lang/String;)[F" =>  //public
		  case "Landroid/os/Bundle;.getIBinder:(Ljava/lang/String;)Landroid/os/IBinder;" =>  //public
		    require(retVars.size == 1)
		    newFacts ++= getBundleValue(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.getInt:(Ljava/lang/String;)I" =>  //public
		  case "Landroid/os/Bundle;.getInt:(Ljava/lang/String;I)I" =>  //public
		  case "Landroid/os/Bundle;.getIntArray:(Ljava/lang/String;)[I" =>  //public
		  case "Landroid/os/Bundle;.getIntegerArrayList:(Ljava/lang/String;)Ljava/util/ArrayList;" =>  //public
		    require(retVars.size == 1)
		    newFacts ++= getBundleValue(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.getLong:(Ljava/lang/String;)J" =>  //public
		  case "Landroid/os/Bundle;.getLong:(Ljava/lang/String;J)J" =>  //public
		  case "Landroid/os/Bundle;.getLongArray:(Ljava/lang/String;)[J" =>  //public
		  case "Landroid/os/Bundle;.getPairValue:()Ljava/lang/String;" =>  //public
		  case "Landroid/os/Bundle;.getParcelable:(Ljava/lang/String;)Landroid/os/Parcelable;" =>  //public
		    require(retVars.size == 1)
		    newFacts ++= getBundleValue(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.getParcelableArray:(Ljava/lang/String;)[Landroid/os/Parcelable;" =>  //public
		    require(retVars.size == 1)
		    newFacts ++= getBundleValue(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.getParcelableArrayList:(Ljava/lang/String;)Ljava/util/ArrayList;" =>  //public
		    require(retVars.size == 1)
		    newFacts ++= getBundleValue(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.getSerializable:(Ljava/lang/String;)Ljava/io/Serializable;" =>  //public
		    require(retVars.size == 1)
		    newFacts ++= getBundleValue(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.getShort:(Ljava/lang/String;)S" =>  //public
		  case "Landroid/os/Bundle;.getShort:(Ljava/lang/String;S)S" =>  //public
		  case "Landroid/os/Bundle;.getShortArray:(Ljava/lang/String;)[S" =>  //public
		  case "Landroid/os/Bundle;.getSparseParcelableArray:(Ljava/lang/String;)Landroid/util/SparseArray;" =>  //public
		    require(retVars.size == 1)
		    newFacts ++= getBundleValue(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.getString:(Ljava/lang/String;)Ljava/lang/String;" =>  //public
		    require(retVars.size == 1)
		    newFacts ++= getBundleValue(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.getString:(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;" =>  //public
		    require(retVars.size == 1)
		    newFacts ++= getBundleValueWithDefault(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.getStringArray:(Ljava/lang/String;)[Ljava/lang/String;" =>  //public
		    require(retVars.size == 1)
		    newFacts ++= getBundleValue(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.getStringArrayList:(Ljava/lang/String;)Ljava/util/ArrayList;" =>  //public
		    require(retVars.size == 1)
		    newFacts ++= getBundleValue(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.hasFileDescriptors:()Z" =>  //public
		  case "Landroid/os/Bundle;.isEmpty:()Z" =>  //public
		  case "Landroid/os/Bundle;.isParcelled:()Z" =>  //public
		  case "Landroid/os/Bundle;.keySet:()Ljava/util/Set;" =>  //public
		    require(retVars.size == 1)
		    newFacts ++= getBundleKeySetToRet(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.putAll:(Landroid/os/Bundle;)V" =>  //public
		    newFacts ++= putAllBundleValues(s, args, currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.putBoolean:(Ljava/lang/String;Z)V" =>  //public
		  case "Landroid/os/Bundle;.putBooleanArray:(Ljava/lang/String;[Z)V" =>  //public
		  case "Landroid/os/Bundle;.putBundle:(Ljava/lang/String;Landroid/os/Bundle;)V" =>  //public
		    newFacts ++= putBundleValue(s, args, currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.putByte:(Ljava/lang/String;B)V" =>  //public
		  case "Landroid/os/Bundle;.putByteArray:(Ljava/lang/String;[B)V" =>  //public
		  case "Landroid/os/Bundle;.putChar:(Ljava/lang/String;C)V" =>  //public
		  case "Landroid/os/Bundle;.putCharArray:(Ljava/lang/String;[C)V" =>  //public
		  case "Landroid/os/Bundle;.putCharSequence:(Ljava/lang/String;Ljava/lang/CharSequence;)V" =>  //public
		    newFacts ++= putBundleValue(s, args, currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.putCharSequenceArray:(Ljava/lang/String;[Ljava/lang/CharSequence;)V" =>  //public
		    newFacts ++= putBundleValue(s, args, currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.putCharSequenceArrayList:(Ljava/lang/String;Ljava/util/ArrayList;)V" =>  //public
		    newFacts ++= putBundleValue(s, args, currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.putDouble:(Ljava/lang/String;D)V" =>  //public
		  case "Landroid/os/Bundle;.putDoubleArray:(Ljava/lang/String;[D)V" =>  //public
		  case "Landroid/os/Bundle;.putFloat:(Ljava/lang/String;F)V" =>  //public
		  case "Landroid/os/Bundle;.putFloatArray:(Ljava/lang/String;[F)V" =>  //public
		  case "Landroid/os/Bundle;.putIBinder:(Ljava/lang/String;Landroid/os/IBinder;)V" =>  //public
		    newFacts ++= putBundleValue(s, args, currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.putInt:(Ljava/lang/String;I)V" =>  //public
		  case "Landroid/os/Bundle;.putIntArray:(Ljava/lang/String;[I)V" =>  //public
		  case "Landroid/os/Bundle;.putIntegerArrayList:(Ljava/lang/String;Ljava/util/ArrayList;)V" =>  //public
		    newFacts ++= putBundleValue(s, args, currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.putLong:(Ljava/lang/String;J)V" =>  //public
		  case "Landroid/os/Bundle;.putLongArray:(Ljava/lang/String;[J)V" =>  //public
		  case "Landroid/os/Bundle;.putParcelable:(Ljava/lang/String;Landroid/os/Parcelable;)V" =>  //public
		    newFacts ++= putBundleValue(s, args, currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.putParcelableArray:(Ljava/lang/String;[Landroid/os/Parcelable;)V" =>  //public
		    newFacts ++= putBundleValue(s, args, currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.putParcelableArrayList:(Ljava/lang/String;Ljava/util/ArrayList;)V" =>  //public
		    newFacts ++= putBundleValue(s, args, currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.putSerializable:(Ljava/lang/String;Ljava/io/Serializable;)V" =>  //public
		    newFacts ++= putBundleValue(s, args, currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.putShort:(Ljava/lang/String;S)V" =>  //public
		  case "Landroid/os/Bundle;.putShortArray:(Ljava/lang/String;[S)V" =>  //public
		  case "Landroid/os/Bundle;.putSparseParcelableArray:(Ljava/lang/String;Landroid/util/SparseArray;)V" =>  //public
		    newFacts ++= putBundleValue(s, args, currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.putString:(Ljava/lang/String;Ljava/lang/String;)V" =>  //public
		    newFacts ++= putBundleValue(s, args, currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.putStringArray:(Ljava/lang/String;[Ljava/lang/String;)V" =>  //public
		    newFacts ++= putBundleValue(s, args, currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.putStringArrayList:(Ljava/lang/String;Ljava/util/ArrayList;)V" =>  //public
		    newFacts ++= putBundleValue(s, args, currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.readFromParcel:(Landroid/os/Parcel;)V" =>  //public
		  case "Landroid/os/Bundle;.readFromParcelInner:(Landroid/os/Parcel;I)V" =>  //
		  case "Landroid/os/Bundle;.remove:(Ljava/lang/String;)V" =>  //public
		  case "Landroid/os/Bundle;.setAllowFds:(Z)Z" =>  //public
		  case "Landroid/os/Bundle;.setClassLoader:(Ljava/lang/ClassLoader;)V" =>  //public
		  case "Landroid/os/Bundle;.size:()I" =>  //public
		  case "Landroid/os/Bundle;.toString:()Ljava/lang/String;" =>  //public declared_synchronized
		    require(retVars.size == 1)
		    newFacts += getPointStringToRet(retVars(0), currentContext)
		    byPassFlag = false
		  case "Landroid/os/Bundle;.typeWarning:(Ljava/lang/String;Ljava/lang/Object;Ljava/lang/String;Ljava/lang/ClassCastException;)V" =>  //private
		  case "Landroid/os/Bundle;.typeWarning:(Ljava/lang/String;Ljava/lang/Object;Ljava/lang/String;Ljava/lang/Object;Ljava/lang/ClassCastException;)V" =>  //private
		  case "Landroid/os/Bundle;.unparcel:()V" =>  //declared_synchronized
		  case "Landroid/os/Bundle;.writeToParcel:(Landroid/os/Parcel;I)V" =>  //public
		  case _ =>
	  }
	  (newFacts, delFacts, byPassFlag)
	}
	
	private def getPointStringToRet(retVar : String, currentContext : Context): RFAFact = {
    val newThisValue = PTAPointStringInstance(currentContext.copy)
    RFAFact(VarSlot(retVar), newThisValue)	 
	}
	  
	private def initBundleFromBundle(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
    require(args.size >1)
    val thisSlot = VarSlot(args(0))
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  val paramSlot = VarSlot(args(1))
	  val paramValue = s.pointsToSet(paramSlot, currentContext)
	  if(!paramValue.isEmpty && !thisValue.isEmpty){
	    val pvs = paramValue.map{ins => s.pointsToSet(FieldSlot(ins, "entries"), currentContext)}.reduce(iunion[Instance])
	    thisValue.map{
	      tv =>
	        pvs.map{s => RFAFact(FieldSlot(tv, "entries"), s)}
	    }.reduce(iunion[RFAFact])
	  } else {
	    isetEmpty
	  }
  }
	
	private def cloneBundle(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
    require(args.size >0)
    val thisSlot = VarSlot(args(0))
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  thisValue.map{s => RFAFact(VarSlot(retVar), s.clone(currentContext))}
  }
	
	private def forPair(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
    val rf = ReachingFactsAnalysisHelper.getReturnFact(NormalType("android.os.Bundle", 0), retVar, currentContext).get
    require(args.size >1)
    val param1Slot = VarSlot(args(0))
	  val param1Value = s.pointsToSet(param1Slot, currentContext)
	  val param2Slot = VarSlot(args(1))
	  val param2Value = s.pointsToSet(param2Slot, currentContext)
	  var entries = isetEmpty[Instance]
	  param1Value.foreach{
	    kv =>
	      param2Value.foreach{
	        vv =>
	          entries += PTATupleInstance(kv, vv, currentContext)
	      }
	  }
	  entries.map{s => RFAFact(FieldSlot(rf.v, "entries"), s)}
  }
	
	private def getBundleKeySetToRet(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
	  var result = isetEmpty[RFAFact]
    require(args.size >0)
    val thisSlot = VarSlot(args(0))
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
    if(!thisValue.isEmpty){
	    val strValue = thisValue.map{ins => s.pointsToSet(FieldSlot(ins, "entries"), currentContext)}.reduce(iunion[Instance])
  	  val rf = ReachingFactsAnalysisHelper.getReturnFact(NormalType("java.util.HashSet", 0), retVar, currentContext).get
  	  result += rf
  	  result ++= strValue.map{
  	    s => 
  	      require(s.isInstanceOf[PTATupleInstance])
  	      RFAFact(FieldSlot(rf.v, "java.util.HashSet.items"), s.asInstanceOf[PTATupleInstance].left)
  	  }
    }
	  result
  }
	
	private def getBundleValue(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
	  var result = isetEmpty[RFAFact]
    require(args.size >1)
    val thisSlot = VarSlot(args(0))
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  val keySlot = VarSlot(args(1))
	  val keyValue = s.pointsToSet(keySlot, currentContext)
    if(!thisValue.isEmpty){
  	  val entValue = thisValue.map{ins => s.pointsToSet(FieldSlot(ins, "entries"), currentContext)}.reduce(iunion[Instance])
  	  if(keyValue.filter(_.isInstanceOf[PTAPointStringInstance]).isEmpty){
  		  entValue.foreach{
  		    v =>
  		      require(v.isInstanceOf[PTATupleInstance])
  		      if(keyValue.contains(v.asInstanceOf[PTATupleInstance].left)){
  		        result += (RFAFact(VarSlot(retVar), v.asInstanceOf[PTATupleInstance].right))
  		      }
  		  }
  	  } else {
  	    entValue.foreach{
  		    v =>
  		      require(v.isInstanceOf[PTATupleInstance])
  		      result += (RFAFact(VarSlot(retVar), v.asInstanceOf[PTATupleInstance].right))
  		  }
  	  }
    }
	  result
  }
	
	private def getBundleValueWithDefault(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
	  var result = isetEmpty[RFAFact]
    require(args.size >2)
    val thisSlot = VarSlot(args(0))
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  val keySlot = VarSlot(args(1))
	  val keyValue = s.pointsToSet(keySlot, currentContext)
	  val defaultSlot = VarSlot(args(2))
	  val defaultValue = s.pointsToSet(defaultSlot, currentContext)
    if(!thisValue.isEmpty){
  	  val entValue = thisValue.map{ins => s.pointsToSet(FieldSlot(ins, "entries"), currentContext)}.reduce(iunion[Instance])
  	  if(keyValue.filter(_.isInstanceOf[PTAPointStringInstance]).isEmpty){
  		  entValue.foreach{
  		    v =>
  		      require(v.isInstanceOf[PTATupleInstance])
  		      if(keyValue.contains(v.asInstanceOf[PTATupleInstance].left)){
  		        result += (RFAFact(VarSlot(retVar), v.asInstanceOf[PTATupleInstance].right))
  		      }
  		  }
  	  } else {
  	    entValue.foreach{
  		    v =>
  		      require(v.isInstanceOf[PTATupleInstance])
  		      result += (RFAFact(VarSlot(retVar), v.asInstanceOf[PTATupleInstance].right))
  		  }
  	  }
    }
	  if(result.isEmpty){
	    result ++= defaultValue.map(RFAFact(VarSlot(retVar), _))
	  }
	  result
  }
	
	private def putBundleValue(s : PTAResult, args : List[String], currentContext : Context) : ISet[RFAFact] ={
	  var result = isetEmpty[RFAFact]
    require(args.size >2)
    val thisSlot = VarSlot(args(0))
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  val keySlot = VarSlot(args(1))
	  val keyValue = s.pointsToSet(keySlot, currentContext)
	  val valueSlot = VarSlot(args(2))
	  val valueValue = s.pointsToSet(valueSlot, currentContext)
	  var entries = isetEmpty[Instance]
	  keyValue.foreach{
	    kv =>
	      valueValue.foreach{
	        vv =>
	          entries += PTATupleInstance(kv, vv, currentContext)
	      }
	  }
	  thisValue.foreach{
	    ins =>
	      result ++= entries.map(e => RFAFact(FieldSlot(ins, "entries"), e))
	  }
	  result
  }
	
	
	
	private def putAllBundleValues(s : PTAResult, args : List[String], currentContext : Context) : ISet[RFAFact] ={
	  var result = isetEmpty[RFAFact]
    require(args.size >1)
    val thisSlot = VarSlot(args(0))
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  val slot2 = VarSlot(args(1))
	  val value2 = s.pointsToSet(slot2, currentContext)
	  thisValue.foreach{
	    ins =>
	      value2.foreach{
	        e => 
	          val ents = s.pointsToSet(FieldSlot(e, "entries"), currentContext)
	          result ++= ents.map(e => RFAFact(FieldSlot(ins, "entries"), e))
	      }
	  }
	  result
  }
}