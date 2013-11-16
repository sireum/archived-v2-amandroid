package org.sireum.amandroid.interProcedural.reachingFactsAnalysis.model

import org.sireum.amandroid.AmandroidRecord
import org.sireum.util._
import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis._
import org.sireum.amandroid.Instance
import org.sireum.amandroid.AmandroidProcedure
import org.sireum.amandroid.Center
import org.sireum.amandroid.Type
import org.sireum.amandroid.NormalType
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.ReachingFactsAnalysisHelper

object HashSetModel {
	def isHashSet(r : AmandroidRecord) : Boolean = r.getName == "[|java:util:HashSet|]"
	  
  private def addItemToHashSetField(s : ISet[RFAFact], args : List[String], currentContext : Context) : ISet[RFAFact] ={
	  val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
	  require(args.size > 1)
	  var newfacts = isetEmpty[RFAFact]
    val thisSlot = VarSlot(args(0))
	  val thisValues = factMap.getOrElse(thisSlot, isetEmpty)
	  val paramSlot = VarSlot(args(1))
	  val paramValues = factMap.getOrElse(paramSlot, isetEmpty)
	  thisValues.foreach{
      ins =>
        newfacts ++= paramValues.map{p=>RFAFact(FieldSlot(ins, "[|java:util:HashSet.items|]"), p)}
    }
	  newfacts 
	}
  
  private def cloneHashSetToRet(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >0)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  thisValue.map{s => RFAFact(VarSlot(retVar), s.clone(currentContext))}
  }
  
  def doHashSetCall(s : ISet[RFAFact], p : AmandroidProcedure, args : List[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact], Boolean) = {
	  var newFacts = isetEmpty[RFAFact]
	  var delFacts = isetEmpty[RFAFact]
	  var byPassFlag = true
	  p.getSignature match{
      case "[|Ljava/util/HashSet;.<init>:()V|]" =>
//        newFacts ++= initializeHashSetField(s, args, currentContext)
		  case "[|Ljava/util/HashSet;.<init>:(I)V|]" =>
//		    newFacts ++= initializeHashSetField(s, args, currentContext)
		  case "[|Ljava/util/HashSet;.<init>:(IF)V|]" =>
//		    newFacts ++= initializeHashSetField(s, args, currentContext)
		  case "[|Ljava/util/HashSet;.<init>:(Ljava/util/Collection;)V|]" =>
//		    newFacts ++= initializeHashSetField(s, args, currentContext)
		  case "[|Ljava/util/HashSet;.<init>:(Ljava/util/HashMap;)V|]" =>
//		    newFacts ++= initializeHashSetField(s, args, currentContext)
		  case "[|Ljava/util/HashSet;.add:(Ljava/lang/Object;)Z|]" =>
		    newFacts ++= addItemToHashSetField(s, args, currentContext)
		    byPassFlag = false
		  case "[|Ljava/util/HashSet;.clear:()V|]" =>
		  case "[|Ljava/util/HashSet;.clone:()Ljava/lang/Object;|]" =>
		    require(retVars.size == 1)
		    newFacts ++= cloneHashSetToRet(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "[|Ljava/util/HashSet;.contains:(Ljava/lang/Object;)Z|]" =>
		  case "[|Ljava/util/HashSet;.createBackingMap:(IF)Ljava/util/HashMap;|]" =>
		    require(retVars.size == 1)
		    ReachingFactsAnalysisHelper.getReturnFact(NormalType("[|java:util:HashMap|]", 0), retVars(0), currentContext) match{
		      case Some(fact) => newFacts += fact
		      case None =>
		    }
		    byPassFlag = false
		  case "[|Ljava/util/HashSet;.isEmpty:()Z|]" =>
		  case "[|Ljava/util/HashSet;.iterator:()Ljava/util/Iterator;|]" =>
		  case "[|Ljava/util/HashSet;.readObject:(Ljava/io/ObjectInputStream;)V|]" =>
		  case "[|Ljava/util/HashSet;.remove:(Ljava/lang/Object;)Z|]" =>
		  case "[|Ljava/util/HashSet;.size:()I|]" =>
		  case "[|Ljava/util/HashSet;.writeObject:(Ljava/io/ObjectOutputStream;)V|]" =>
		  case _ =>
    }
    (newFacts, delFacts, byPassFlag)
  }
}