package org.sireum.amandroid.interProcedural.reachingFactsAnalysis.model

import org.sireum.amandroid.AmandroidProcedure
import org.sireum.util._
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis._
import org.sireum.amandroid.Center
import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid.ClassInstance
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.ReachingFactsAnalysisHelper

/**
 * @author Fengguo Wei & Sankardas Roy
 */
object NativeCallModel {
	 def isNativeCall(p : AmandroidProcedure) : Boolean = p.isNative
	 
	 def doNativeCall(s : ISet[RFAFact], p : AmandroidProcedure, args : List[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact], Boolean) = {
	  var newFacts = isetEmpty[RFAFact]
	  var delFacts = isetEmpty[RFAFact]
	  var byPassFlag = true
	  val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
	  	  
	  p.getSignature match{
	    case "[|Ljava/lang/Object;.getClass:()Ljava/lang/Class;|]" =>
	      // algo:thisvalue.foreach {ins => set insRec's classObj field with a classIns whose type is java:lang:Class and name is same as ins's type
	               // then, create two facts (a) (retVarSlot, insRec.classObj), (b) ([insRec.classObj, "[|java:lang:Class.name|]"], concreteString(ins.typ))}
	      require(args.size > 0)
          val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      thisValue.foreach{
	        ins =>
	          require(Center.hasRecord(ins.getType.typ))
	          val insRec = Center.getRecord(ins.getType.typ)
	          newFacts += (RFAFact(VarSlot(retVars(0)), insRec.getClassObj))
	          val strIns = RFAConcreteStringInstance(insRec.getClassObj.getName, insRec.getClassObj.getDefSite)
	          newFacts += (RFAFact(FieldSlot(insRec.getClassObj, "[|java:lang:Class.name|]"), strIns))
	      }
	      byPassFlag = false
	    case "[|Ljava/lang/Class;.getNameNative:()Ljava/lang/String;|]" =>
	      // algo:thisValue.foreach.{ cIns => get value of (cIns.name|]") and create fact (retVar, value)}
	      require(args.size > 0)
          val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      thisValue.foreach{
	        cIns =>
	          println(cIns + " " + cIns.getClass())
	          require(cIns.isInstanceOf[ClassInstance])
	          val name = cIns.asInstanceOf[ClassInstance].getName
	          val strIns = RFAConcreteStringInstance(name, cIns.getDefSite)
              newFacts += (RFAFact(VarSlot(retVars(0)), strIns))
	      }
	      byPassFlag = false
	    case _ =>
	  }
	  (newFacts, delFacts, byPassFlag)
	}
}