package org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis.model

import org.sireum.amandroid.alir.AndroidConstants
import org.sireum.jawa.Center
import org.sireum.util._
import org.sireum.jawa.JawaProcedure
import org.sireum.jawa.alir.Context
import org.sireum.jawa.alir.interProcedural.reachingFactsAnalysis._
import org.sireum.amandroid.android.parser.UriData
import org.sireum.amandroid.alir.AppCenter
import org.sireum.jawa.JawaRecord
import org.sireum.jawa.alir.Instance
import java.net.URI
import org.sireum.jawa.util.StringFormConverter
import org.sireum.jawa.MessageCenter._
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis.IntentHelper

object InterComponentCommunicationModel {
  
	def isIccOperation(proc : JawaProcedure) : Boolean = {
    var flag = false
    val childRecord = proc.getDeclaringRecord
    val parentRecord = Center.resolveRecord(AndroidConstants.CONTEXT, Center.ResolveLevel.BODIES)
    if(Center.getRecordHierarchy.isRecordRecursivelySubClassOfIncluding(childRecord, parentRecord))
	    AndroidConstants.getIccMethods.foreach{
	      item =>
	        if(proc.getSubSignature == item)
	         flag = true
	    }
    flag
  }
	
	def doIccCall(s : ISet[RFAFact], calleeProc : JawaProcedure, args : List[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[JawaProcedure]) = {
	  val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
	  require(args.size > 1)
	  val intentSlot = VarSlot(args(1))
	  val intentValues = factMap.getOrElse(intentSlot, isetEmpty)
	  val intentcontents = IntentHelper.getIntentContents(factMap, intentValues, currentContext)
	  val comMap = IntentHelper.mappingIntents(intentcontents)
	  var targets : ISet[JawaProcedure] = isetEmpty
	  comMap.foreach{
	    case (_, coms) =>
	      coms.foreach{
	        case (com, _) =>
	          com.tryGetProcedure(AndroidConstants.MAINCOMP_ENV_SUBSIG) match{
              case Some(r) => targets += r
              case None => 
                com.tryGetProcedure(AndroidConstants.COMP_ENV_SUBSIG) match{
                  case Some(r) => targets += r
                  case None => err_msg_critical("Target component " + com + " does not have environment.")
                }
            }
	      }
	  }
	  (s, targets)
	}

}