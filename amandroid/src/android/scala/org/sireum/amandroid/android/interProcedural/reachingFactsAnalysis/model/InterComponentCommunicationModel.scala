package org.sireum.amandroid.android.interProcedural.reachingFactsAnalysis.model

import org.sireum.amandroid.android.AndroidConstants
import org.sireum.amandroid.Center
import org.sireum.util._
import org.sireum.amandroid.AmandroidProcedure
import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.FieldSlot
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAConcreteStringInstance
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.VarSlot
import org.sireum.amandroid.android.parser.UriData
import org.sireum.amandroid.android.AppCenter
import org.sireum.amandroid.AmandroidRecord
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.ReachingFactsAnalysisHelper
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAFact
import org.sireum.amandroid.Instance
import java.net.URI
import org.sireum.amandroid.util.StringFormConverter
import org.sireum.amandroid.MessageCenter._
import org.sireum.amandroid.android.interProcedural.reachingFactsAnalysis.IntentHelper

object InterComponentCommunicationModel {
  
	def isIccOperation(proc : AmandroidProcedure) : Boolean = {
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
	
	def doIccCall(s : ISet[RFAFact], calleeProc : AmandroidProcedure, args : List[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[AmandroidProcedure]) = {
	  val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
	  require(args.size > 1)
	  val intentSlot = VarSlot(args(1))
	  val intentValues = factMap.getOrElse(intentSlot, isetEmpty)
	  val intentcontents = IntentHelper.getIntentContents(factMap, intentValues, currentContext)
	  val comMap = IntentHelper.mappingIntents(intentcontents)
	  var targets : ISet[AmandroidProcedure] = isetEmpty
	  comMap.foreach{
	    case (_, coms) =>
	      coms.foreach{
	        case (com, _) =>
	          com.tryGetProcedure(AndroidConstants.DUMMY_MAIN) match{
              case Some(r) => 
                targets += r
              case None => err_msg_critical("Target component " + com + " does not have dummymain.")
            }
	      }
	  }
	  (s, targets)
	}

}