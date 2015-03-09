/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.alir.pta.reachingFactsAnalysis.model

import org.sireum.amandroid.AndroidConstants
import org.sireum.jawa.Center
import org.sireum.util._
import org.sireum.jawa.JawaProcedure
import org.sireum.jawa.alir.Context
import org.sireum.jawa.alir.pta.reachingFactsAnalysis._
import org.sireum.amandroid.parser.UriData
import org.sireum.amandroid.AppCenter
import org.sireum.jawa.JawaRecord
import org.sireum.jawa.alir.pta.Instance
import java.net.URI
import org.sireum.jawa.util.StringFormConverter
import org.sireum.jawa.MessageCenter._
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.IntentHelper
import org.sireum.jawa.alir.pta.PTAResult
import org.sireum.jawa.alir.pta.VarSlot

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object InterComponentCommunicationModel {
  final val TITLE = "InterComponentCommunicationModel"
	def isIccOperation(proc : JawaProcedure) : Boolean = {
    var flag = false
    val childRecord = proc.getDeclaringRecord
    val parentRecord = Center.resolveRecord(AndroidConstants.CONTEXT, Center.ResolveLevel.HIERARCHY)
    if(!childRecord.isInterface && Center.getRecordHierarchy.isRecordRecursivelySubClassOfIncluding(childRecord, parentRecord))
	    AndroidConstants.getIccMethods.foreach{
	      item =>
	        if(proc.getSubSignature == item)
	         flag = true
	    }
    flag
  }
	
	def doIccCall(s : PTAResult, calleeProc : JawaProcedure, args : List[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[JawaProcedure]) = {
	  require(args.size > 1)
	  val intentSlot = VarSlot(args(1))
	  val intentValues = s.pointsToSet(intentSlot, currentContext)
	  val intentcontents = IntentHelper.getIntentContents(s, intentValues, currentContext)
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
                  case None => err_msg_normal(TITLE, "Target component " + com + " does not have environment.")
                }
            }
	      }
	  }
	  (isetEmpty, targets)
	}

}