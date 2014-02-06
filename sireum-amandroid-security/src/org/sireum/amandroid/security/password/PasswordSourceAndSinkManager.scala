package org.sireum.amandroid.security.password

import org.sireum.amandroid.alir.interProcedural.taintAnalysis.BasicSourceAndSinkManager
import org.sireum.amandroid.android.parser.LayoutControl
import org.sireum.util._
import org.sireum.jawa.JawaProcedure
import org.sireum.jawa.MessageCenter._
import org.sireum.jawa.alir.interProcedural.controlFlowGraph.CGCallNode
import org.sireum.jawa.alir.interProcedural.controlFlowGraph.CGNode
import org.sireum.jawa.alir.interProcedural.reachingFactsAnalysis.RFAFact
import org.sireum.pilar.ast.JumpLocation
import org.sireum.amandroid.alir.AndroidConstants
import org.sireum.jawa.alir.util.ExplicitValueFinder
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis.model.InterComponentCommunicationModel
import org.sireum.jawa.alir.interProcedural.reachingFactsAnalysis.ReachingFactsAnalysisHelper
import org.sireum.pilar.ast._
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis.IntentHelper
import org.sireum.jawa.alir.interProcedural.reachingFactsAnalysis.VarSlot

class PasswordSourceAndSinkManager(appPackageName : String, 
    												layoutControls : Map[Int, LayoutControl], 
    												callbackMethods : ISet[JawaProcedure], 
    												sasFilePath : String) extends BasicSourceAndSinkManager(appPackageName, layoutControls, callbackMethods, sasFilePath){
  def isCallbackSource(proc : JawaProcedure) : Boolean = {
    false
  }
  
	def isUISource(calleeProcedure : JawaProcedure, callerProcedure : JawaProcedure, callerLoc : JumpLocation) : Boolean = {
	  if(calleeProcedure.getSignature == AndroidConstants.ACTIVITY_FINDVIEWBYID || calleeProcedure.getSignature == AndroidConstants.VIEW_FINDVIEWBYID){
	    val nums = ExplicitValueFinder.findExplicitIntValueForArgs(callerProcedure, callerLoc, 1)
	    nums.foreach{
	      num =>
	        this.layoutControls.get(num) match{
	          case Some(control) =>
	            return control.isSensitive
	          case None =>
	            err_msg_normal("Layout control with ID " + num + " not found.")
	        }
	    }
	  }
	  false
	}
	
	def isIccSink(invNode : CGCallNode, rfaFact : ISet[RFAFact]) : Boolean = {
	  var sinkflag = false
    val calleeSet = invNode.getCalleeSet
    calleeSet.foreach{
      callee =>
        if(InterComponentCommunicationModel.isIccOperation(callee.calleeProc)){
          sinkflag = true
          val rfafactMap = ReachingFactsAnalysisHelper.getFactMap(rfaFact)
          val args = invNode.getOwner.getProcedureBody.location(invNode.getLocIndex).asInstanceOf[JumpLocation].jump.asInstanceOf[CallJump].callExp.arg match{
              case te : TupleExp =>
                te.exps.map{
			            exp =>
			              exp match{
					            case ne : NameExp => ne.name.name
					            case _ => exp.toString()
					          }
			          }.toList
              case a => throw new RuntimeException("wrong exp type: " + a)
            }
          val intentSlot = VarSlot(args(1))
          val intentValues = rfafactMap.getOrElse(intentSlot, isetEmpty)
          val intentContents = IntentHelper.getIntentContents(rfafactMap, intentValues, invNode.getContext)
          val comMap = IntentHelper.mappingIntents(intentContents)
          comMap.foreach{
            case (_, coms) =>
              if(coms.isEmpty) sinkflag = true
              coms.foreach{
                case (com, typ) =>
                  typ match {
                    case IntentHelper.IntentType.EXPLICIT => if(com.isPhantom) sinkflag = true
//                    case IntentHelper.IntentType.EXPLICIT => sinkflag = true
                    case IntentHelper.IntentType.IMPLICIT => sinkflag = true
                  }
              }
          }
        }
    }
    sinkflag
	}
	
	def isIccSource(entNode : CGNode, iddgEntNode : CGNode) : Boolean = {
	  false
	}
	
}