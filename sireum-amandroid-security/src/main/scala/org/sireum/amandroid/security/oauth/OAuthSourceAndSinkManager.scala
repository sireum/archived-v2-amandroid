package org.sireum.amandroid.security.oauth

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
import org.sireum.jawa.alir.interProcedural.controlFlowGraph.CGInvokeNode
import org.sireum.jawa.Center

class OAuthSourceAndSinkManager(appPackageName : String, 
    												layoutControls : Map[Int, LayoutControl], 
    												callbackMethods : ISet[JawaProcedure], 
    												sasFilePath : String) extends BasicSourceAndSinkManager(appPackageName, layoutControls, callbackMethods, sasFilePath){
  private final val TITLE = "OAuthSourceAndSinkManager"
    
  override def isSource(calleeProcedure : JawaProcedure, callerProcedure : JawaProcedure, callerLoc : JumpLocation) = false
    
  override def isCallbackSource(proc : JawaProcedure) : Boolean = {
    false
  }
  
	override def isUISource(calleeProcedure : JawaProcedure, callerProcedure : JawaProcedure, callerLoc : JumpLocation) : Boolean = {
	  false
	}
	
	override def isSource(loc : LocationDecl) : Boolean = {
	  var flag = false
	  val visitor = Visitor.build({
	      case as : AssignAction =>
	        as.rhs match {
	          case le : LiteralExp =>
	            if(le.typ.name.equals("STRING")){
	              if(le.text.equals("access_token"))
	                flag = true
	            }
	            false
	          case _ =>
	            false
	        }
		})
	  
	  visitor(loc)
	  flag
	}
	
	def isIccSink(invNode : CGInvokeNode, rfaFact : ISet[RFAFact]) : Boolean = {
	  var sinkflag = false
    val calleeSet = invNode.getCalleeSet
    calleeSet.foreach{
      callee =>
        if(InterComponentCommunicationModel.isIccOperation(Center.getProcedureWithoutFailing(callee.callee))){
          sinkflag = true
          val rfafactMap = ReachingFactsAnalysisHelper.getFactMap(rfaFact)
          val args = Center.getProcedureWithoutFailing(invNode.getOwner).getProcedureBody.location(invNode.getLocIndex).asInstanceOf[JumpLocation].jump.asInstanceOf[CallJump].callExp.arg match{
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