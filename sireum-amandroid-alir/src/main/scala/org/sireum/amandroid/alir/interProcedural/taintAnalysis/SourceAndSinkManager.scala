package org.sireum.amandroid.alir.interProcedural.taintAnalysis

import java.io.BufferedReader
import java.io.FileReader
import org.sireum.util._
import org.sireum.jawa.JawaProcedure
import org.sireum.amandroid.android.parser.LayoutControl
import org.sireum.amandroid.android.parser.ARSCFileParser
import java.util.regex.Pattern
import java.util.regex.Matcher
import org.sireum.jawa.Center
import org.sireum.jawa.util.StringFormConverter
import org.sireum.amandroid.alir.AndroidConstants
import org.sireum.pilar.ast.LocationDecl
import org.sireum.jawa.alir.util.ExplicitValueFinder
import org.sireum.pilar.ast.JumpLocation
import org.sireum.jawa.MessageCenter._
import java.io.File
import org.sireum.jawa.alir.interProcedural.reachingFactsAnalysis.RFAFact
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis.model.InterComponentCommunicationModel
import org.sireum.jawa.alir.interProcedural.reachingFactsAnalysis.VarSlot
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis.IntentHelper
import org.sireum.jawa.alir.interProcedural.reachingFactsAnalysis.ReachingFactsAnalysisHelper
import org.sireum.jawa.alir.interProcedural.controlFlowGraph._
import org.sireum.jawa.alir.interProcedural.dataDependenceAnalysis.InterProceduralDataDependenceGraph
import org.sireum.amandroid.alir.AppCenter
import org.sireum.pilar.ast._
import java.io.InputStreamReader
import java.io.FileInputStream

object SourceAndSinkCenter {
  
  object Category extends Enumeration {
    val NORMAL, ICC = Value
  }
  
  /**
   * it's a map from source API sig to it's category
   */
	private var sources : IMap[String, Category.Value] = imapEmpty
	/**
   * it's a map from sink API sig to it's category
   */
	private var sinks : IMap[String, Category.Value] = imapEmpty
	/**
   * it's a map from API sig to its required permission
   */
	private var apiPermissions : IMap[String, ISet[String]] = imapEmpty
	private var layoutControls : Map[Int, LayoutControl] = Map()
	private var callbackMethods : ISet[JawaProcedure] = isetEmpty
//	private var resourceRepo : ARSCFileParser = null
	private var appPackageName : String = null
	
	def init(appPackageName : String, layoutControls : Map[Int, LayoutControl], callbackMethods : ISet[JawaProcedure], sasFilePath : String) = {
	  this.appPackageName = appPackageName
	  this.layoutControls = layoutControls
	  this.callbackMethods = callbackMethods
	  SSParser.parse(sasFilePath) match{
	    case (sources, sinks) => 
	      sources.foreach{
	        case (sig, ps) =>
	          this.sources += (sig -> Category.NORMAL)
	          this.apiPermissions += (sig -> ps)
	      }
	      sinks.foreach{
	        case (sig, ps) =>
	          this.sinks += (sig -> Category.NORMAL)
	          this.apiPermissions += (sig -> ps)
	      }
	  }
	  msg_detail("source size: " + this.sources.size + " sink size: " + this.sinks.size)
	}
	
	private def matchs(procedure : JawaProcedure, procedurepool : ISet[String]) : Boolean = procedurepool.contains(procedure.getSignature)
	
	def isSourceProcedure(procedure : JawaProcedure) = matchs(procedure, this.sources.map(s=>s._1).toSet)
	
	def isSinkProcedure(procedure : JawaProcedure) = matchs(procedure, this.sinks.map(s=>s._1).toSet)
	
	def isSource(calleeProcedure : JawaProcedure, callerProcedure : JawaProcedure, callerLoc : JumpLocation) : Boolean = {
	  if(isSourceProcedure(calleeProcedure)) return true
	  if(isUISource(calleeProcedure, callerProcedure, callerLoc)) return true
	  false
	}
	
	def isCallbackSource(proc : JawaProcedure) : Boolean = {
	  if(this.callbackMethods.contains(proc) && proc.getParamNames.size > 0) true
	  else false
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
	
	def checkIccSink(invNode : CGCallNode, rfaFact : ISet[RFAFact]) : Boolean = {
    var sinkflag = false
    val calleeSet = invNode.getCalleeSet
    calleeSet.foreach{
      callee =>
        if(InterComponentCommunicationModel.isIccOperation(callee)){
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
//                    case IntentHelper.IntentType.EXPLICIT => if(com.isPhantom) sinkflag = true
                    case IntentHelper.IntentType.EXPLICIT => sinkflag = true
                    case IntentHelper.IntentType.IMPLICIT => sinkflag = true
                  }
              }
          }
        }
    }
    sinkflag
	}
  
  def checkIccSource(iddg : InterProceduralDataDependenceGraph[CGNode], entNode : CGNode, sinkNodes : ISet[CGNode]) : Boolean = {
    var sourceflag = true
    val reachableSinks = sinkNodes.filter{sinN => iddg.findPath(entNode, sinN) != null}
    if(!reachableSinks.isEmpty){
	    val sinkProcs = reachableSinks.filter(_.isInstanceOf[CGCallNode]).map(_.asInstanceOf[CGCallNode].getCalleeSet).reduce(iunion[JawaProcedure])
	    require(!sinkProcs.isEmpty)
	    val neededPermissions = sinkProcs.map(sin => this.apiPermissions.getOrElse(sin.getSignature, isetEmpty)).reduce(iunion[String])
	    val infos = AppCenter.getAppInfo.getComponentInfos
	    infos.foreach{
	      info =>
	        if(info.name == entNode.getOwner.getDeclaringRecord.getName){
	          if(info.exported == true){
	            if(info.permission.isDefined){
	              sourceflag = !(neededPermissions - info.permission.get).isEmpty
	            }
	          }
	        }
	    }
    }
    sourceflag
  }

	
	def addSource(source : String, category : Category.Value) = {
	  this.sources += (source -> category)
	  this.apiPermissions += (source -> this.apiPermissions.getOrElse(source, isetEmpty))
	}
	
	def addSink(sink : String, category : Category.Value) = {
	  this.sinks += (sink -> category)
	  this.apiPermissions += (sink -> this.apiPermissions.getOrElse(sink, isetEmpty))
	}
	
	def getSourceSigs : ISet[String] = this.sources.map{_._1}.toSet
	def getSinkSigs : ISet[String] = this.sinks.map{_._1}.toSet
	def getInterestedSigs : ISet[String] = getSourceSigs ++ getSinkSigs
}

object SSParser{
  
	private val regex = "(\\[\\|.+\\|\\])\\s*(.+)?\\s+->\\s+(.+)"
  def parse(filePath : String) : (IMap[String, ISet[String]], IMap[String, ISet[String]]) = {
	  def readFile : BufferedReader = new BufferedReader(new FileReader(filePath))
    var sources : IMap[String, ISet[String]] = imapEmpty
    var sinks : IMap[String, ISet[String]] = imapEmpty
    val p : Pattern = Pattern.compile(regex)
    val rdr = readFile
    var line = rdr.readLine()
    while(line != null){
      val m = p.matcher(line)
      if(m.find()){
        val (tag, apiSig, permissions) = parseLine(m)
        tag match{
          case "_SOURCE_" => sources += (apiSig -> permissions)
          case "_SINK_" => sinks += (apiSig -> permissions)
          case "_NONE_" =>
          case _ => throw new RuntimeException("Not expected tag: " + tag)
        }
      } else {
        throw new RuntimeException("Did not match the regex: " + line)
      }
      line = rdr.readLine()
    }
    (sources, sinks)
  }
  
  def parseLine(m : Matcher) : (String, String, ISet[String]) = {
    require(m.group(1) != null && m.group(3) != null)
    val apiSig = m.group(1)
    val rawps = m.group(2)
    var permissions : ISet[String] = isetEmpty
    if(rawps != null){
      permissions ++= rawps.split(" ")
    }
    val tag = m.group(3)
    (tag, apiSig, permissions)
  }
}