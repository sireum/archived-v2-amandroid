/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.alir.taintAnalysis

import java.io.BufferedReader
import java.io.FileReader
import org.sireum.util._
import org.sireum.jawa.JawaProcedure
import org.sireum.amandroid.parser.LayoutControl
import org.sireum.amandroid.parser.ARSCFileParser
import java.util.regex.Pattern
import java.util.regex.Matcher
import org.sireum.jawa.Center
import org.sireum.jawa.util.StringFormConverter
import org.sireum.amandroid.AndroidConstants
import org.sireum.pilar.ast.LocationDecl
import org.sireum.jawa.alir.util.ExplicitValueFinder
import org.sireum.pilar.ast.JumpLocation
import org.sireum.jawa.MessageCenter._
import java.io.File
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.IntentHelper
import org.sireum.jawa.alir.controlFlowGraph._
import org.sireum.jawa.alir.dataDependenceAnalysis.InterProceduralDataDependenceGraph
import org.sireum.amandroid.AppCenter
import org.sireum.pilar.ast._
import java.io.InputStreamReader
import java.io.FileInputStream
import org.sireum.jawa.alir.interProcedural.Callee
import org.sireum.jawa.alir.taintAnalysis.SourceAndSinkManager
import org.sireum.jawa.alir.pta.PTAResult
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.model.InterComponentCommunicationModel
import org.sireum.jawa.alir.pta.VarSlot

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object SourceAndSinkCategory {
  final val STMT_SOURCE = "stmt_source"
  final val STMT_SINK = "stmt_sink"
  final val API_SOURCE = "api_source"
  final val API_SINK = "api_sink"
  final val ICC_SOURCE = "icc_source"
  final val ICC_SINK = "icc_sink"
  final val CALLBACK_SOURCE = "callback_source"
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
abstract class AndroidSourceAndSinkManager(appPackageName : String, 
    												layoutControls : Map[Int, LayoutControl], 
    												callbackMethods : ISet[JawaProcedure], 
    												sasFilePath : String) extends SourceAndSinkManager{
  
  private final val TITLE = "BasicSourceAndSinkManager"
  
  /**
   * it's a map from source API sig to it's category
   */
	protected var sources : IMap[String, String] = imapEmpty
	/**
   * it's a map from sink API sig to it's category
   */
	protected var sinks : IMap[String, String] = imapEmpty
	/**
   * it's a map from API sig to its required permission
   */
	protected var apiPermissions : IMap[String, ISet[String]] = imapEmpty

  SSParser.parse(sasFilePath) match{
    case (sources, sinks) => 
      sources.foreach{
        case (sig, ps) =>
          this.sources += (sig -> SourceAndSinkCategory.API_SOURCE)
          this.apiPermissions += (sig -> ps)
      }
      sinks.foreach{
        case (sig, ps) =>
          this.sinks += (sig -> SourceAndSinkCategory.API_SINK)
          this.apiPermissions += (sig -> ps)
      }
      msg_detail(TITLE, "source size: " + this.sources.size + " sink size: " + this.sinks.size)
  }
	
	private def matchs(procedure : JawaProcedure, procedurepool : ISet[String]) : Boolean = procedurepool.contains(procedure.getSignature)
	
	def isSourceProcedure(procedure : JawaProcedure) = matchs(procedure, this.sources.map(s=>s._1).toSet)
	
	def isSinkProcedure(procedure : JawaProcedure) = matchs(procedure, this.sinks.map(s=>s._1).toSet)
	
	def isSource(calleeProcedure : JawaProcedure, callerProcedure : JawaProcedure, callerLoc : JumpLocation) : Boolean = {
	  if(isSourceProcedure(calleeProcedure)) return true
	  if(isUISource(calleeProcedure, callerProcedure, callerLoc)) return true
	  false
	}
	
	def isSource(loc : LocationDecl, ptaresult : PTAResult) : Boolean = false
	
	def isSink(loc : LocationDecl, ptaresult : PTAResult) : Boolean = false
	
	def addSource(source : String, category : String) = {
	  this.sources += (source -> category)
	  this.apiPermissions += (source -> this.apiPermissions.getOrElse(source, isetEmpty))
	}
	
	def addSink(sink : String, category : String) = {
	  this.sinks += (sink -> category)
	  this.apiPermissions += (sink -> this.apiPermissions.getOrElse(sink, isetEmpty))
	}
	
	def isCallbackSource(proc : JawaProcedure) : Boolean
	def isUISource(calleeProcedure : JawaProcedure, callerProcedure : JawaProcedure, callerLoc : JumpLocation) : Boolean
	def isIccSink(invNode : ICFGInvokeNode, s : PTAResult) : Boolean
	def isIccSource(entNode : ICFGNode, iddgEntNode : ICFGNode) : Boolean
	
	def getSourceSigs : ISet[String] = this.sources.map{_._1}.toSet
	def getSinkSigs : ISet[String] = this.sinks.map{_._1}.toSet
	def getInterestedSigs : ISet[String] = getSourceSigs ++ getSinkSigs
	
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
class DefaultAndroidSourceAndSinkManager(appPackageName : String, 
    												layoutControls : Map[Int, LayoutControl], 
    												callbackMethods : ISet[JawaProcedure], 
    												sasFilePath : String) extends AndroidSourceAndSinkManager(appPackageName, layoutControls, callbackMethods, sasFilePath){
	
  private final val TITLE = "DefaultSourceAndSinkManager"
  
	def isCallbackSource(proc : JawaProcedure) : Boolean = {
	  if(this.callbackMethods.contains(proc) && proc.getParamNames.size > 1) true
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
	            err_msg_normal(TITLE, "Layout control with ID " + num + " not found.")
	        }
	    }
	  }
	  false
	}
	
	def isIccSink(invNode : ICFGInvokeNode, s : PTAResult) : Boolean = {
    var sinkflag = false
    val calleeSet = invNode.getCalleeSet
    calleeSet.foreach{
      callee =>
        if(InterComponentCommunicationModel.isIccOperation(callee.callee)){
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
          val intentValues = s.pointsToSet(intentSlot, invNode.getContext)
          val intentContents = IntentHelper.getIntentContents(s, intentValues, invNode.getContext)
          val comMap = IntentHelper.mappingIntents(intentContents)
          comMap.foreach{
            case (_, coms) =>
              if(coms.isEmpty) sinkflag = true
              coms.foreach{
                case (com, typ) =>
                  typ match {
                    case IntentHelper.IntentType.EXPLICIT => if(com.isUnknown) sinkflag = true
//                    case IntentHelper.IntentType.EXPLICIT => sinkflag = true
                    case IntentHelper.IntentType.IMPLICIT => sinkflag = true
                  }
              }
          }
        }
    }
    sinkflag
	}
  
  def isIccSource(entNode : ICFGNode, iddgEntNode : ICFGNode) : Boolean = {
    var sourceflag = false
//    val reachableSinks = sinkNodes.filter{sinN => iddg.findPath(entNode, sinN) != null}
//    if(!reachableSinks.isEmpty){
//	    val sinkProcs = reachableSinks.filter(_.isInstanceOf[ICFGCallNode]).map(_.asInstanceOf[ICFGCallNode].getCalleeSet).reduce(iunion[Callee])
//	    require(!sinkProcs.isEmpty)
//	    val neededPermissions = sinkProcs.map(sin => this.apiPermissions.getOrElse(sin.calleeProc.getSignature, isetEmpty)).reduce(iunion[String])
//	    val infos = AppCenter.getAppInfo.getComponentInfos
//	    infos.foreach{
//	      info =>
//	        if(info.name == entNode.getOwner.getDeclaringRecord.getName){
//	          if(info.exported == true){
//	            if(info.permission.isDefined){
//	              sourceflag = !(neededPermissions - info.permission.get).isEmpty
//	            }
//	          }
//	        }
//	    }
//    }
    sourceflag
  }
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
class DataLeakageAndroidSourceAndSinkManager(appPackageName : String, 
                            layoutControls : Map[Int, LayoutControl], 
                            callbackMethods : ISet[JawaProcedure], 
                            sasFilePath : String) extends DefaultAndroidSourceAndSinkManager(appPackageName, layoutControls, callbackMethods, sasFilePath){
  
  private final val TITLE = "DataLeakageAndroidSourceAndSinkManager"
  
  override def isCallbackSource(proc : JawaProcedure) : Boolean = {
    if(this.callbackMethods.contains(proc) && proc.getParamNames.size > 1) false
    else false
  }
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object SSParser{
  
	private val regex = "([^\\s]+)\\s+(.+)?\\s*->\\s+(.+)"
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