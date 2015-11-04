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
import org.sireum.jawa.JawaMethod
import org.sireum.amandroid.parser.LayoutControl
import org.sireum.amandroid.parser.ARSCFileParser
import java.util.regex.Pattern
import java.util.regex.Matcher
import org.sireum.amandroid.AndroidConstants
import org.sireum.pilar.ast.LocationDecl
import org.sireum.jawa.alir.util.ExplicitValueFinder
import org.sireum.pilar.ast.JumpLocation
import java.io.File
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.IntentHelper
import org.sireum.jawa.alir.controlFlowGraph._
import org.sireum.jawa.alir.dataDependenceAnalysis.InterProceduralDataDependenceGraph
import org.sireum.pilar.ast._
import java.io.InputStreamReader
import java.io.FileInputStream
import org.sireum.jawa.alir.interProcedural.Callee
import org.sireum.jawa.alir.taintAnalysis.SourceAndSinkManager
import org.sireum.jawa.alir.pta.PTAResult
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.model.InterComponentCommunicationModel
import org.sireum.jawa.alir.pta.VarSlot
import org.sireum.jawa.Signature
import org.sireum.amandroid.Apk
import org.sireum.jawa.Global
import org.sireum.jawa.alir.interProcedural.InterProceduralNode
import org.sireum.jawa.alir.taintAnalysis.TaintNode
import org.sireum.jawa.alir.taintAnalysis.TaintSource
import org.sireum.jawa.alir.taintAnalysis.TaintSink
import org.sireum.jawa.alir.dataDependenceAnalysis._
import org.sireum.jawa.alir.taintAnalysis.TagTaintDescriptor
import org.sireum.jawa.alir.taintAnalysis.TypeTaintDescriptor

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
abstract class AndroidSourceAndSinkManager(
    global: Global,
    apk: Apk,
    layoutControls: Map[Int, LayoutControl], 
    callbackMethods: ISet[JawaMethod], 
    sasFilePath: String) extends SourceAndSinkManager{
  
  private final val TITLE = "BasicSourceAndSinkManager"

  private def matchs(method: JawaMethod, methodpool: ISet[String]): Boolean = methodpool.exists{
    sig =>
      sig == method.getSignature.signature.replaceAll("\\*", "")
  }

  def isSourceMethod(method: JawaMethod) = matchs(method, this.sources.map(s => s._1).toSet)

  def isSink(method: JawaMethod) = matchs(method, this.sinks.map(s => s._1).toSet)

  def isSource(calleeMethod: JawaMethod, callerMethod: JawaMethod, callerLoc: JumpLocation): Boolean = {
    if(isSourceMethod(calleeMethod)) return true
    if(isUISource(calleeMethod, callerMethod, callerLoc)) return true
    false
  }

  def isSource(loc: LocationDecl, ptaresult: PTAResult): Boolean = false

  def isSink(loc: LocationDecl, ptaresult: PTAResult): Boolean = false

  def isCallbackSource(proc: JawaMethod): Boolean
  def isUISource(calleeMethod: JawaMethod, callerMethod: JawaMethod, callerLoc: JumpLocation): Boolean
  def isIccSink(invNode: ICFGInvokeNode, s: PTAResult): Boolean
  def isIccSource(entNode: ICFGNode, iddgEntNode: ICFGNode): Boolean

  def getSourceSigs: ISet[String] = this.sources.map{_._1}.toSet
  def getSinkSigs: ISet[String] = this.sinks.map{_._1}.toSet
  def getInterestedSigs: ISet[String] = getSourceSigs ++ getSinkSigs

}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
class DefaultAndroidSourceAndSinkManager(
    global: Global,
    apk: Apk, 
    layoutControls: Map[Int, LayoutControl], 
    callbackMethods: ISet[JawaMethod], 
    sasFilePath: String) extends AndroidSourceAndSinkManager(global, apk, layoutControls, callbackMethods, sasFilePath){

  private final val TITLE = "DefaultSourceAndSinkManager"
  
  def getSourceAndSinkNode[N <: InterProceduralNode](node: N, ptaresult: PTAResult): (ISet[TaintSource[N]], ISet[TaintSink[N]]) = {
    node match {
      case icfgN: ICFGNode => handleICFGNode(icfgN, ptaresult)
      case iddgN: IDDGNode => handleIDFGNode(iddgN, ptaresult)
      case _ => (isetEmpty, isetEmpty)
    }
  }
  
  private def getSourceTags(calleep: JawaMethod): ISet[String] = {
    this.sources.filter(_._1 == calleep.getSignature.signature.replaceAll("\\*", "")).map(_._2).fold(isetEmpty)(iunion _)
  }
  
  private def getSinkTags(calleep: JawaMethod): ISet[String] = {
    this.sinks.filter(_._1 == calleep.getSignature.signature.replaceAll("\\*", "")).map(_._2._2).fold(isetEmpty)(iunion _)
  }
  
  private def handleICFGNode[N <: InterProceduralNode](icfgN: ICFGNode, ptaresult: PTAResult): (ISet[TaintSource[N]], ISet[TaintSink[N]]) = {
    val sources = msetEmpty[TaintSource[N]]
    val sinks = msetEmpty[TaintSink[N]]
    val gNode = icfgN.asInstanceOf[N]
    icfgN match {
      case invNode: ICFGInvokeNode =>
        val calleeSet = invNode.getCalleeSet
        calleeSet.foreach{
          callee =>
            val calleep = callee.callee
            val calleesig = calleep.getSignature
            val caller = global.getMethod(invNode.getOwner).get
            val jumpLoc = caller.getBody.location(invNode.getLocIndex).asInstanceOf[JumpLocation]
            if(this.isSource(calleep, caller, jumpLoc)) {
              val tags = getSourceTags(calleep)
              global.reporter.echo(TITLE, "found source: " + calleep + "@" + invNode.getContext + " " + tags)
              val tn = TaintSource(gNode, TagTaintDescriptor(calleesig.signature, isetEmpty, SourceAndSinkCategory.API_SOURCE, tags))
              sources += tn
            }
            if(this.isSink(calleep)) {
              val tags = getSinkTags(calleep)
              global.reporter.echo(TITLE, "found sink: " + calleep + "@" + invNode.getContext + " " + tags)
              val poss = this.sinks.filter(_._1 == calleesig.signature.replaceAll("\\*", "")).map(_._2._1).fold(isetEmpty)(iunion _)
              val tn = TaintSink(gNode, TagTaintDescriptor(calleesig.signature, poss, SourceAndSinkCategory.API_SINK, tags))
              sinks += tn
            }
            if(invNode.isInstanceOf[ICFGCallNode] && this.isIccSink(invNode.asInstanceOf[ICFGCallNode], ptaresult)) {
              global.reporter.echo(TITLE, "found icc sink: " + invNode)
              val tn = TaintSink(gNode, TagTaintDescriptor(invNode.getLocUri, Set(1), SourceAndSinkCategory.ICC_SINK, Set("ICC")))
              sinks += tn
            }
        }
      case entNode: ICFGEntryNode =>
        if(this.isIccSource(entNode)){
          global.reporter.echo(TITLE, "found icc source: " + entNode)
          val tn = TaintSource(gNode, TagTaintDescriptor(entNode.getOwner.signature, isetEmpty, SourceAndSinkCategory.ICC_SOURCE, Set("ICC")))
          sources += tn
        }
        if(this.isCallbackSource(global.getMethod(entNode.getOwner).get)){
          global.reporter.echo(TITLE, "found callback source: " + entNode)
          val tn = TaintSource(gNode, TagTaintDescriptor(entNode.getOwner.signature, isetEmpty, SourceAndSinkCategory.CALLBACK_SOURCE, Set("CALL_BACK")))
          sources += tn
        }
      case normalNode: ICFGNormalNode =>
        val owner = global.getMethod(normalNode.getOwner).get
        val loc = owner.getBody.location(normalNode.getLocIndex)
        if(this.isSource(loc, ptaresult)){
          global.reporter.echo(TITLE, "found simple statement source: " + normalNode)
          val tn = TaintSource(gNode, TagTaintDescriptor(normalNode.getOwner.signature, isetEmpty, SourceAndSinkCategory.STMT_SOURCE, isetEmpty))
          sources += tn
        }
        if(this.isSink(loc, ptaresult)){
          global.reporter.echo(TITLE, "found simple statement sink: " + normalNode)
          val tn = TaintSink(gNode, TagTaintDescriptor(normalNode.getOwner.signature, isetEmpty, SourceAndSinkCategory.STMT_SINK, isetEmpty))
          sinks += tn
        }
      case _ =>
    }
    (sources.toSet, sinks.toSet)
  }
  
  private def handleIDFGNode[N <: InterProceduralNode](iddgN: IDDGNode, ptaresult: PTAResult): (ISet[TaintSource[N]], ISet[TaintSink[N]]) = {
    val sources = msetEmpty[TaintSource[N]]
    val sinks = msetEmpty[TaintSink[N]]
    val gNode = iddgN.asInstanceOf[N]
    iddgN match {
      case invNode: IDDGInvokeNode =>
        val calleeSet = invNode.getCalleeSet
        calleeSet.foreach{
          callee =>
            val calleesig = callee.callee.getSignature
            val calleep = callee.callee
            val caller = global.getMethod(invNode.getOwner).get
            val jumpLoc = caller.getBody.location(invNode.getLocIndex).asInstanceOf[JumpLocation]
            if(this.isSource(calleep, caller, jumpLoc)){
              global.reporter.echo(TITLE, "found source: " + calleep + "@" + invNode.getContext)
              val tn = TaintSource(gNode, TypeTaintDescriptor(calleep.getSignature.signature, None, SourceAndSinkCategory.API_SOURCE))
              sources += tn
            }
            if(invNode.isInstanceOf[IDDGCallArgNode] && this.isSink(calleep)){
              val poss = this.sinks.filter(_._1 == calleep.getSignature.signature.replaceAll("\\*", "")).map(_._2._1).fold(isetEmpty)(iunion _)
              if(poss.isEmpty || poss.contains(invNode.asInstanceOf[IDDGCallArgNode].position)) {
                global.reporter.echo(TITLE, "found sink: " + calleep + "@" + invNode.getContext + " " + invNode.asInstanceOf[IDDGCallArgNode].position)
                val tn = TaintSink(gNode, TypeTaintDescriptor(calleep.getSignature.signature, Some(invNode.asInstanceOf[IDDGCallArgNode].position), SourceAndSinkCategory.API_SINK))
                sinks += tn
              }
            }
            if(invNode.isInstanceOf[IDDGCallArgNode] && invNode.asInstanceOf[IDDGCallArgNode].position == 1 && this.isIccSink(invNode.getICFGNode.asInstanceOf[ICFGCallNode], ptaresult)){
              global.reporter.echo(TITLE, "found icc sink: " + invNode)
              val tn = TaintSink(gNode, TypeTaintDescriptor(invNode.getLocUri, Some(1), SourceAndSinkCategory.ICC_SINK))
              sinks += tn
            }
        }
      case entNode: IDDGEntryParamNode =>
        if(this.isIccSource(entNode.getICFGNode)){
          global.reporter.echo(TITLE, "found icc source: " + entNode)
          val tn = TaintSource(gNode, TypeTaintDescriptor(entNode.getOwner.signature, None, SourceAndSinkCategory.ICC_SOURCE))
          sources += tn
        }
        if(entNode.position > 0 && this.isCallbackSource(global.getMethod(entNode.getOwner).get)){
          global.reporter.echo(TITLE, "found callback source: " + entNode)
          val tn = TaintSource(gNode, TypeTaintDescriptor(entNode.getOwner.signature, None, SourceAndSinkCategory.CALLBACK_SOURCE))
          sources += tn
        }
      case normalNode: IDDGNormalNode =>
        val owner = global.getMethod(normalNode.getOwner).get
        val loc = owner.getBody.location(normalNode.getLocIndex)
        if(this.isSource(loc, ptaresult)){
          global.reporter.echo(TITLE, "found simple statement source: " + normalNode)
          val tn = TaintSource(gNode, TypeTaintDescriptor(normalNode.getOwner.signature, None, SourceAndSinkCategory.STMT_SOURCE))
          sources += tn
        }
        if(this.isSink(loc, ptaresult)){
          global.reporter.echo(TITLE, "found simple statement sink: " + normalNode)
          val tn = TaintSink(gNode, TypeTaintDescriptor(normalNode.getOwner.signature, None, SourceAndSinkCategory.STMT_SINK))
          sinks += tn
        }
      case _ =>
    }
    (sources.toSet, sinks.toSet)
  }
  
  def isCallbackSource(proc: JawaMethod): Boolean = {
    if(this.callbackMethods.contains(proc) && proc.getParamNames.size > 1) true
    else false
  }

  def isUISource(calleeMethod: JawaMethod, callerMethod: JawaMethod, callerLoc: JumpLocation): Boolean = {
    if(calleeMethod.getSignature.signature == AndroidConstants.ACTIVITY_FINDVIEWBYID || calleeMethod.getSignature.signature == AndroidConstants.VIEW_FINDVIEWBYID){
      val nums = ExplicitValueFinder.findExplicitIntValueForArgs(callerMethod, callerLoc, 1)
      nums.foreach{
        num =>
          this.layoutControls.get(num) match{
            case Some(control) =>
              return control.isSensitive
            case None =>
              calleeMethod.getDeclaringClass.global.reporter.echo(TITLE, "Layout control with ID " + num + " not found.")
          }
      }
    }
    false
  }

  def isIccSink(invNode: ICFGInvokeNode, s: PTAResult): Boolean = {
    var sinkflag = false
    val calleeSet = invNode.getCalleeSet
    calleeSet.foreach{
      callee =>
        if(InterComponentCommunicationModel.isIccOperation(callee.callee)){
          val args = global.getMethod(invNode.getOwner).get.getBody.location(invNode.getLocIndex).asInstanceOf[JumpLocation].jump.asInstanceOf[CallJump].callExp.arg match {
            case te: TupleExp =>
              te.exps.map{
                exp =>
                  exp match{
                    case ne: NameExp => ne.name.name
                    case _ => exp.toString()
                  }
              }.toList
            case a => throw new RuntimeException("wrong exp type: " + a)
          }
          val intentSlot = VarSlot(args(1), false, true)
          val intentValues = s.pointsToSet(intentSlot, invNode.getContext)
          val intentContents = IntentHelper.getIntentContents(s, intentValues, invNode.getContext)
          val compType = AndroidConstants.getIccCallType(callee.callee.getSubSignature)
          val comMap = IntentHelper.mappingIntents(global, apk, intentContents, compType)
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
  
  def isIccSource(entNode: ICFGNode): Boolean = {
    var sourceflag = false
//    val reachableSinks = sinkNodes.filter{sinN => iddg.findPath(entNode, sinN) != null}
//    if(!reachableSinks.isEmpty){
//    val sinkMethods = reachableSinks.filter(_.isInstanceOf[ICFGCallNode]).map(_.asInstanceOf[ICFGCallNode].getCalleeSet).reduce(iunion[Callee])
//    require(!sinkMethods.isEmpty)
//    val neededPermissions = sinkMethods.map(sin => this.apiPermissions.getOrElse(sin.calleeMethod.getSignature, isetEmpty)).reduce(iunion[String])
//    val infos = AppCenter.getAppInfo.getComponentInfos
//    infos.foreach{
//      info =>
//        if(info.name == entNode.getOwner.getDeclaringClass.getName){
//          if(info.exported == true){
//            if(info.permission.isDefined){
//              sourceflag = !(neededPermissions - info.permission.get).isEmpty
//            }
//          }
//        }
//    }
//    }
    sourceflag
  }
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
class DataLeakageAndroidSourceAndSinkManager(
    global: Global,
    apk: Apk, 
    layoutControls: Map[Int, LayoutControl], 
    callbackMethods: ISet[JawaMethod], 
    sasFilePath: String) extends DefaultAndroidSourceAndSinkManager(global, apk, layoutControls, callbackMethods, sasFilePath){
  
  private final val TITLE = "DataLeakageAndroidSourceAndSinkManager"
  
  private def sensitiveData: ISet[String] = Set("android.location.Location", "android.content.Intent")
  
  override def isCallbackSource(proc: JawaMethod): Boolean = {
    if(this.callbackMethods.contains(proc)){
      if(proc.getParamTypes.exists { pt => sensitiveData.contains(pt.name) }) true
      else false
    }
    else false
  }
}