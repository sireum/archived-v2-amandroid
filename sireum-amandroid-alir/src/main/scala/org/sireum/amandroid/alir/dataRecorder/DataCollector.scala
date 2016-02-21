/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Main Contributors:
 *    Fengguo Wei - Argus Lab @ University of South Florida
 *    Sankardas Roy - Bowling Green State University
 *    
 * Contributors:
 *    Robby - Santos Lab @ Kansas State University
 *    Wu Zhou - Fireeye
 *    Fengchi Lin - Chinese People's Public Security University
 ******************************************************************************/
package org.sireum.amandroid.alir.dataRecorder

import org.sireum.util._
import org.sireum.amandroid.parser.IntentFilter
import org.sireum.jawa.alir.taintAnalysis.TaintAnalysisResult
import org.stringtemplate.v4.STGroupFile
import java.util.ArrayList
import org.sireum.jawa.alir.controlFlowGraph.ICFGCallNode
import org.sireum.jawa.alir.pta.reachingFactsAnalysis.ReachingFactsAnalysisHelper
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.IntentHelper
import org.sireum.pilar.ast._
import org.sireum.amandroid.parser.UriData
import org.sireum.jawa.alir.Context
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.model.InterComponentCommunicationModel
import org.sireum.jawa.alir.dataFlowAnalysis.InterProceduralDataFlowGraph
import org.sireum.jawa.alir.pta.VarSlot
import org.sireum.amandroid.AndroidConstants
import org.sireum.amandroid.Apk
import org.sireum.jawa.Global
import org.sireum.jawa.Signature
import org.sireum.jawa.alir.interProcedural.InterProceduralNode
import org.sireum.alir.AlirEdge
import org.sireum.amandroid.parser.ComponentType

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object DataCollector {
  
  private val template = new STGroupFile("org/sireum/amandroid/alir/resources/dataRecorder/AppData.stg")
  
  private def getIntentFilterStrings(intentFilters: ISet[IntentFilter]): ArrayList[String] = {
    val intFs: ArrayList[String] = new ArrayList[String]
    intentFilters.foreach{
      intfilter =>
        val intF = template.getInstanceOf("IntentFilter")
        val actions = intfilter.getActions
        if(!actions.isEmpty){
          val actionStrings: ArrayList[String] = new ArrayList[String]
          actions.foreach(f=>actionStrings.add(f))
          intF.add("actions", actionStrings)
        }
        val categories = intfilter.getCategorys
        if(!categories.isEmpty){
          val categoryStrings: ArrayList[String] = new ArrayList[String]
          categories.foreach(f=>categoryStrings.add(f))
          intF.add("categories", categoryStrings)
        }
        val data = intfilter.getData
        if(!data.isEmpty){
          val dataT = template.getInstanceOf("Data")
          val schemes = data.getSchemes
          if(!schemes.isEmpty){
            val schemeStrings: ArrayList[String] = new ArrayList[String]
            schemes.foreach(f=>schemeStrings.add(f))
            dataT.add("schemes", schemeStrings)
          }
          val authorities = data.getAuthorities
          if(!authorities.isEmpty){
            val hostStrings: ArrayList[String] = new ArrayList[String]
            val portStrings: ArrayList[String] = new ArrayList[String]
            authorities.foreach{f=>hostStrings.add(f.host);portStrings.add(f.port)}
            dataT.add("hosts", hostStrings)
            dataT.add("ports", portStrings)
          }
          val paths = data.getPaths
          if(!paths.isEmpty){
            val pathStrings: ArrayList[String] = new ArrayList[String]
            paths.foreach(f=>pathStrings.add(f))
            dataT.add("paths", pathStrings)
          }
          val pathPrefixs = data.getPathPrefixs
          if(!pathPrefixs.isEmpty){
            val pathPrefixStrings: ArrayList[String] = new ArrayList[String]
            pathPrefixs.foreach(f=>pathPrefixStrings.add(f))
            dataT.add("pathPrefixs", pathPrefixStrings)
          }
          val pathPatterns = data.getPathPatterns
          if(!pathPatterns.isEmpty){
            val pathPatternStrings: ArrayList[String] = new ArrayList[String]
            pathPatterns.foreach(f=>pathPatternStrings.add(f))
            dataT.add("pathPatterns", pathPatternStrings)
          }
          val mimeTypes = data.getMimeTypes
          if(!mimeTypes.isEmpty){
            val mimeTypeStrings: ArrayList[String] = new ArrayList[String]
            mimeTypes.foreach(f=>mimeTypeStrings.add(f))
            dataT.add("mimeTypes", mimeTypeStrings)
          }
          intF.add("data", dataT.render())
        }
        intFs.add(intF.render())
    }
    intFs
  }
  
  final case class AppData(
      name: String, 
      uses_permissions: ISet[String],
      components: ISet[ComponentData],
      taintResultOpt: Option[TaintAnalysisResult[InterProceduralNode, AlirEdge[InterProceduralNode]]]){
    override def toString: String = {
      val appData = template.getInstanceOf("AppData")
      appData.add("name", name)
      val up: ArrayList[String] = new ArrayList[String]
      uses_permissions.foreach(f=>up.add(f))
      appData.add("uses_permissions", up)
      val comps: ArrayList[String] = new ArrayList[String]
      components.foreach(f=>comps.add(f.toString))
      appData.add("components", comps)
      val taintResultT = template.getInstanceOf("TaintResult")
      val sourceStrings: ArrayList[String] = new ArrayList[String]
      if(taintResultOpt.isDefined){
        taintResultOpt.get.getSourceNodes.foreach{
          sn =>
            val ssInfo = template.getInstanceOf("SourceSinkInfo")
            val descriptorStrings: ArrayList[String] = new ArrayList[String]
            descriptorStrings.add(sn.descriptor.toString())
            ssInfo.add("descriptors", descriptorStrings)
            sourceStrings.add(ssInfo.render())
        }
        val sinkStrings: ArrayList[String] = new ArrayList[String]
        taintResultOpt.get.getSinkNodes.foreach{
          sn =>
            val ssInfo = template.getInstanceOf("SourceSinkInfo")
            val descriptorStrings: ArrayList[String] = new ArrayList[String]
            descriptorStrings.add(sn.descriptor.toString())
            ssInfo.add("descriptors", descriptorStrings)
            sinkStrings.add(ssInfo.render())
        }
        taintResultT.add("sources", sourceStrings)
        taintResultT.add("sinks", sinkStrings)
        val pathStrings: ArrayList[String] = new ArrayList[String]
        val taintPaths = taintResultOpt.get.getTaintedPaths
        taintPaths.foreach{
          taintPath =>
            val path = template.getInstanceOf("TaintPath")
            val sourcessInfo = template.getInstanceOf("SourceSinkInfo")
            val sourceDescriptorStrings: ArrayList[String] = new ArrayList[String]
            sourceDescriptorStrings.add(taintPath.getSource.descriptor.toString())
            sourcessInfo.add("descriptors", sourceDescriptorStrings)
            path.add("source", sourcessInfo)
            val sinkssInfo = template.getInstanceOf("SourceSinkInfo")
            val sinkDescriptorStrings: ArrayList[String] = new ArrayList[String]
            sinkDescriptorStrings.add(taintPath.getSink.descriptor.toString())
            sinkssInfo.add("descriptors", sinkDescriptorStrings)
            path.add("sink", sinkssInfo)
            val typStrings: ArrayList[String] = new ArrayList[String]
            taintPath.getTypes.foreach(f=>typStrings.add(f))
            path.add("typs", typStrings)
            val pathString: ArrayList[String] = new ArrayList[String]
            val paths = taintPath.getPath
            if(paths.size > 1) {
              paths.tail.foreach{
                edge =>
                  pathString.add(edge.target + "  ->")
              }
              pathString.add(paths.head.source.toString())
            } else if(paths.size == 1) {
              pathString.add(paths.head.target + "  ->")
              pathString.add(paths.head.source.toString())
            }
            path.add("path", pathString)
            pathStrings.add(path.render())
        }
        taintResultT.add("paths", pathStrings)
        appData.add("taintResult", taintResultT)
      }
      appData.render()
    }
  }
  
  final case class IccInfo(
      procs: ISet[Signature],
      context: Context,
      intents: ISet[Intent]){
    override def toString: String = {
      val iccInfo = template.getInstanceOf("IccInfo")
      val procStrings = new ArrayList[String]
      procs.foreach{proc => procStrings.add(proc.signature)}
      iccInfo.add("procs", procStrings)
      iccInfo.add("context", context)
      val intentStrings = new ArrayList[String]
      intents.foreach(id => intentStrings.add(id.toString))
      iccInfo.add("intents", intentStrings)
      iccInfo.render()
    }
  }
  
  final case class Intent(
      componentNames: ISet[String],
      actions: ISet[String],
      categories: ISet[String],
      uriDatas: ISet[UriData],
      types: ISet[String],
      preciseExplicit: Boolean,
      preciseImplicit: Boolean,
      targets: ISet[(String, String)]){
    final val EXPLICIT = "EXPLICIT"
    final val IMPLICIT = "IMPLICIT"
    final val MIXED = "mixed"
    def getType: String = {
      if(!componentNames.isEmpty && (!actions.isEmpty || !categories.isEmpty || !uriDatas.isEmpty || !types.isEmpty))
        MIXED
      else if(!componentNames.isEmpty) EXPLICIT
      else IMPLICIT
    }
    override def toString: String = {
      val intent = template.getInstanceOf("Intent")
      if(!componentNames.isEmpty){
        val componentNameStrings = new ArrayList[String]
        componentNames.foreach(componentNameStrings.add(_))
        intent.add("componentNames", componentNameStrings)
      }
      if(!actions.isEmpty){
        val actionStrings = new ArrayList[String]
        actions.foreach(actionStrings.add(_))
        intent.add("actions", actionStrings)
      }
      if(!categories.isEmpty){
        val categoryStrings = new ArrayList[String]
        categories.foreach(categoryStrings.add(_))
        intent.add("categories", categoryStrings)
      }
      if(!uriDatas.isEmpty){
        val dataStrings = new ArrayList[String]
        uriDatas.foreach{
          data =>
            val uriData = template.getInstanceOf("UriData")
            val scheme = data.getScheme
            if(scheme != null){
              uriData.add("scheme", scheme)
            }
            val host = data.getHost
            if(host != null){
              uriData.add("host", host)
            }
            val port = data.getPort
            if(port != null){
              uriData.add("port", port)
            }
            val path = data.getPath
            if(path != null){
              uriData.add("path", path)
            }
            val pathPrefix = data.getPathPrefix
            if(pathPrefix != null){
              uriData.add("pathPrefix", pathPrefix)
            }
            val pathPattern = data.getPathPattern
            if(pathPattern != null){
              uriData.add("pathPattern", pathPattern)
            }
            dataStrings.add(uriData.render())
        }
        intent.add("datas", dataStrings)
      }
      if(!types.isEmpty){
        val typeStrings = new ArrayList[String]
        types.foreach(typeStrings.add(_))
        intent.add("typs", typeStrings)
      }
      val targetStrings = new ArrayList[String]
      targets.foreach{
        case (proc, typ) =>
          val target = template.getInstanceOf("Target")
          target.add("proc", proc)
          target.add("typ", typ)
          targetStrings.add(target.render())
      }
      intent.add("targets", targetStrings)
      intent.render()
    }
  }
     
  final case class ComponentData(
      name: String,
      typ: ComponentType.Value,
      exported: Boolean,
      dynamicReg: Boolean,
      protectPermission: ISet[String],
      intentFilters: ISet[IntentFilter],
      iccInfos: ISet[IccInfo]){
    override def toString: String = {
      val compData = template.getInstanceOf("ComponentData")
      compData.add("compName", name)
      val typstr = typ match {
        case ComponentType.ACTIVITY => "activity"
        case ComponentType.SERVICE => "service"
        case ComponentType.RECEIVER => "receiver"
        case ComponentType.PROVIDER => "provider"
      }
      compData.add("typ", typstr)
      compData.add("exported", exported)
      compData.add("dynamicReg", dynamicReg)
      val permissions = new ArrayList[String]
      import collection.JavaConversions._
      permissions ++= protectPermission
      compData.add("protectPermission", permissions)
      compData.add("intentFilters", getIntentFilterStrings(intentFilters))
      val iccInfoStrings = new ArrayList[String]
      iccInfos.foreach(iccinfo => iccInfoStrings.add(iccinfo.toString))
      compData.add("iccInfos", iccInfoStrings)
      compData.render()
    }
  }
  
  def collect(global: Global, apk: Apk) = {
    val appInfo = apk.getAppInfo
    val appName = apk.getAppName
    val uses_permissions = appInfo.getUsesPermissions
    val compInfos = appInfo.getComponentInfos
    val intentFDB = apk.getIntentFilterDB
    val compDatas = compInfos.map{
      comp =>
        val compTyp = comp.compType
        val compRec = global.getClassOrResolve(compTyp)
        val typ = comp.typ
        val exported = comp.exported
        val protectPermission = comp.permission
        val intentFilters = intentFDB.getIntentFilters(compTyp)
        var iccInfos = isetEmpty[IccInfo]
        var taintResult: Option[TaintAnalysisResult[InterProceduralNode, AlirEdge[InterProceduralNode]]] = None
        if(!compRec.isUnknown){
          if(apk.hasIDFG(compRec)) {
            val InterProceduralDataFlowGraph(icfg, ptaresult) = apk.getIDFG(compRec).get
            val iccNodes = icfg.nodes.filter{
              node =>
                node.isInstanceOf[ICFGCallNode] && node.asInstanceOf[ICFGCallNode].getCalleeSet.exists(c => InterComponentCommunicationModel.isIccOperation(c.callee))
            }.map(_.asInstanceOf[ICFGCallNode])
            iccInfos =
              iccNodes.map{
                iccNode =>
                  val iccMethod = global.getMethod(iccNode.getOwner).get
                  val args = iccMethod.getBody.location(iccNode.getLocIndex).asInstanceOf[JumpLocation].jump.asInstanceOf[CallJump].callExp.arg match{
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
                  val intentValues = ptaresult.pointsToSet(intentSlot, iccNode.context)
                  val intentcontents = IntentHelper.getIntentContents(ptaresult, intentValues, iccNode.getContext)
                  val compType = AndroidConstants.getIccCallType(iccNode.getCalleeSet.head.callee.getSubSignature)
                  val comMap = IntentHelper.mappingIntents(global, apk, intentcontents, compType)
                  val intents = intentcontents.map(ic=>Intent(ic.componentNames, ic.actions, ic.categories, ic.datas, ic.types, ic.preciseExplicit, ic.preciseImplicit, comMap(ic).map(c=>(c._1.getName, c._2.toString()))))
                  IccInfo(iccNode.getCalleeSet.map(_.callee.getSignature), iccNode.getContext, intents)
              }.toSet
          }
      }
      val dynamicReg = apk.getDynamicRegisteredReceivers.contains(compRec)
      ComponentData(compTyp.jawaName, typ, exported, dynamicReg, protectPermission, intentFilters, iccInfos)
    }
    val taintResult = apk.getTaintAnalysisResult[InterProceduralNode, AlirEdge[InterProceduralNode]]
    AppData(appName, uses_permissions.toSet, compDatas.toSet, taintResult)
  }
}
