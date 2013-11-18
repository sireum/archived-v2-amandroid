package org.sireum.amandroid.android.dataRecorder

import org.sireum.amandroid.android.AppCenter
import org.sireum.util._
import org.sireum.amandroid.android.parser.IntentFilter
import org.sireum.amandroid.interProcedural.taintAnalysis.TaintAnalysisResult
import org.sireum.amandroid.Center
import org.stringtemplate.v4.STGroupFile
import java.util.ArrayList
import org.sireum.amandroid.interProcedural.controlFlowGraph.CGCallNode
import org.sireum.amandroid.android.interProcedural.reachingFactsAnalysis.model.InterComponentCommunicationModel
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.ReachingFactsAnalysisHelper
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.VarSlot
import org.sireum.amandroid.android.interProcedural.reachingFactsAnalysis.IntentHelper
import org.sireum.pilar.ast._
import org.sireum.amandroid.android.parser.UriData
import org.sireum.amandroid.interProcedural.Context

object DataCollector {
  
  private val template = new STGroupFile("org/sireum/amandroid/android/resources/dataRecorder/AppData.stg")
  
  private def getIntentFilterStrings(intentFilters : ISet[IntentFilter]) : ArrayList[String] = {
    val intFs : ArrayList[String] = new ArrayList[String]
    intentFilters.foreach{
      intfilter =>
        val intF = template.getInstanceOf("IntentFilter")
        val actions = intfilter.getActions
        if(!actions.isEmpty){
          val actionStrings : ArrayList[String] = new ArrayList[String]
          actions.foreach(f=>actionStrings.add(f))
          intF.add("actions", actionStrings)
        }
        val categories = intfilter.getCategorys
        if(!categories.isEmpty){
          val categoryStrings : ArrayList[String] = new ArrayList[String]
          categories.foreach(f=>categoryStrings.add(f))
          intF.add("categories", categoryStrings)
        }
        val data = intfilter.getData
        if(!data.isEmpty){
          val dataT = template.getInstanceOf("Data")
          val schemes = data.getSchemes
          if(!schemes.isEmpty){
            val schemeStrings : ArrayList[String] = new ArrayList[String]
            schemes.foreach(f=>schemeStrings.add(f))
            dataT.add("schemes", schemeStrings)
          }
          val authorities = data.getAuthorities
          if(!authorities.isEmpty){
            val hostStrings : ArrayList[String] = new ArrayList[String]
            val portStrings : ArrayList[String] = new ArrayList[String]
            authorities.foreach{f=>hostStrings.add(f.host);portStrings.add(f.port)}
            dataT.add("hosts", hostStrings)
            dataT.add("ports", portStrings)
          }
          val paths = data.getPaths
          if(!paths.isEmpty){
            val pathStrings : ArrayList[String] = new ArrayList[String]
            paths.foreach(f=>pathStrings.add(f))
            dataT.add("paths", pathStrings)
          }
          val pathPrefixs = data.getPathPrefixs
          if(!pathPrefixs.isEmpty){
            val pathPrefixStrings : ArrayList[String] = new ArrayList[String]
            pathPrefixs.foreach(f=>pathPrefixStrings.add(f))
            dataT.add("pathPrefixs", pathPrefixStrings)
          }
          val pathPatterns = data.getPathPatterns
          if(!pathPatterns.isEmpty){
            val pathPatternStrings : ArrayList[String] = new ArrayList[String]
            pathPatterns.foreach(f=>pathPatternStrings.add(f))
            dataT.add("pathPatterns", pathPatternStrings)
          }
          val mimeTypes = data.getMimeTypes
          if(!mimeTypes.isEmpty){
            val mimeTypeStrings : ArrayList[String] = new ArrayList[String]
            mimeTypes.foreach(f=>mimeTypeStrings.add(f))
            dataT.add("mimeTypes", mimeTypeStrings)
          }
          intF.add("data", dataT.render())
        }
        intFs.add(intF.render())
    }
    intFs
  }
  
  final case class AppData(name : String, 
	    										 uses_permissions : ISet[String],
	    										 components : ISet[ComponentData],
	    										 dynamicRegisteredComponents : ISet[DynamicRegisteredComponentData]){
    override def toString : String = {
      val appData = template.getInstanceOf("AppData")
      appData.add("name", name)
      val up : ArrayList[String] = new ArrayList[String]
      uses_permissions.foreach(f=>up.add(f))
      appData.add("uses_permissions", up)
      val comps : ArrayList[String] = new ArrayList[String]
      components.foreach(f=>comps.add(f.toString))
      appData.add("components", comps)
      if(!dynamicRegisteredComponents.isEmpty){
        val drcomps : ArrayList[String] = new ArrayList[String]
	      dynamicRegisteredComponents.foreach(f=>drcomps.add(f.toString))
	      appData.add("dynamicRegisteredComponents", drcomps)
      }
      appData.render()
    }
  }
  
  final case class IccInfo(procs : ISet[String],
      										 context : Context,
      										 intentDatas : ISet[IntentData]){
    override def toString : String = {
      val iccInfo = template.getInstanceOf("IccInfo")
      val procStrings = new ArrayList[String]
      procs.foreach(procStrings.add(_))
      iccInfo.add("procs", procStrings)
      iccInfo.add("context", context)
      val intentDataStrings = new ArrayList[String]
      intentDatas.foreach(id => intentDataStrings.add(id.toString))
      iccInfo.add("intentDatas", intentDataStrings)
      iccInfo.render()
    }
  }
  
  final case class IntentData(componentNames : ISet[String],
	      									 	  actions : ISet[String],
	      										  categories : ISet[String],
	      										  uriDatas : ISet[UriData],
	      										  types : ISet[String],
	      										  preciseExplicit : Boolean,
	      										  preciseImplicit : Boolean,
	      										  targets : ISet[(String, String)]){
    final val EXPLICIT = "EXPLICIT"
    final val IMPLICIT = "IMPLICIT"
    final val MIXED = "mixed"
    def getType : String = {
      if(!componentNames.isEmpty && (!actions.isEmpty || !categories.isEmpty || !uriDatas.isEmpty || !types.isEmpty))
        MIXED
      else if(!componentNames.isEmpty) EXPLICIT
      else IMPLICIT
    }
    override def toString : String = {
      val intentData = template.getInstanceOf("IntentData")
      if(!componentNames.isEmpty){
	      val componentNameStrings = new ArrayList[String]
	      componentNames.foreach(componentNameStrings.add(_))
	      intentData.add("componentNames", componentNameStrings)
      }
      if(!actions.isEmpty){
	      val actionStrings = new ArrayList[String]
	      actions.foreach(actionStrings.add(_))
	      intentData.add("actions", actionStrings)
      }
      if(!categories.isEmpty){
	      val categoryStrings = new ArrayList[String]
	      categories.foreach(categoryStrings.add(_))
	      intentData.add("categories", categoryStrings)
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
	      intentData.add("datas", dataStrings)
      }
      if(!types.isEmpty){
	      val typeStrings = new ArrayList[String]
	      types.foreach(typeStrings.add(_))
	      intentData.add("typs", typeStrings)
      }
      val targetStrings = new ArrayList[String]
      targets.foreach{
        case (proc, typ) =>
          val target = template.getInstanceOf("Target")
          target.add("proc", proc)
          target.add("typ", typ)
          targetStrings.add(target.render())
      }
      intentData.add("targets", targetStrings)
      intentData.render()
    }
  }
  
  final case class DynamicRegisteredComponentData(
      													 name : String, 
	    													 typ : String,
	    													 protectPermission : Option[String],
	    													 intentFilters : ISet[IntentFilter],
	    													 precise : Boolean){
    override def toString : String = {
      val compData = template.getInstanceOf("DynamicRegisteredComponentData")
      compData.add("compName", name)
      compData.add("typ", typ)
      compData.add("protectPermission", protectPermission.getOrElse(null))
      compData.add("intentFilters", getIntentFilterStrings(intentFilters))
      compData.render()
    }
  }
    										 
  final case class ComponentData(name : String, 
	    													 typ : String,
	    													 exported : Boolean,
	    													 protectPermission : Option[String],
	    													 intentFilters : ISet[IntentFilter],
	    													 iccInfos : ISet[IccInfo],
	    													 taintResult : TaintAnalysisResult){
    override def toString : String = {
      val compData = template.getInstanceOf("ComponentData")
      compData.add("compName", name)
      compData.add("typ", typ)
      compData.add("exported", exported)
      compData.add("protectPermission", protectPermission.getOrElse(null))
      compData.add("intentFilters", getIntentFilterStrings(intentFilters))
      val iccInfoStrings = new ArrayList[String]
      iccInfos.foreach(iccinfo => iccInfoStrings.add(iccinfo.toString))
      compData.add("iccInfos", iccInfoStrings)
      val taintResultT = template.getInstanceOf("TaintResult")
      val sourceStrings : ArrayList[String] = new ArrayList[String]
      if(taintResult != null){
	      taintResult.getSourceNodes.foreach{
	        sn =>
	          val ssInfo = template.getInstanceOf("SourceSinkInfo")
	          val descriptorStrings : ArrayList[String] = new ArrayList[String]
	          sn.getDescriptors.foreach(f=>descriptorStrings.add(f.toString()))
	          ssInfo.add("descriptors", descriptorStrings)
	          sourceStrings.add(ssInfo.render())
	      }
	      val sinkStrings : ArrayList[String] = new ArrayList[String]
	      taintResult.getSinkNodes.foreach{
	        sn =>
	          val ssInfo = template.getInstanceOf("SourceSinkInfo")
	          val descriptorStrings : ArrayList[String] = new ArrayList[String]
	          sn.getDescriptors.foreach(f=>descriptorStrings.add(f.toString()))
	          ssInfo.add("descriptors", descriptorStrings)
	          sinkStrings.add(ssInfo.render())
	      }
	      taintResultT.add("sources", sourceStrings)
	      taintResultT.add("sinks", sinkStrings)
	      val pathStrings : ArrayList[String] = new ArrayList[String]
	      val taintPaths = taintResult.getTaintedPaths
	      taintPaths.foreach{
	        taintPath =>
	          val path = template.getInstanceOf("TaintPath")
	          val sourcessInfo = template.getInstanceOf("SourceSinkInfo")
	          val sourceDescriptorStrings : ArrayList[String] = new ArrayList[String]
	          taintPath.getSource.getDescriptors.foreach(f=>sourceDescriptorStrings.add(f.toString()))
	          sourcessInfo.add("descriptors", sourceDescriptorStrings)
	          path.add("source", sourcessInfo)
	          val sinkssInfo = template.getInstanceOf("SourceSinkInfo")
	          val sinkDescriptorStrings : ArrayList[String] = new ArrayList[String]
	          taintPath.getSink.getDescriptors.foreach(f=>sinkDescriptorStrings.add(f.toString()))
	          sinkssInfo.add("descriptors", sinkDescriptorStrings)
	          path.add("sink", sinkssInfo)
	          val typStrings : ArrayList[String] = new ArrayList[String]
	          taintPath.getTypes.foreach(f=>typStrings.add(f))
	          path.add("typs", typStrings)
	          val pathString : ArrayList[String] = new ArrayList[String]
	          taintPath.getPath.foreach(f=>pathString.add(f.source + " -> " + f.target))
	          path.add("path", pathString)
	          pathStrings.add(path.render())
	      }
	      taintResultT.add("paths", pathStrings)
	      compData.add("taintResult", taintResultT)
      }
      compData.render()
    }
  }
  
	def collect = {
	  val appInfo = AppCenter.getAppInfo
	  val appName = appInfo.getAppName
	  val uses_permissions = appInfo.getUsesPermissions
	  val compInfos = appInfo.getComponentInfos
	  val intentFDB = AppCenter.getIntentFilterDB
	  val compDatas = compInfos.map{
	    comp =>
	      val compName = comp.name
	      val compRec = Center.getRecord(compName)
	      val typ = comp.typ
	      val exported = comp.exported
	      val protectPermission = comp.permission
	      val intentFilters = intentFDB.getIntentFilters(compName)
	      var iccInfos = isetEmpty[IccInfo]
	      var taintResult : TaintAnalysisResult = null
	      if(!compRec.isPhantom){
		      val (icfg, irfaResult) = AppCenter.getInterproceduralReachingFactsAnalysisResult(compRec)
		      val iccNodes = icfg.nodes.filter{
		        	node =>
		        	  node.isInstanceOf[CGCallNode] && node.asInstanceOf[CGCallNode].getCalleeSet.exists(InterComponentCommunicationModel.isIccOperation(_))
		      	}.map(_.asInstanceOf[CGCallNode])
		      iccInfos =
			      iccNodes.map{
			        iccNode =>
			          val s = irfaResult.entrySet(iccNode)
					      val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
					      val args = iccNode.getOwner.getProcedureBody.location(iccNode.getLocIndex).asInstanceOf[JumpLocation].jump.asInstanceOf[CallJump].callExp.arg match{
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
							  val intentValues = factMap.getOrElse(intentSlot, isetEmpty)
							  val intentcontents = IntentHelper.getIntentContents(factMap, intentValues, iccNode.getContext)
							  val comMap = IntentHelper.mappingIntents(intentcontents)
							  val intentDatas = intentcontents.map(ic=>IntentData(ic.componentNames, ic.actions, ic.categories, ic.datas, ic.types, ic.preciseExplicit, ic.preciseImplicit, comMap(ic).map(c=>(c._1.getName, c._2.toString()))))
							  IccInfo(iccNode.getCalleeSet.map(_.getSignature), iccNode.getContext, intentDatas)
			      }.toSet
		      taintResult = AppCenter.getTaintAnalysisResult(compRec)
	      }
	      ComponentData(compName, typ, exported, protectPermission, intentFilters, iccInfos, taintResult)
	  }
	  val drcompDatas = AppCenter.getDynamicRegisteredComponents.map{
	      case (comp, precise) =>
	        DynamicRegisteredComponentData(comp.getName, "receiver", None, intentFDB.getIntentFilters(comp), precise)
	    }.toSet
	  AppData(appName, uses_permissions, compDatas, drcompDatas)
	}
}