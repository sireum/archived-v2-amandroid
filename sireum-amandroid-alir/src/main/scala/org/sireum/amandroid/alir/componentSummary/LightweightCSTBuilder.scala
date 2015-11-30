package org.sireum.amandroid.alir.componentSummary

import org.sireum.amandroid.appInfo.AppInfoCollector
import org.sireum.jawa.Reporter
import org.sireum.amandroid.Apk
import org.sireum.jawa.Global
import org.sireum.util._
import org.sireum.jawa.util.MyTimer
import java.io.File
import org.sireum.jawa.JawaClass
import org.sireum.amandroid.appInfo.ReachableInfoCollector
import org.sireum.jawa.alir.pta.suspark.InterproceduralSuperSpark
import org.sireum.jawa.alir.dataFlowAnalysis.InterProceduralDataFlowGraph
import org.sireum.amandroid.alir.componentSummary.ComponentSummaryTable.CHANNELS
import org.sireum.jawa.alir.controlFlowGraph.ICFGEntryNode
import org.sireum.jawa.alir.controlFlowGraph.ICFGCallNode
import org.sireum.amandroid.AndroidConstants
import org.sireum.jawa.alir.pta.VarSlot
import org.sireum.jawa.alir.pta.Instance
import org.sireum.jawa.alir.Context
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.IntentHelper.IntentContent
import org.sireum.amandroid.parser.UriData

/**
 * Hopefully this will be really light weight to build the component based summary table
 * @author fgwei
 */
class LightweightCSTBuilder(global: Global, apk: Apk, yard: ApkYard, outputUri: FileResourceUri, timer: Option[MyTimer]) extends AppInfoCollector(global, apk, outputUri, timer) {
  
  private val intentContents: MMap[JawaClass, MMap[Instance, IntentContent]] = mmapEmpty
  
  override def collectInfo: Unit = {
    val manifestUri = outputUri + File.separator + "AndroidManifest.xml"
    val mfp = AppInfoCollector.analyzeManifest(global.reporter, manifestUri)
    this.intentFdb.merge(mfp.getIntentDB)
    val components = msetEmpty[JawaClass]
    mfp.getComponentInfos.foreach {
      f =>
        if(f.enabled){
          val comp = global.getClassOrResolve(f.compType)
          if(!comp.isUnknown && comp.isApplicationClass){
            components += comp
          }
        }
    }
    components.foreach {
      comp =>
        val rpcs = getRpcMethods(comp)
        apk.addRpcMethods(comp, rpcs)
    }
    apk.setComponents(components.toSet)
    apk.updateIntentFilterDB(this.intentFdb)
    apk.setAppInfo(this)
    buildCST(components.toSet)
  }
  
  private def buildCST(comps: ISet[JawaClass]) = {
    comps foreach {
      comp =>
        val methods = comp.getDeclaredMethods
        val idfg = InterproceduralSuperSpark(global, methods, timer)
        buildCSTFromIDFG(comp, idfg)
    }
  }
  
  private def collectIntentContent(component: JawaClass, idfg: InterProceduralDataFlowGraph) = {
    
  }
  
  private def buildCSTFromIDFG(component: JawaClass, idfg: InterProceduralDataFlowGraph) = {
    val csp = new ComponentSummaryProvider {
      def getIntentCaller(idfg: InterProceduralDataFlowGraph, intentValue: ISet[Instance], context: Context): ISet[IntentContent] = {
        val result = msetEmpty[IntentContent]
        val icfg = idfg.icfg
        val ptaresult = idfg.ptaresult
        intentValue.foreach {
          intentIns =>
            var preciseExplicit = true
            var preciseImplicit = true
            val componentNames = msetEmpty[String]
            val actions = msetEmpty[String]
            val categories = msetEmpty[String]
            val datas: MSet[UriData] = msetEmpty
            val types: MSet[String] = msetEmpty
            icfg.nodes.foreach {
              node =>
                node match {
                  case cn: ICFGCallNode =>
                    val callees = cn.getCalleeSet
                    val args = cn.argNames
                    val currentContext = cn.context
                    callees foreach {
                      callee =>
                        callee.callee.getSignature.signature match {
                          case "Landroid/content/Intent;.<init>:(Landroid/content/Context;Ljava/lang/Class;)V" =>  //public constructor
                            
                          case "Landroid/content/Intent;.<init>:(Landroid/content/Intent;)V" =>  //public constructor
                          case "Landroid/content/Intent;.<init>:(Landroid/content/Intent;Z)V" =>  //private constructor
                          case "Landroid/content/Intent;.<init>:(Ljava/lang/String;)V" =>  //public constructor
                            val thisSlot = VarSlot(args(0), false, true)
                            val thisValue = ptaresult.pointsToSet(thisSlot, currentContext)
                            val actionSlot = VarSlot(args(1), false, true)
                            val actionValue = ptaresult.pointsToSet(actionSlot, currentContext)
                          case "Landroid/content/Intent;.<init>:(Ljava/lang/String;Landroid/net/Uri;)V" =>  //public constructor
                          case "Landroid/content/Intent;.<init>:(Ljava/lang/String;Landroid/net/Uri;Landroid/content/Context;Ljava/lang/Class;)V" =>  //public constructor
                          case "Landroid/content/Intent;.addCategory:(Ljava/lang/String;)Landroid/content/Intent;" =>  //public
                          case "Landroid/content/Intent;.addFlags:(I)Landroid/content/Intent;" =>  //public
                          case "Landroid/content/Intent;.setAction:(Ljava/lang/String;)Landroid/content/Intent;" =>  //public
                          case "Landroid/content/Intent;.setClass:(Landroid/content/Context;Ljava/lang/Class;)Landroid/content/Intent;" =>  //public
                          case "Landroid/content/Intent;.setClassName:(Landroid/content/Context;Ljava/lang/String;)Landroid/content/Intent;" =>  //public
                          case "Landroid/content/Intent;.setClassName:(Ljava/lang/String;Ljava/lang/String;)Landroid/content/Intent;" =>  //public
                          case "Landroid/content/Intent;.setComponent:(Landroid/content/ComponentName;)Landroid/content/Intent;" =>  //public
                          case "Landroid/content/Intent;.setData:(Landroid/net/Uri;)Landroid/content/Intent;" =>  //public
                          case "Landroid/content/Intent;.setDataAndNormalize:(Landroid/net/Uri;)Landroid/content/Intent;" =>  //public
                          case "Landroid/content/Intent;.setDataAndType:(Landroid/net/Uri;Ljava/lang/String;)Landroid/content/Intent;" =>  //public
                          case "Landroid/content/Intent;.setDataAndTypeAndNormalize:(Landroid/net/Uri;Ljava/lang/String;)Landroid/content/Intent;" =>  //public
                          case "Landroid/content/Intent;.setFlags:(I)Landroid/content/Intent;" =>  //public
                          case "Landroid/content/Intent;.setPackage:(Ljava/lang/String;)Landroid/content/Intent;" =>  //public
                          case "Landroid/content/Intent;.setType:(Ljava/lang/String;)Landroid/content/Intent;" =>  //public
                          case "Landroid/content/Intent;.setTypeAndNormalize:(Ljava/lang/String;)Landroid/content/Intent;" =>  //public
                          case _ => 
                        }
                    }
                  case _ =>
                }
            }
            result += IntentContent(componentNames.toSet, actions.toSet, categories.toSet, datas.toSet, types.toSet, 
             preciseExplicit, preciseImplicit, context)
        }
        result.toSet
      }
    }
    val summaryTable = ComponentSummaryTable.buildComponentSummaryTable(apk, component, idfg, csp)
  }
}