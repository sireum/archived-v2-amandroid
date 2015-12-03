package org.sireum.amandroid.run.csm

import org.sireum.util._
import org.sireum.amandroid.Apk
import org.sireum.amandroid.AndroidGlobalConfig
import org.sireum.jawa.PrintReporter
import org.sireum.jawa.MsgLevel
import org.sireum.jawa.Global
import org.sireum.amandroid.alir.componentSummary.ApkYard
import org.sireum.jawa.util.PerComponentTimer
import org.sireum.jawa.util.MyTimer
import org.sireum.amandroid.alir.componentSummary.LightweightCSTBuilder
import org.sireum.amandroid.appInfo.AppInfoCollector
import org.sireum.amandroid.alir.componentSummary.ComponentSummaryTable.CHANNELS
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.IntentHelper.IntentContent
import org.sireum.amandroid.alir.componentSummary.Intent_Summary
import org.sireum.jawa.JawaClass
import org.sireum.amandroid.alir.componentSummary.IntentCallee
import org.sireum.amandroid.alir.componentSummary.IntentCaller
import org.sireum.amandroid.alir.componentSummary.RPC_Summary
import org.sireum.amandroid.alir.componentSummary.RPCCaller
import org.sireum.amandroid.alir.componentSummary.RPCCallee
import org.sireum.jawa.JawaType

/**
 * @author fgwei
 */
object ComponentInfo_run {
  object ComponentInfoCounter {
    var total = 0
    var haveresult = 0
    val compMap: MMap[String, Int] = mmapEmpty
    def avgComp: Float = compMap.map(_._2).sum.toFloat / compMap.size.toFloat
    val compInfoMap: MMap[(String, JawaType), Component] = mmapEmpty
    def avgICC: (Float, Float, Float, Float) = {
      var avg_intent_ascaller: Float = 0
      var avg_intent_ascallee: Float = 0
      var avg_rpc_ascaller: Float = 0
      var avg_rpc_ascallee: Float = 0
      val comps = compInfoMap.map(_._2)
      println(comps.map(_.intent_ascaller).sum)
      avg_intent_ascaller = comps.map(_.intent_ascaller).sum.toFloat / comps.size.toFloat
      avg_intent_ascallee = comps.map(_.intent_ascallee).sum.toFloat / comps.size.toFloat
      avg_rpc_ascaller = comps.map(_.rpc_ascaller).sum.toFloat / comps.size.toFloat
      avg_rpc_ascallee = comps.map(_.rpc_ascallee).sum.toFloat / comps.size.toFloat
      (avg_intent_ascaller, avg_intent_ascallee, avg_rpc_ascaller, avg_rpc_ascallee)
    }
    override def toString: String = "total: " + total + ", haveResult: " + haveresult + ", avgComp: " + avgComp +
     ", avgICC: " + avgICC
  }
  
  case class Component(apk: String, comp: JawaType) {
    var intent_ascaller: Int = 0
    var intent_ascallee: Int = 0
    var rpc_ascaller: Int = 0
    var rpc_ascallee: Int = 0
  }
  
  def main(args: Array[String]): Unit = {
    if(args.size < 2){
      System.err.print("Usage: source_path output_path")
      return
    }
    val sourcePath = args(0)
    val outputPath = args(1)
    val outputUri = FileUtil.toUri(outputPath)
    val dpsuri = AndroidGlobalConfig.dependence_dir.map(FileUtil.toUri(_))
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), "", true).toSet
//    .filter(_.contains("/Volumes/ArgusGroupFG/Stash/Amandroid/Sources/appspopular/MUSIC_AND_AUDIO/com.maxmpz.audioplayer.apk"))
    files foreach {
      file =>
        if(Apk.isValidApk(file)) {
          ComponentInfoCounter.total += 1
          println("#####" + file + "#####")
          try {
            val timer = new PerComponentTimer(300)
            timer.start
            val reporter = new PrintReporter(MsgLevel.ERROR)
            val global = new Global(file, reporter)
            global.setJavaLib(AndroidGlobalConfig.lib_files)
            val app_info = new LightweightCSTBuilder(global, Some(timer))
            val yard = new ApkYard(global)
            val apk = yard.loadApk(file, outputUri, dpsuri, app_info, false, false, true)
            app_info.getComponentInfos.map(ci => ComponentInfoCounter.compInfoMap((file, ci.compType)) = Component(file, ci.compType))
            val st = app_info.getSummaryTables
            val intentChannels = st.map(_._2.get[Intent_Summary](CHANNELS.INTENT_CHANNEL))
            val allIntentCallees = intentChannels.map(_.asCallee).reduceOption{_ ++ _}.getOrElse(imapEmpty)
            val rpcChannels = st.map(_._2.get[RPC_Summary](CHANNELS.RPC_CHANNEL))
            val allRPCCallees = rpcChannels.map(_.asCallee).reduceOption{_ ++ _}.getOrElse(imapEmpty)
            st.foreach {
              case (comp, table) =>
                yard.addSummaryTable(comp, table)
                val isummary = table.get[Intent_Summary](CHANNELS.INTENT_CHANNEL)
                ComponentInfoCounter.compInfoMap(file, comp.getType).intent_ascaller += isummary.asCaller.size
                isummary.asCaller foreach {
                  case (callernode, caller) =>
                    val intent_caller = caller.asInstanceOf[IntentCaller]
                    val icc_callees = allIntentCallees.filter(_._2.matchWith(intent_caller))
                    icc_callees foreach {
                      case (calleenode, callee) =>
                        val intent_callee = callee.asInstanceOf[IntentCallee]
                        ComponentInfoCounter.compInfoMap(file, intent_callee.component.getType).intent_ascallee += 1
                        val precise = intent_caller.intent.preciseExplicit && intent_caller.intent.preciseImplicit
                        println({if(precise) "precise:" else "imprecise:"} + " " + comp + " --icc--> " + intent_callee.component.getName)
                    }
                }
                val rsummary = table.get[RPC_Summary](CHANNELS.RPC_CHANNEL)
                ComponentInfoCounter.compInfoMap(file, comp.getType).rpc_ascaller += rsummary.asCaller.size
                rsummary.asCaller foreach {
                  case (callernode, caller) =>
                    val rpc_caller = caller.asInstanceOf[RPCCaller]
                    val rpc_callees = allRPCCallees.filter(_._2.matchWith(rpc_caller))
                    rpc_callees foreach {
                      case (calleenode, callee) =>
                        val rpc_callee = callee.asInstanceOf[RPCCallee]
                        ComponentInfoCounter.compInfoMap(file, rpc_callee.method.getDeclaringClass.getType).rpc_ascallee += 1
                        println(comp + " --rpc--> " + rpc_callee.method.getDeclaringClass.getName)
                    }
                }
            }
            ComponentInfoCounter.compMap(file) = app_info.getComponentInfos.size
            ComponentInfoCounter.haveresult += 1
          } catch {
            case e: Exception =>
              e.printStackTrace()
          } finally {
            println(ComponentInfoCounter.toString())
          }
        }
    }
  }
}