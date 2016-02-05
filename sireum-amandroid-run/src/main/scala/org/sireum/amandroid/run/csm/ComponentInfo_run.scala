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
import org.sireum.amandroid.parser.IntentFilter
import org.sireum.jawa.alir.dataFlowAnalysis.InterProceduralDataFlowGraph
import org.sireum.jawa.util.JVMUtil
import java.io._
import org.sireum.jawa.alir.pta.suspark.InterproceduralSuperSpark
import org.sireum.jawa.Signature

/**
 * @author fgwei
 */
object ComponentInfo_run {
  object ComponentInfoCounter {
    var total = 0
    var haveresult = 0
    val apks: MList[StaApk] = mlistEmpty
    
    def fdComp: IMap[Int, Int] = apks.groupBy(_.comps.size).map{case (s, as) => (s, as.size)}
    def fdHandleImplicit: IMap[Int, Int] = {
      apks.groupBy {
        apk => apk.comps.count { comp => comp.handleImplicit }
      }.map{case (s, as) => (s, as.size)}
    }
    def fdHandleExplicitOnly: IMap[Int, Int] = {
      apks.groupBy {
        apk => apk.comps.count { comp => !comp.handleImplicit }
      }.map{case (s, as) => (s, as.size)}
    }
    def fdNumberOfEdgesForIntraAppGraph: IMap[Int, Int] = {
      apks.groupBy {
        apk => apk.comps.map(comp => comp.edges + comp.outgoingIntent).sum / 100 * 100
      }.map{case (s, as) => (s, as.size)}
    }
    def fdNumberOfIncomingIntent: IMap[Int, Int] = {
      apks.map(_.comps).reduce(_ ++ _).groupBy(c => c.incomingIntent).map{case (s, as) => (s, as.size)}
    }
    def fdNumberOfOutgoingIntent: IMap[Int, Int] = {
      apks.map(_.comps).reduce(_ ++ _).groupBy(_.outgoingIntent).map{case (s, as) => (s, as.size)}
    }
    def fdMedianIncomingExplicit: IMap[Int, Int] = {
      apks.groupBy{
        apk =>
          median(apk.comps.map(_.incomingExplicit).toSeq)
      }.map{case (s, as) => (s, as.size)}
    }
    def fdMedianOutgoingExplicit: IMap[Int, Int] = {
      apks.groupBy{
        apk =>
          median(apk.comps.map(_.outgoingExplicit).toSeq)
      }.map{case (s, as) => (s, as.size)}
    }
    
    def median(s: Seq[Int]) = {
      if(s.isEmpty) 0
      else {
        val (lower, upper) = s.sortWith(_<_).splitAt(s.size / 2)
        if (s.size % 2 == 0) (lower.last + upper.head) / 2 else upper.head
      }
    }
    
    override def toString: String = "total: " + total + ", haveResult: " + haveresult
    
    def generateCsvs(outputUri: FileResourceUri) = {
      writeFdComp(outputUri)
      writeFdHandleImplicit(outputUri)
      writeFdHandleExplicitOnly(outputUri)
      writeFdNumberOfEdgesForIntraAppGraph(outputUri)
      writeFdImplicitIntentEdges(outputUri)
      writeFdNumberOfIncomingIntent(outputUri)
      writeFdNumberOfOutgoingIntent(outputUri)
      writeFdNumberOfIncomingExplicit(outputUri)
      writeFdNumberOfOutgoingExplicit(outputUri)
      writeFdNumberOfIncomingImplicit(outputUri)
      writeFdNumberOfOutgoingImplicit(outputUri)
    }
    
    private def generateCsvHead(header: IList[Int]): String = {
      "apk_name," + header.mkString(",")
    }
    private def generateCsvRow(header: IList[Int], apk: String, input: IList[Int]): String = {
      val s = new StringBuilder
      s.append(apk)
      header.foreach {
        h =>
          val v = input.count {_ == h}
          if(v != 0) s.append("," + v)
          else s.append(",")
      }
      s.toString()
    }
    def writeFdComp(outputUri: FileResourceUri) = {
      val outFile = FileUtil.toFile(outputUri + "/" + "Comp.csv")
      val outstream = new PrintWriter(outFile)
      val h = apks.groupBy(_.comps.size).map(_._1).toList.sortBy(x => x)
      outstream.append(generateCsvHead(h) + "\n")
      apks.sortBy(_.apk).foreach {
        apk =>
          outstream.append(generateCsvRow(h, apk.apk, List(apk.comps.size)) + "\n")
      }
      outstream.flush()
      outstream.close()
    }
    def writeFdHandleImplicit(outputUri: FileResourceUri) = {
      val outFile = FileUtil.toFile(outputUri + "/" + "HandleImplicit.csv")
      val outstream = new PrintWriter(outFile)
      val h = apks.groupBy {
        apk => apk.comps.count { comp => comp.handleImplicit }
      }.map(_._1).toList.sortBy(x => x)
      outstream.append(generateCsvHead(h) + "\n")
      apks.sortBy(_.apk).foreach {
        apk =>
          outstream.append(generateCsvRow(h, apk.apk, List(apk.comps.count { comp => comp.handleImplicit })) + "\n")
      }
      outstream.flush()
      outstream.close()
    }
    def writeFdHandleExplicitOnly(outputUri: FileResourceUri) = {
      val outFile = FileUtil.toFile(outputUri + "/" + "HandleExplicitOnly.csv")
      val outstream = new PrintWriter(outFile)
      val h = apks.groupBy {
        apk => apk.comps.count { comp => !comp.handleImplicit }
      }.map(_._1).toList.sortBy(x => x)
      outstream.append(generateCsvHead(h) + "\n")
      apks.sortBy(_.apk).foreach {
        apk =>
          outstream.append(generateCsvRow(h, apk.apk, List(apk.comps.count { comp => !comp.handleImplicit })) + "\n")
      }
      outstream.flush()
      outstream.close()
    }
    def writeFdNumberOfEdgesForIntraAppGraph(outputUri: FileResourceUri) = {
      val outFile = FileUtil.toFile(outputUri + "/" + "NumberOfEdgesForIntraAppGraph.csv")
      val outstream = new PrintWriter(outFile)
      val h = apks.groupBy {
        apk => apk.comps.map(comp => comp.edges + comp.outgoingIntent).sum / 100 * 100
      }.map(_._1).toList.sortBy(x => x)
      outstream.append(generateCsvHead(h) + "\n")
      apks.sortBy(_.apk).foreach {
        apk =>
          outstream.append(generateCsvRow(h, apk.apk, List(apk.comps.map(comp => comp.edges + comp.outgoingIntent).sum / 100 * 100)) + "\n")
      }
      outstream.flush()
      outstream.close()
    }
    def writeFdImplicitIntentEdges(outputUri: FileResourceUri) = {
      val outFile = FileUtil.toFile(outputUri + "/" + "ImplicitIntentEdges.csv")
      val outstream = new PrintWriter(outFile)
      val h = apks.groupBy(_.comps.map(comp => comp.outgoingImplicit).sum).map(_._1).toList.sortBy(x => x)
      outstream.append(generateCsvHead(h) + "\n")
      apks.sortBy(_.apk).foreach {
        apk =>
          outstream.append(generateCsvRow(h, apk.apk, List(apk.comps.map(comp => comp.outgoingImplicit).sum)) + "\n")
      }
      outstream.flush()
      outstream.close()
    }
    def writeFdNumberOfIncomingIntent(outputUri: FileResourceUri) = {
      val outFile = FileUtil.toFile(outputUri + "/" + "NumberOfIncomingIntent.csv")
      val outstream = new PrintWriter(outFile)
      val h = apks.map(_.comps).reduce(_ ++ _).groupBy(c => c.incomingIntent).map(_._1).toList.sortBy(x => x)
      outstream.append(generateCsvHead(h) + "\n")
      apks.sortBy(_.apk).foreach {
        apk =>
          outstream.append(generateCsvRow(h, apk.apk, apk.comps.map(_.incomingIntent)) + "\n")
      }
      outstream.flush()
      outstream.close()
    }
    def writeFdNumberOfOutgoingIntent(outputUri: FileResourceUri) = {
      val outFile = FileUtil.toFile(outputUri + "/" + "NumberOfOutgoingIntent.csv")
      val outstream = new PrintWriter(outFile)
      val h = apks.map(_.comps).reduce(_ ++ _).groupBy(c => c.outgoingIntent).map(_._1).toList.sortBy(x => x)
      outstream.append(generateCsvHead(h) + "\n")
      apks.sortBy(_.apk).foreach {
        apk =>
          outstream.append(generateCsvRow(h, apk.apk, apk.comps.map(_.outgoingIntent)) + "\n")
      }
      outstream.flush()
      outstream.close()
    }
    def writeFdNumberOfIncomingExplicit(outputUri: FileResourceUri) = {
      val outFile = FileUtil.toFile(outputUri + "/" + "NumberOfIncomingExplicit.csv")
      val outstream = new PrintWriter(outFile)
      val h = apks.map(_.comps).reduce(_ ++ _).groupBy(c => c.incomingExplicit).map(_._1).toList.sortBy(x => x)
      outstream.append(generateCsvHead(h) + "\n")
      apks.sortBy(_.apk).foreach {
        apk =>
          outstream.append(generateCsvRow(h, apk.apk, apk.comps.map(_.incomingIntent)) + "\n")
      }
      outstream.flush()
      outstream.close()
    }
    def writeFdNumberOfOutgoingExplicit(outputUri: FileResourceUri) = {
      val outFile = FileUtil.toFile(outputUri + "/" + "NumberOfOutgoingExplicit.csv")
      val outstream = new PrintWriter(outFile)
      val h = apks.map(_.comps).reduce(_ ++ _).groupBy(c => c.outgoingExplicit).map(_._1).toList.sortBy(x => x)
      outstream.append(generateCsvHead(h) + "\n")
      apks.sortBy(_.apk).foreach {
        apk =>
          outstream.append(generateCsvRow(h, apk.apk, apk.comps.map(_.outgoingExplicit)) + "\n")
      }
      outstream.flush()
      outstream.close()
    }
    def writeFdNumberOfIncomingImplicit(outputUri: FileResourceUri) = {
      val outFile = FileUtil.toFile(outputUri + "/" + "NumberOfIncomingImplicit.csv")
      val outstream = new PrintWriter(outFile)
      val h = apks.map(_.comps).reduce(_ ++ _).groupBy(c => c.incomingImplicit).map(_._1).toList.sortBy(x => x)
      outstream.append(generateCsvHead(h) + "\n")
      apks.sortBy(_.apk).foreach {
        apk =>
          outstream.append(generateCsvRow(h, apk.apk, apk.comps.map(_.incomingImplicit)) + "\n")
      }
      outstream.flush()
      outstream.close()
    }
    def writeFdNumberOfOutgoingImplicit(outputUri: FileResourceUri) = {
      val outFile = FileUtil.toFile(outputUri + "/" + "NumberOfOutgoingImplicit.csv")
      val outstream = new PrintWriter(outFile)
      val h = apks.map(_.comps).reduce(_ ++ _).groupBy(c => c.outgoingImplicit).map(_._1).toList.sortBy(x => x)
      outstream.append(generateCsvHead(h) + "\n")
      apks.sortBy(_.apk).foreach {
        apk =>
          outstream.append(generateCsvRow(h, apk.apk, apk.comps.map(_.outgoingImplicit)) + "\n")
      }
      outstream.flush()
      outstream.close()
    }
  }
  
  case class StaApk(apk: String) {
    val components: MMap[JawaType, StaComponent] = mmapEmpty
    def comps = components.map(_._2).toList
  }
  
  case class StaComponent(apk: String, comp: JawaType) {
    var handleImplicit: Boolean = false
    var edges: Int = 0
    var incomingExplicit: Int = 0
    var incomingImplicit: Int = 0
    var outgoingExplicit: Int = 0
    var outgoingImplicit: Int = 0
    def incomingIntent: Int = incomingExplicit + incomingImplicit
    def outgoingIntent: Int = outgoingExplicit + outgoingImplicit
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
//    .filter(_.contains("wechatphonebook.apk"))
    files foreach {
      file =>
        if(Apk.isValidApk(file)) {
          ComponentInfoCounter.total += 1
          println("#####" + file + "#####")
          JVMUtil.showMemoryUsage
          val staapk = StaApk(file)
          ComponentInfoCounter.apks += staapk
          
          try {
            val reporter = new PrintReporter(MsgLevel.ERROR)
            val global = new Global(file, reporter)
            global.setJavaLib(AndroidGlobalConfig.lib_files)
            val timer = new PerComponentTimer(600)
            timer.start
            val app_info = new LightweightCSTBuilder(global, Some(timer))
            val yard = new ApkYard(global)
            val apk = yard.loadApk(file, outputUri, dpsuri, app_info, false, false, true)
            app_info.getComponentInfos.map{
              ci => 
                val stacomp = StaComponent(file, ci.compType)
                stacomp.handleImplicit = !app_info.getIntentDB.getIntentFilters(ci.compType).isEmpty
                val comp = global.getClassOrResolve(ci.compType)
                stacomp.edges = apk.getIDFG(comp).map(_.icfg.edges.size).getOrElse(0)
                staapk.components(ci.compType) = stacomp
            }
            val st = app_info.getSummaryTables
            val intentChannels = st.map(_._2.get[Intent_Summary](CHANNELS.INTENT_CHANNEL))
            val allIntentCallees = intentChannels.map(_.asCallee).reduceOption{_ ++ _}.getOrElse(imapEmpty)
            val rpcChannels = st.map(_._2.get[RPC_Summary](CHANNELS.RPC_CHANNEL))
            val allRPCCallees = rpcChannels.map(_.asCallee).reduceOption{_ ++ _}.getOrElse(imapEmpty)
            st.foreach {
              case (comp, table) =>
                val stacomp = staapk.components(comp.getType)
                yard.addSummaryTable(comp, table)
                val isummary = table.get[Intent_Summary](CHANNELS.INTENT_CHANNEL)
                stacomp.outgoingExplicit += isummary.asCaller.filter(!_._2.asInstanceOf[IntentCaller].intent.isImplicit).size
                stacomp.outgoingImplicit += isummary.asCaller.filter(_._2.asInstanceOf[IntentCaller].intent.isImplicit).size
                isummary.asCaller foreach {
                  case (callernode, caller) =>
                    val intent_caller = caller.asInstanceOf[IntentCaller]
                    val icc_callees = allIntentCallees.filter(_._2.matchWith(intent_caller))
                    icc_callees foreach {
                      case (calleenode, callee) =>
                        val intent_callee = callee.asInstanceOf[IntentCallee]
                        val calleeStaComp = staapk.components(intent_callee.component.getType)
                        if(!intent_caller.intent.isImplicit) calleeStaComp.incomingExplicit +=  1
                        else calleeStaComp.incomingImplicit += 1
                        val precise = intent_caller.intent.preciseExplicit && intent_caller.intent.preciseImplicit
                        println({if(precise) "precise:" else "imprecise:"} + " " + comp + " --icc--> " + intent_callee.component.getName)
                    }
                }
            }
            ComponentInfoCounter.haveresult += 1
            ComponentInfoCounter.generateCsvs(outputUri)
          } catch {
            case e: Exception =>
              e.printStackTrace()
          } finally {
            println(ComponentInfoCounter.toString())
            System.gc()
            System.gc()
          }
        }
    }
  }
}