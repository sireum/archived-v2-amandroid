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
package org.sireum.amandroid.cli

import org.sireum.option.SireumAmandroidGenGraphMode
import java.io.File
import org.sireum.util._
import org.sireum.amandroid.cli.util.CliLogger
import org.sireum.jawa.util.IgnoreException
import java.net.URI
import org.sireum.jawa.util.APKFileResolver
import org.sireum.amandroid.decompile.Dex2PilarConverter
import org.sireum.amandroid.util.AndroidLibraryAPISummary
import org.sireum.amandroid.appInfo.AppInfoCollector
import org.sireum.jawa.alir.controlFlowGraph.InterproceduralControlFlowGraph
import org.sireum.jawa.alir.controlFlowGraph.ICFGNode
import org.sireum.amandroid.AndroidConstants
import org.sireum.jawa.JawaMethod
import java.io.FileOutputStream
import java.util.zip.GZIPOutputStream
import java.io.BufferedOutputStream
import org.sireum.jawa.xml.AndroidXStream
import org.sireum.amandroid.AndroidGlobalConfig
import org.sireum.amandroid.decompile.AmDecoder
import org.sireum.jawa.alir.pta.suspark.InterproceduralSuperSpark
import org.sireum.option.GraphFormat
import org.sireum.option.GraphType
import java.io.BufferedWriter
import java.io.OutputStreamWriter
import org.jgrapht.ext.VertexNameProvider
import org.jgrapht.ext.EdgeNameProvider
import org.sireum.jawa.alir.controlFlowGraph.ICFGEntryNode
import org.sireum.jawa.alir.controlFlowGraph.ICFGReturnNode
import org.sireum.jawa.alir.controlFlowGraph.ICFGExitNode
import org.sireum.jawa.alir.controlFlowGraph.ICFGCallNode
import org.sireum.jawa.alir.controlFlowGraph.ICFGNormalNode
import org.sireum.jawa.alir.Context
import org.sireum.jawa.FileReporter
import org.sireum.jawa.Global
import org.sireum.amandroid.decompile.ApkDecompiler
import org.sireum.jawa.MsgLevel
import org.sireum.amandroid.Apk
import org.sireum.amandroid.util.ApkFileUtil
import org.sireum.jawa.Constants
import org.sireum.jawa.NoReporter
import org.sireum.jawa.util.FutureUtil
import org.sireum.jawa.alir.dataFlowAnalysis.InterProceduralDataFlowGraph
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.{global => ec}
import scala.concurrent.Await
import java.util.concurrent.TimeoutException

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object GenGraphCli {
  def run(saamode: SireumAmandroidGenGraphMode) {
    val sourceDir = saamode.srcFile
    val sourceFile = new File(sourceDir)
    val outputDir = saamode.analysis.outdir
    val timeout = saamode.analysis.timeout
    val mem = saamode.general.mem
    val debug = saamode.general.debug
    val format = saamode.format match {
      case GraphFormat.GML => "GML"
      case GraphFormat.GraphML => "GraphML"
    }
    val graphtyp = saamode.graphtyp match {
      case GraphType.API => "API"
      case GraphType.DETAILED_CALL => "DETAILED_CALL"
      case GraphType.FULL => "FULL"
      case GraphType.SIMPLE_CALL => "SIMPLE_CALL"
    }
    val header = saamode.header
    forkProcess(timeout, sourceDir, outputDir, mem, format, graphtyp, debug, header)
  }

def forkProcess(timeout: Int, sourceDir: String, outputDir: String, mem: Int, format: String, graphtyp: String, debug: Boolean, header: String) = {
    val args: MList[String] = mlistEmpty
    args += timeout.toString
    args += format
    args += graphtyp
    args += debug.toString()
    args += header
    args ++= List(sourceDir, outputDir)
    org.sireum.jawa.util.JVMUtil.startSecondJVM(GenGraph.getClass(), "-Xmx" + mem + "G", args.toList, true)
  }
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object GenGraph {
  
  private final val TITLE = "GenCallGraph"
  
  def main(args: Array[String]) {
    if(args.size != 7){
      println("Usage: <timeout minutes> <[Graph Format: DOT, GraphML, GML> <[Graph Type: FULL, SIMPLE_CALL, DETAILED_CALL, API> <debug> <header> <source path> <output path>")
      return
    }
    val timeout = args(0).toInt
    val format = args(1)
    val graphtyp = args(2)
    val debug = args(3).toBoolean
    val header = args(4)
    val sourcePath = args(5)
    val outputPath = args(6)
    val dpsuri = AndroidGlobalConfig.dependence_dir.map(FileUtil.toUri(_))
    val liblist = AndroidGlobalConfig.lib_files
    val static = AndroidGlobalConfig.static_init
    val parallel = AndroidGlobalConfig.parallel
    val k_context = AndroidGlobalConfig.k_context
    
    val apkFileUris: MSet[FileResourceUri] = msetEmpty
    val fileOrDir = new File(sourcePath)
    fileOrDir match {
      case dir if dir.isDirectory() =>
        apkFileUris ++= ApkFileUtil.getApks(FileUtil.toUri(dir), true)
      case file =>
        if(Apk.isValidApk(FileUtil.toUri(file)))
          apkFileUris += FileUtil.toUri(file)
        else println(file + " is not decompilable.")
    }
    genGraph(apkFileUris.toSet, outputPath, dpsuri, liblist, static, parallel, k_context, timeout, header, format, graphtyp, debug)
  }
  
  def genGraph(
      apkFileUris: Set[FileResourceUri], 
      outputPath: String, 
      dpsuri: Option[FileResourceUri], 
      liblist: String, 
      static: Boolean, 
      parallel: Boolean, 
      k_context: Int, 
      timeout: Int, 
      header: String, 
      format: String, 
      graphtyp: String, 
      debug: Boolean) = {
    Context.init_context_length(k_context)
    println("Total apks: " + apkFileUris.size)
    try{
      var i: Int = 0
      apkFileUris.foreach{
        apkFileUri =>
          try{
            i+=1
            println("Analyzing #" + i + ":" + apkFileUri)
            val outputUri = FileUtil.toUri(outputPath)
            val reporter = 
              if(debug) new FileReporter(getOutputDirUri(outputUri, apkFileUri), MsgLevel.INFO)
              else new NoReporter
            val global = new Global(apkFileUri, reporter)
            global.setJavaLib(liblist)
            
            val timer = timeout minutes
            val (f, cancel) = FutureUtil.interruptableFuture[(InterproceduralControlFlowGraph[ICFGNode], FileResourceUri)] { () =>
              val (outUri, srcs, _) = ApkDecompiler.decompile(FileUtil.toFile(apkFileUri), FileUtil.toFile(outputUri), dpsuri, false, false, true, true)
              srcs foreach {
                src =>
                  val fileUri = FileUtil.toUri(FileUtil.toFilePath(outUri) + File.separator + src)
                  if(FileUtil.toFile(fileUri).exists()) {
                    //store the app's pilar code in AmandroidCodeSource which is organized class by class.
                    global.load(fileUri, Constants.PILAR_FILE_EXT, AndroidLibraryAPISummary)
                  }
              }
              val apk = new Apk(apkFileUri)
              AppInfoCollector.collectInfo(apk, global, outUri)
              val eps = apk.getEntryPoints
              val pros =
                eps.map{
                  compName =>
                    val comp = global.resolveToBody(compName)
                    val procedures = comp.getDeclaredMethodsByName(AndroidConstants.MAINCOMP_ENV) ++ comp.getDeclaredMethodsByName(AndroidConstants.COMP_ENV)
                    procedures
                }.reduce(iunion[JawaMethod])
              (InterproceduralSuperSpark(global, pros.map(_.getSignature)).icfg, outUri)
            }
            try {
              val (icfg, outUri) = Await.result(f, timer)
              val apkName = apkFileUri.substring(apkFileUri.lastIndexOf("/") + 1, apkFileUri.lastIndexOf("."))
              graphtyp match{
                case "FULL" => 
                  val graph = icfg
                  val ext = format match {
                    case "GraphML" => ".graphml"
                    case "GML" => ".gml"
                  }
                  val file = FileUtil.toFile(outUri + "/" + apkName.filter(_.isUnicodeIdentifierPart) + ext)
                  val w = new FileOutputStream(file)
                  val zips = new BufferedOutputStream(w)
                  val zipw = new BufferedWriter(new OutputStreamWriter(zips, "UTF-8"))
                  try {
                    format match {
                      case "GraphML" => graph.toGraphML(zipw)
                      case "GML" => graph.toGML(zipw)
                    }
                  } catch {case e: Exception => }
                  finally {
                    zipw.close()
                  }
                case "SIMPLE_CALL" => 
                  val path = new File(outputPath + "/" + apkName.filter(_.isUnicodeIdentifierPart) + "/simple_cg")
                  val fm = format match {
                    case "GraphML" => "GraphML"
                    case "GML" => "GML"
                  }
                  icfg.getCallGraph.toSimpleCallGraph(header, path.getPath, fm)
                case "DETAILED_CALL" => 
                  val path = new File(outputPath + "/" + apkName.filter(_.isUnicodeIdentifierPart) + "/detailed_cg")
                  val fm = format match {
                    case "GraphML" => "GraphML"
                    case "GML" => "GML"
                  }
                  icfg.getCallGraph.toDetailedCallGraph(header, icfg, path.getPath, fm)
                case "API" => 
                  val graph = icfg.toApiGraph(global)
                  val ext = format match {
                    case "GraphML" => ".graphml"
                    case "GML" => ".gml"
                  }
                  val file = FileUtil.toFile(outUri + "/" + apkName.filter(_.isUnicodeIdentifierPart) + ext)
                  val w = new FileOutputStream(file)
                  val zips = new BufferedOutputStream(w)
                  val zipw = new BufferedWriter(new OutputStreamWriter(zips, "UTF-8"))
                  try {
                    format match {
                      case "GraphML" => graph.toGraphML(zipw)
                      case "GML" => graph.toGML(zipw)
                    }
                  } catch {case e: Exception =>}
                  finally {
                    zipw.close()
                  }
              }
              println(apkName + " result stored!")
              if(debug) println("Debug info write into " + reporter.asInstanceOf[FileReporter].f)
              println("Done!")
            } catch {
              case te: TimeoutException => 
                cancel()
                println(te.getMessage)
            }
         } catch {
           case e: Throwable => 
             CliLogger.logError(new File(outputPath), "Error: " , e)
         } finally {
           // before starting the analysis of the current app, first clear the previous app's records' code from the AmandroidCodeSource
           System.gc()
           System.gc()
         }
      }
    } catch {
      case e: Throwable => 
        CliLogger.logError(new File(outputPath), "Error: " , e)

    }
  }
  private def getOutputDirUri(outputUri: FileResourceUri, apkUri: FileResourceUri): FileResourceUri = {
    outputUri + {if(!outputUri.endsWith("/")) "/" else ""} + apkUri.substring(apkUri.lastIndexOf("/") + 1, apkUri.lastIndexOf("."))
  }
}
