/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.cli

import org.sireum.option.SireumAmandroidGenGraphMode
import org.sireum.option.AnalyzeSource
import java.io.File
import org.sireum.option.MessageLevel
import org.sireum.util._
import org.sireum.jawa.MessageCenter
import org.sireum.jawa.MessageCenter._
import org.sireum.amandroid.cli.util.CliLogger
import org.sireum.jawa.util.IgnoreException
import java.net.URI
import org.sireum.jawa.util.APKFileResolver
import org.sireum.amandroid.decompile.Dex2PilarConverter
import org.sireum.jawa.JawaCodeSource
import org.sireum.jawa.GlobalConfig
import org.sireum.amandroid.util.AndroidLibraryAPISummary
import org.sireum.amandroid.appInfo.AppInfoCollector
import org.sireum.jawa.alir.controlFlowGraph.InterproceduralControlFlowGraph
import org.sireum.jawa.alir.controlFlowGraph.ICFGNode
import org.sireum.jawa.Center
import org.sireum.amandroid.AndroidConstants
import org.sireum.jawa.JawaProcedure
import java.io.FileOutputStream
import java.util.zip.GZIPOutputStream
import java.io.BufferedOutputStream
import org.sireum.jawa.xml.AndroidXStream
import org.sireum.amandroid.AndroidGlobalConfig
import org.sireum.amandroid.AppCenter
import org.sireum.amandroid.decompile.AmDecoder
import org.sireum.jawa.alir.pta.suspark.InterproceduralSuperSpark
import org.sireum.jawa.util.MyTimeoutException
import org.sireum.jawa.util.MyTimer
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


/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object GenGraphCli {
	def run(saamode : SireumAmandroidGenGraphMode) {
    val sourceType = saamode.general.typ
    val sourceDir = saamode.srcFile
    val sourceFile = new File(sourceDir)
    val outputDir = saamode.analysis.outdir
    val nostatic = saamode.analysis.noStatic
    val parallel = saamode.analysis.parallel
    val noicc = saamode.analysis.noicc
    val k_context = saamode.analysis.k_context
    val timeout = saamode.analysis.timeout
    val mem = saamode.general.mem
    val msgLevel = saamode.general.msgLevel
    val format = saamode.format
    val graphtyp = saamode.graphtyp
    forkProcess(nostatic, parallel, noicc, k_context, timeout, sourceType, sourceDir, outputDir, mem, format, graphtyp, msgLevel)
  }
	
	def forkProcess(nostatic : Boolean, parallel : Boolean, noicc : Boolean, k_context : Int, timeout : Int, typSpec : AnalyzeSource.Type, sourceDir : String, outputDir : String, mem : Int, format : GraphFormat.Type, graphtyp : GraphType.Type, msgLevel : MessageLevel.Type) = {
    val args : MList[String] = mlistEmpty
    args += "-s"
    args += (!nostatic).toString
    args += "-par"
    args += parallel.toString
    args += "-i"
    args += (!noicc).toString
    args += "-k"
    args += k_context.toString
    args += "-to"
    args += timeout.toString
    args += "-f"
    args += format.toString
    args += "-gt"
    args += graphtyp.toString
    args += "-msg"
    args += msgLevel.toString
    args ++= List("-t", typSpec.toString, sourceDir, outputDir)
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
	  if(args.size != 20){
      println("Usage: -s [handle static init] -par [parallel] -i [handle icc] -k [k context] -to [timeout minutes] -f [Graph Format: DOT, GraphML, GML] -gt [Graph Type: FULL, SIMPLE_CALL, DETAILED_CALL, API] -msg [Message Level: NO, CRITICAL, NORMAL, VERBOSE] -t type[allows: APK, DIR] <source path> <output path>")
      return
    }
    val static = args(1).toBoolean
    val parallel = args(3).toBoolean
    val icc = args(5).toBoolean
    val k_context = args(7).toInt
    val timeout = args(9).toInt
    val format = args(11)
    val graphtyp = args(13)
    val msgLevel = args(15)
    val typ = args(17)
    val sourcePath = args(18)
    val outputPath = args(19)
    
    msgLevel match{
      case "NO$" =>
        MessageCenter.msglevel = MessageCenter.MSG_LEVEL.NO
      case "CRITICAL$" =>
        MessageCenter.msglevel = MessageCenter.MSG_LEVEL.CRITICAL
      case "NORMAL$" =>
        MessageCenter.msglevel = MessageCenter.MSG_LEVEL.NORMAL
      case "VERBOSE$" =>
        MessageCenter.msglevel = MessageCenter.MSG_LEVEL.VERBOSE
      case _ => 
        println("Unexpected msg level: " + msgLevel)
        return
    }
    
    val apkFileUris = typ match{
      case "APK$" =>
        require(sourcePath.endsWith(".apk"))
        Set(FileUtil.toUri(sourcePath))
      case "DIR$" =>
        require(new File(sourcePath).isDirectory())
        FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
      case _ => 
        println("Unexpected type: " + typ)
        return
    }
		genGraph(apkFileUris, outputPath, static, parallel, icc, k_context, timeout, format, graphtyp)
	}
  
  def genGraph(apkFileUris : Set[FileResourceUri], outputPath : String, static : Boolean, parallel : Boolean, icc : Boolean, k_context : Int, timeout : Int, format : String, graphtyp : String) = {
    GlobalConfig.ICFG_CONTEXT_K = k_context
    println("Total apks: " + apkFileUris.size)
    try{
      var i : Int = 0
      JawaCodeSource.preLoad(FileUtil.toUri(AndroidGlobalConfig.android_lib_dir), GlobalConfig.PILAR_FILE_EXT)
      
      apkFileUris.foreach{
        apkFileUri =>
          try{
            i+=1
            println("Analyzing #" + i + ":" + apkFileUri)
            val timer = Some(new MyTimer(timeout*60))
            if(timer.isDefined) timer.get.start
            val apkName = apkFileUri.substring(apkFileUri.lastIndexOf("/") + 1, apkFileUri.lastIndexOf("."))
            
        		val resultDir = new File(outputPath + "/APPs/")
            val outputUri = FileUtil.toUri(outputPath)
        		val outUri = AmDecoder.decode(apkFileUri, outputUri)
            val fileandout = (apkFileUri, outUri + "classes")
        
        		// convert the dex file to the "pilar" form
        		val pilarRootUri = Dex2PilarConverter.convert(fileandout._1, fileandout._2)
          	//store the app's pilar code in AmandroidCodeSource which is organized record by record.
          	JawaCodeSource.load(pilarRootUri, GlobalConfig.PILAR_FILE_EXT, AndroidLibraryAPISummary)
          	
          	val app_info = new AppInfoCollector(apkFileUri, outUri, timer)
          	app_info.collectInfo

            val eps = app_info.getEntryPoints
            val pros =
              eps.map{
                compName =>
                  val comp = Center.resolveRecord(compName, Center.ResolveLevel.BODY)
                  val procedures = comp.getProceduresByShortName(AndroidConstants.MAINCOMP_ENV) ++ comp.getProceduresByShortName(AndroidConstants.COMP_ENV)
                  procedures
              }.reduce(iunion[JawaProcedure])

            val icfg = InterproceduralSuperSpark(pros, timer).icfg
          	
            val file = new File(outputPath + "/" + apkName.filter(_.isUnicodeIdentifierPart) + ".txt.gz")
      	    val w = new FileOutputStream(file)
            val zips = new GZIPOutputStream(new BufferedOutputStream(w))
            val zipw = new BufferedWriter(new OutputStreamWriter(zips, "UTF-8"))
            
            graphtyp match{
              case "FULL$" => 
                val graph = icfg
                format match {
                  case "DOT$" => graph.toDot(zipw)
                  case "GraphML$" => graph.toGraphML(zipw)
                  case "GML$" => graph.toGML(zipw)
                }
              case "SIMPLE_CALL$" => 
                val graph = icfg.getCallGraph.toSimpleCallGraph
                format match {
                  case "DOT$" => graph.toDot(zipw)
                  case "GraphML$" => graph.toGraphML(zipw, graph.vIDProvider, null, graph.eIDProvider, null)
                  case "GML$" => graph.toGML(zipw, graph.vIDProvider, null, graph.eIDProvider, null)
                }
              case "DETAILED_CALL$" => 
                val graph = icfg.getCallGraph.toDetailedCallGraph(icfg)
                format match {
                  case "DOT$" => graph.toDot(zipw)
                  case "GraphML$" => graph.toGraphML(zipw, graph.vIDProvider, graph.vLabelProvider, graph.eIDProvider, null)
                  case "GML$" => graph.toGML(zipw, graph.vIDProvider, graph.vLabelProvider, graph.eIDProvider, null)
                }
              case "API$" => 
                val graph = icfg.toApiGraph
                format match {
                  case "DOT$" => graph.toDot(zipw)
                  case "GraphML$" => graph.toGraphML(zipw)
                  case "GML$" => graph.toGML(zipw)
                }
            }
      	    zipw.close()
      	    println(apkName + " result stored!")
            
            println("Done!")
          } catch {
            case te : MyTimeoutException => err_msg_critical(TITLE, te.message)
            case e : Throwable => 
              CliLogger.logError(new File(outputPath), "Error: " , e)
          } finally {
            Center.reset
          	AppCenter.reset
          	// before starting the analysis of the current app, first clear the previous app's records' code from the AmandroidCodeSource
          	JawaCodeSource.clearAppRecordsCodes
          	System.gc()
            System.gc()
          }
      }
    } catch {
      case e : Throwable => 
        CliLogger.logError(new File(outputPath), "Error: " , e)

    }
	}
}