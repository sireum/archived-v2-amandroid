/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.cli

import org.sireum.option.SireumAmandroidGenCallGraphMode
import org.sireum.option.AnalyzeSource
import java.io.File
import org.sireum.option.MessageLevel
import org.sireum.util._
import org.sireum.jawa.MessageCenter
import org.sireum.amandroid.cli.util.CliLogger
import org.sireum.jawa.util.IgnoreException
import java.net.URI
import org.sireum.jawa.util.APKFileResolver
import org.sireum.amandroid.decompile.Dex2PilarConverter
import org.sireum.jawa.JawaCodeSource
import org.sireum.jawa.GlobalConfig
import org.sireum.amandroid.util.AndroidLibraryAPISummary
import org.sireum.amandroid.appInfo.AppInfoCollector
import org.sireum.jawa.alir.pointsToAnalysis.PointerAssignmentGraph
import org.sireum.jawa.alir.controlFlowGraph.InterproceduralControlFlowGraph
import org.sireum.jawa.alir.pointsToAnalysis.PtaNode
import org.sireum.jawa.alir.controlFlowGraph.CGNode
import org.sireum.jawa.alir.pointsToAnalysis.InterproceduralPointsToAnalysis
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


/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object GenCallGraphCli {
	def run(saamode : SireumAmandroidGenCallGraphMode) {
    val sourceType = saamode.general.typ match{
      case AnalyzeSource.APK => "APK"
      case AnalyzeSource.DIR => "DIR"}
    val sourceDir = saamode.srcFile
    val sourceFile = new File(sourceDir)
    val outputDir = saamode.outFile
    val mem = saamode.general.mem
    val msgLevel = saamode.general.msgLevel
    forkProcess(sourceType, sourceDir, outputDir, mem, msgLevel)
  }
	
	def forkProcess(typSpec : String, 
	    						sourceDir : String, outputDir : String, 
	    						mem : Int,
	    						msgLevel : MessageLevel.Type) = {
	  val args : MList[String] = mlistEmpty
	  args += "-msg"
	  args += msgLevel.toString
	  args ++= List("-t", typSpec, sourceDir, outputDir)
    org.sireum.jawa.util.JVMUtil.startSecondJVM(GenCallGraph.getClass(), "-Xmx" + mem + "G", args.toList, true)
  }
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object GenCallGraph {
  
  private final val TITLE = "GenCallGraph"
  
	def main(args: Array[String]) {
	  if(args.size != 6){
	    println("Usage: -msg [Message Level: NO, CRITICAL, NORMAL, VERBOSE] -t type[allows: APK, DIR] <source path> <output path>")
	    return
	  }
	  val msgLevel = args(1)
	  val typ = args(3)
	  val sourcePath = args(4)
	  val outputPath = args(5)
	  
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
      case "APK" =>
        require(sourcePath.endsWith(".apk"))
        Set(FileUtil.toUri(sourcePath))
      case "DIR" =>
        require(new File(sourcePath).isDirectory())
        FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
      case _ => 
        println("Unexpected type: " + typ)
        return
    }
		genCallGraph(apkFileUris, outputPath)
	}
  
  def genCallGraph(apkFileUris : Set[FileResourceUri], outputPath : String) = {
    
    println("Total apks: " + apkFileUris.size)
    try{
      var i : Int = 0
      JawaCodeSource.preLoad(FileUtil.toUri(AndroidGlobalConfig.android_lib_dir), GlobalConfig.PILAR_FILE_EXT)
      
      apkFileUris.foreach{
        apkFileUri =>
          try{
            i+=1
            println("Analyzing " + apkFileUri)
            
            val apkName = apkFileUri.substring(apkFileUri.lastIndexOf("/"), apkFileUri.lastIndexOf("."))
            
        		val resultDir = new File(outputPath + "/APPs/")
            val outputUri = FileUtil.toUri(outputPath)
        		val outUri = AmDecoder.decode(apkFileUri, outputUri)
            val dexFile = outUri + "/classes.dex"
        
        		// convert the dex file to the "pilar" form
        		val pilarRootUri = Dex2PilarConverter.convert(dexFile)
            
          	//store the app's pilar code in AmandroidCodeSource which is organized record by record.
          	JawaCodeSource.load(pilarRootUri, GlobalConfig.PILAR_FILE_EXT, AndroidLibraryAPISummary)
          	
          	val app_info = new AppInfoCollector(apkFileUri, outUri)
          	app_info.collectInfo
            
          	val pag = new PointerAssignmentGraph[PtaNode]()
            val cg = new InterproceduralControlFlowGraph[CGNode]
            val eps = app_info.getEntryPoints
            val pros =
              eps.map{
                compName =>
                  val comp = Center.resolveRecord(compName, Center.ResolveLevel.BODY)
                  val procedures = comp.getProceduresByShortName(AndroidConstants.MAINCOMP_ENV) ++ comp.getProceduresByShortName(AndroidConstants.COMP_ENV)
                  procedures
              }.reduce(iunion[JawaProcedure])

            new InterproceduralPointsToAnalysis().pta(pag, cg, pros, false)
          	
            val file = new File(outputPath + "/" + apkName.filter(_.isUnicodeIdentifierPart) + ".txt.gz")
      	    val w = new FileOutputStream(file)
            val zipw = new GZIPOutputStream(new BufferedOutputStream(w))
      	    val graph = cg.toTextGraph
      	    zipw.write(graph.getBytes())
      	    zipw.close()
      	    println(apkName + " result stored!")
            
            println("#" + i + ":Done!")
          } catch {
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