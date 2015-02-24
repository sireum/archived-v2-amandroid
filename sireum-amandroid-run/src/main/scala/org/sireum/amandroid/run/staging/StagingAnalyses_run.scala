///*
//Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
//All rights reserved. This program and the accompanying materials      
//are made available under the terms of the Eclipse Public License v1.0 
//which accompanies this distribution, and is available at              
//http://www.eclipse.org/legal/epl-v10.html                             
//*/
//package org.sireum.amandroid.run.staging
//
//import org.junit.runner.RunWith
//import org.scalatest.junit.JUnitRunner
//import org.sireum.amandroid.AndroidGlobalConfig
//import org.sireum.jawa.JawaCodeSource
//import org.sireum.jawa.alir.LibSideEffectProvider
//import org.sireum.amandroid.libPilarFiles.AndroidLibPilarFiles
//import org.sireum.util.FileUtil
//import org.sireum.jawa.GlobalConfig
//import org.sireum.amandroid.appInfo.AppInfoCollector
//import org.sireum.amandroid.alir.taintAnalysis.DefaultAndroidSourceAndSinkManager
//import java.io.File
//import java.net.URI
//import org.sireum.jawa.util.APKFileResolver
//import org.sireum.amandroid.decompile.Dex2PilarConverter
//import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidRFAConfig
//import org.sireum.amandroid.util.AndroidLibraryAPISummary
//import org.sireum.jawa.Center
//import org.sireum.amandroid.AndroidConstants
//import org.sireum.amandroid.AppCenter
//import org.sireum.jawa.MessageCenter._
//import org.sireum.amandroid.alir.taintAnalysis.AndroidDataDependentTaintAnalysis
//import java.util.zip.GZIPInputStream
//import java.io.FileInputStream
//import org.sireum.jawa.xml.AndroidXStream
//import org.sireum.amandroid.alir.dataRecorder.AmandroidResult
//import org.sireum.amandroid.security.password.PasswordSourceAndSinkManager
//import org.sireum.amandroid.security.oauth.OAuthSourceAndSinkManager
//import org.sireum.amandroid.security.dataInjection.IntentInjectionSourceAndSinkManager
//import org.sireum.amandroid.security.apiMisuse.CryptographicMisuse
//import org.sireum.jawa.alir.interProcedural.InterProceduralDataFlowGraph
//import org.sireum.amandroid.decompile.AmDecoder
//
//
///**
// * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
// */
//object StagingAnalyses_run {
//  
//  private final val TITLE = "StagingAnalyses_run"
//  
//  def main(args: Array[String]) {
//    if(args.size != 1){
//      System.err.print("Usage: source_path")
//      return
//    }
//    
//    JawaCodeSource.preLoad(FileUtil.toUri(AndroidGlobalConfig.android_lib_dir), GlobalConfig.PILAR_FILE_EXT)
//    LibSideEffectProvider.init(new File(AndroidGlobalConfig.android_libsummary_dir + "/AndroidLibSideEffectResult.xml.gz"))
//    val outputUri = FileUtil.toUri("/media/fgwei/c3337db2-6708-4063-9079-a61c105f519f/Outputs/staging_analyses")
//    val sourcePath = args(0)
//    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
//    files.foreach{
//      file =>
//  	    println("Processing -> " + file)
//  	    try{
//  	    	
//  	    	val srcFile = new File(new URI(file))
//  	    	val outUri = AmDecoder.decode(file, outputUri)
//  	    	val dexFile = outUri + "/classes.dex"
//  	    	// convert the dex file to the "pilar" form
//  	    	val pilarRootUri = Dex2PilarConverter.convert(dexFile)
//  	    	val pilarFile = new File(new URI(pilarRootUri))
//  	  		
//  	    	//store the app's pilar code in AmandroidCodeSource which is organized record by record.
//  	    	JawaCodeSource.load(pilarRootUri, GlobalConfig.PILAR_FILE_EXT, AndroidLibraryAPISummary)
//  		    
//  	    	val pre = new AppInfoCollector(file, outUri)
//  		    pre.collectInfo
//  	    	val ssm = new DefaultAndroidSourceAndSinkManager(pre.getPackageName, pre.getLayoutControls, pre.getCallbackMethods, AndroidGlobalConfig.SourceAndSinkFilePath)
//  		    val pssm = new PasswordSourceAndSinkManager(pre.getPackageName, pre.getLayoutControls, pre.getCallbackMethods, AndroidGlobalConfig.PasswordSinkFilePath)
//  		    val ossm = new OAuthSourceAndSinkManager(pre.getPackageName, pre.getLayoutControls, pre.getCallbackMethods, AndroidGlobalConfig.PasswordSinkFilePath)
//  		    val iissm = new IntentInjectionSourceAndSinkManager(pre.getPackageName, pre.getLayoutControls, pre.getCallbackMethods, AndroidGlobalConfig.IntentInjectionSinkFilePath)
//  		    
//  	    	var entryPoints = Center.getEntryPoints(AndroidConstants.MAINCOMP_ENV)
//  		    entryPoints ++= Center.getEntryPoints(AndroidConstants.COMP_ENV)
//  		    
//  		    val fileName = file.substring(file.lastIndexOf("/"), file.lastIndexOf("."))
//    	    val outputDir = AndroidGlobalConfig.amandroid_home + "/output"
//  		  	val fileDir = new File(outputDir + "/AmandroidResult/ResultStore/" + fileName)
//  		    
//  		    var starttime : Long = 0
//  		    var endtime : Long = 0
//  		    
//  		    entryPoints.foreach{
//  	    	  ep =>
//  	    	    try{
//  		    	    msg_critical(TITLE, "--------------Component " + ep + "--------------")
//  		    	    val file = new File(fileDir + "/" + ep.getDeclaringRecord.getName.filter(_.isUnicodeIdentifierPart) + ".xml.gz")
//  		    	    if(file.exists()){
//  						    val reader = new GZIPInputStream(new FileInputStream(file))
//  						    val xmlObject = AndroidXStream.fromXml(reader).asInstanceOf[AmandroidResult]
//  					      reader.close()
//  					      msg_critical(TITLE, "Data Leak Detection")
//  					      starttime = System.currentTimeMillis()
//  					      val tar = AndroidDataDependentTaintAnalysis(xmlObject.ddg, xmlObject.idfg.summary, ssm)    
//  					      endtime = System.currentTimeMillis()
//  					      err_msg_critical(TITLE, "Data leak time: " + (endtime - starttime) + "ms")
//  					      msg_critical(TITLE, "Password Leak Detection")
//  					      starttime = System.currentTimeMillis()
//  			    	    val ptar = AndroidDataDependentTaintAnalysis(xmlObject.ddg, xmlObject.idfg.summary, pssm)    
//  			    	    endtime = System.currentTimeMillis()
//  					      err_msg_critical(TITLE, "Password leak time: " + (endtime - starttime) + "ms")
//  			    	    msg_critical(TITLE, "OAuth Leak Detection")
//  			    	    starttime = System.currentTimeMillis()
//  			    	    val otar = AndroidDataDependentTaintAnalysis(xmlObject.ddg, xmlObject.idfg.summary, ossm)    
//  			    	    endtime = System.currentTimeMillis()
//  					      err_msg_critical(TITLE, "OAuth leak time: " + (endtime - starttime) + "ms")
//  			    	    msg_critical(TITLE, "Injent Injection Detection")
//  			    	    starttime = System.currentTimeMillis()
//  			    	    val iitar = AndroidDataDependentTaintAnalysis(xmlObject.ddg, xmlObject.idfg.summary, iissm)    
//  			    	    endtime = System.currentTimeMillis()
//  					      err_msg_critical(TITLE, "Injent Injection Detection time: " + (endtime - starttime) + "ms")
//  			    	    msg_critical(TITLE, "Crypto API Misuse Check")
//  			    	    starttime = System.currentTimeMillis()
//  			    	    CryptographicMisuse(new InterProceduralDataFlowGraph(xmlObject.idfg.icfg, xmlObject.idfg.summary))
//  			    	    endtime = System.currentTimeMillis()
//  					      err_msg_critical(TITLE, "Crypto API Misuse Check time: " + (endtime - starttime) + "ms")
//  		    	    } else {
//  		    	      println("Component " + ep + " does not exist.")
//  		    	    }
//  	    	    } catch {
//  	    	      case e : Exception =>
//  	    	        e.printStackTrace()
//  	    	    }
//    	    }
//  	    } catch {
//  	      case e : Exception => e.printStackTrace()
//  	    } finally {
//  	      Center.reset
//  	    	AppCenter.reset
//  	    	// before starting the analysis of the current app, first clear the previous app's records' code from the AmandroidCodeSource
//  	    	JawaCodeSource.clearAppRecordsCodes
//  	    	System.gc()
//  	    }
//		}
//  }
//  
//}