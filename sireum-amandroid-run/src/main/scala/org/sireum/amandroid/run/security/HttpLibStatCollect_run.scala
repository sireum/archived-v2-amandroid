/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.run.security

import org.sireum.util._
import org.sireum.jawa.MessageCenter._
import java.io.File
import org.sireum.jawa.util.APKFileResolver
import java.net.URI
import org.sireum.jawa.JawaCodeSource
import org.sireum.jawa.Center
import org.sireum.jawa.ClassLoadManager
import org.sireum.jawa.util.IgnoreException
import org.sireum.jawa.GlobalConfig
import java.io.PrintWriter
import org.sireum.jawa.alir.LibSideEffectProvider
import org.sireum.amandroid.AndroidGlobalConfig
import org.sireum.amandroid.decompile.Dex2PilarConverter
import org.sireum.amandroid.util.AndroidLibraryAPISummary
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidRFAConfig
import org.sireum.amandroid.AppCenter

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object HttpLibStatCollect_run {
  private final val TITLE = "HttpLibCollect_run"
  object Counter {
    var total = 0
    var haveresult = 0
    var javaNetHttpClientUser = Set[String]()
   
    var apacheHttpClientUser = Set[String]()
    
    var googleHttpClientUser = Set[String]()
    
    var chBoyeHttpClientUser = Set[String]()
    
    def write = {
      val outputDir = AndroidGlobalConfig.amandroid_home + "/output"
      val appDataDirFile = new File(outputDir + "/httpLibStat")
    	if(!appDataDirFile.exists()) appDataDirFile.mkdirs()
    	val out = new PrintWriter(appDataDirFile + "/summary.txt")
      try{
           if(javaNetHttpClientUser.size > 0) {
              out.write("\n java net http client users are : \n")
              javaNetHttpClientUser.foreach{
               srcFileName =>
                 out.write(srcFileName + "\n")
             }
           }
           
        
            if(apacheHttpClientUser.size > 0) {
              out.write("\n apache http client users are : \n")
              apacheHttpClientUser.foreach{
               srcFileName =>
                 out.write(srcFileName + "\n")
             }
           }
           
         if(googleHttpClientUser.size > 0) {
              out.write("\n google HttpClient users are : \n")
              googleHttpClientUser.foreach{
               srcFileName =>
                 out.write(srcFileName + "\n")
             }
           }
           
        
          
          if(chBoyeHttpClientUser.size > 0) {
              out.write("\n ch.Boye Http Client users are : \n")
              chBoyeHttpClientUser.foreach{
               srcFileName =>
                out.write(srcFileName + "\n")
              }
          }
          
       
          
          if(javaNetHttpClientUser.size > 0 & apacheHttpClientUser.size > 0) {
              out.write("\n javaNet-and-apache Http users are : \n")
              javaNetHttpClientUser.intersect(apacheHttpClientUser).foreach{
               srcFileName =>
                out.write(srcFileName + "\n")
              }
          }
          
  
      } finally {
      	out.close()
      }
    }
    
    override def toString : String = 
      "total: " + total + ", haveResult: " + haveresult + 
  		", javaNetHttpClientUser: " + javaNetHttpClientUser.size +
  		", apacheHttpClientUser: " + apacheHttpClientUser.size +
  		", JavaNetAndApacheHttpClientUser:" + (javaNetHttpClientUser.intersect(apacheHttpClientUser)).size +
  		", googleHttpClientUser: " + googleHttpClientUser.size +
  		", chBoyeHttpClientUser:" + chBoyeHttpClientUser.size 
  }
  
  def main(args: Array[String]): Unit = {
    if(args.size != 2){
      System.err.print("Usage: source_path output_path")
      return
    }
    
    val sourcePath = args(0)
    val outputPath = args(1)
    
    //JawaCodeSource.preLoad(FileUtil.toUri(AndroidGlobalConfig.android_lib_dir), GlobalConfig.PILAR_FILE_EXT)
    //LibSideEffectProvider.init(new File(AndroidGlobalConfig.android_libsummary_dir + "/AndroidLibSideEffectResult.xml.zip"))
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
    files.foreach{
      file =>
        try{
          msg_critical(TITLE, "####" + file + "#####")
        	Counter.total += 1
        	
        	val srcFile = new File(new URI(file))
        	val dexFile = APKFileResolver.getDexFile(file, FileUtil.toUri(srcFile.getParentFile()))
        	
        	// convert the dex file to the "pilar" form
        	val pilarFileUri = Dex2PilarConverter.convert(dexFile, FileUtil.toUri(srcFile.getParentFile()))
      		
        	//store the app's pilar code in AmandroidCodeSource which is organized record by record.
        	JawaCodeSource.load(pilarFileUri, GlobalConfig.PILAR_FILE_EXT, AndroidLibraryAPISummary)
      	
      	
        	(JawaCodeSource.getAppRecordsCodes).foreach{
        	  case (name, code) => 	    
        	    if(code.contains("java.net.HttpURLConnection")){  // java net http client
        	      System.err.println("java net httpURLConn in App code record " + name)
        	      Counter.javaNetHttpClientUser += file
        	      
        	    }
        	   
        	    
        	    if(code.contains("org.apache.http.client")){
        	      System.err.println("apache http client in App code record " + name)
        	      Counter.apacheHttpClientUser += file
        	    }
        	    
        	    if(code.contains("com.google.api.client.http")){
        	      System.err.println("google http client in App code record " + name)
        	      Counter.googleHttpClientUser += file
        	    }
        	    
        	    if(code.contains("ch.boye.httpclientandroidlib")){
        	      System.err.println("ch boye http client in App code record " + name)
        	      Counter.chBoyeHttpClientUser += file
        	    }
        	}
      	
      	
        	(JawaCodeSource.getThirdPartyLibraryRecordsCodes).foreach{
        	  case (name, code) =>
        	    
        	   if(code.contains("java.net.HttpURLConnection")){  // java net http client
        	      System.err.println("java net httpURLConn in App Using Lib " + name)
        	      Counter.javaNetHttpClientUser += file
        	      
        	    }
        	          	    
        	    if(code.contains("org.apache.http.client")){
        	      System.err.println("apache http client in App Using Lib " + name)
        	      Counter.apacheHttpClientUser += file
        	    }
        	    
        	    if(code.contains("com.google.api.client.http")){
        	      System.err.println("google http client in App Using Lib " + name)
        	      Counter.googleHttpClientUser += file
        	    }
        	    
        	    if(code.contains("ch.boye.httpclientandroidlib")){
        	      System.err.println("ch boye http client in App Using Lib " + name)
        	      Counter.chBoyeHttpClientUser += file
        	    }
        	    
  
        	}
      	} catch {
      	  case e : Exception =>
      	    e.printStackTrace();
      	} finally {
  	    	Center.reset
  	    	AppCenter.reset
  	    	// before starting the analysis of the current app, first clear the previous app's records' code from the AmandroidCodeSource
  	    	JawaCodeSource.clearAppRecordsCodes
  			  System.gc()
  			  Counter.write
  	    	msg_critical(TITLE, Counter.toString)
  	    	msg_critical(TITLE, "************************************\n")
      	}
    }
  }
}