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
import org.sireum.amandroid.AndroidGlobalConfig
import java.io.File
import java.io.PrintWriter
import java.net.URI
import org.sireum.jawa.util.APKFileResolver
import org.sireum.amandroid.decompile.Dex2PilarConverter
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.AndroidRFAConfig
import org.sireum.jawa.JawaCodeSource
import org.sireum.jawa.GlobalConfig
import org.sireum.amandroid.util.AndroidLibraryAPISummary
import org.sireum.jawa.Center
import org.sireum.amandroid.AppCenter
import org.sireum.amandroid.appInfo.AppInfoCollector
import org.sireum.amandroid.parser.ResourceFileParser
import org.sireum.amandroid.parser.ARSCFileParser
import java.util.regex.Pattern
import org.sireum.jawa.MessageCenter
import org.sireum.amandroid.security.AmandroidSocket

/**
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */
object SsoOauthStringCollect_run {

  private final val TITLE = "SsoOauthStringCollect_run"
  
  case class relStrs(typ : String, resOrRec : String) { 
    var strs : Set[String] = Set()
  }
  
  object Counter {
    var total = 0
    var haveresult = 0
    var ssoLoginUser = Set[String]() // stores apk names which have single sign on e.g "via facebook login"
    var oauthUser = Set[String]() //stores apk names which have oauth
    var oauth1User = Set[String]() //stores apk names which have oauth1
    var oauth2codeUser = Set[String]() //stores apk names which have oauth2code
    var ssoInfo : Map[String, Set[relStrs]] = Map() // apkfile -> (key/record, relevant strings)
    var oauthInfo : Map[String, Set[relStrs]] = Map() // apkfile -> (key/record, relevant strings)
    var oauth1Info : Map[String, Set[relStrs]] = Map() // apkfile -> (key/record, relevant strings)
    var oauth2codeInfo : Map[String, Set[relStrs]] = Map() // apkfile -> (key/record, relevant strings)
    
    def write = {
      val outputDir = AndroidGlobalConfig.amandroid_home + "/output"
      val appDataDirFile = new File(outputDir + "/ssoLoginStringsStat")
    	if(!appDataDirFile.exists()) appDataDirFile.mkdirs()
    	val outSso = new PrintWriter(appDataDirFile + "/summarySso.txt")
      val outOauth = new PrintWriter(appDataDirFile + "/summaryOauth.txt")
      val outOauth1 = new PrintWriter(appDataDirFile + "/summaryOauth1.txt")
      val outOauth2code = new PrintWriter(appDataDirFile + "/summaryOauth2code.txt")
      val outInfoSso = new PrintWriter(appDataDirFile + "/relatedStringsSso.txt")
      val outInfoOauth = new PrintWriter(appDataDirFile + "/relatedStringsOauth.txt")
      val outInfoOauth1 = new PrintWriter(appDataDirFile + "/relatedStringsOauth1.txt")
      val outInfoOauth2code = new PrintWriter(appDataDirFile + "/relatedStringsOauth2code.txt")
      
      try{
           if(ssoLoginUser.size > 0) {
              outSso.write("\n sso login users are : \n")
              ssoLoginUser.foreach{
               srcFileName =>
                 outSso.write(srcFileName + "\n")                
             }
            
           }
           
           if(oauthUser.size > 0) {
              outOauth.write("\n oauth users are : \n")
              oauthUser.foreach{
               srcFileName =>
                 outOauth.write(srcFileName + "\n")                
             }
            
           }
           
           if(oauth1User.size > 0) {
              outOauth1.write("\n oauth1 users are : \n")
              oauth1User.foreach{
               srcFileName =>
                 outOauth1.write(srcFileName + "\n")                
             }
            
           }
           
           if(oauth2codeUser.size > 0) {
              outOauth2code.write("\n oauth2code users are : \n")
              oauth2codeUser.foreach{
               srcFileName =>
                 outOauth2code.write(srcFileName + "\n")                
             }
            
           }
           
           
           
           if(!ssoInfo.isEmpty) {
              outInfoSso.write("\n sso infos are as follows \n")
              ssoInfo.foreach{           
                case (srcFileName, info) =>
                 outInfoSso.write("\nin "+ srcFileName + ":\n")
                 if(!info.isEmpty)
                 info.foreach{
                   case x =>
                     outInfoSso.write(" \n" + x.resOrRec + x.typ + "   contains \n      " + x.strs.toString()) 
                 }
                                
             }
            
           }
           
           if(!oauthInfo.isEmpty) {
              outInfoOauth.write("\n oauth infos are as follows \n")
              oauthInfo.foreach{           
                case (srcFileName, info) =>
                 outInfoOauth.write("\nin "+ srcFileName + ":\n")
                 if(!info.isEmpty)
                 info.foreach{
                   case x =>
                     outInfoOauth.write(" \n" + x.resOrRec + x.typ + "   contains \n      " + x.strs.toString()) 
                 }
                                
             }
            
           }
           
           if(!oauth1Info.isEmpty) {
              outInfoOauth1.write("\n oauth1 infos are as follows \n")
              oauth1Info.foreach{           
                case (srcFileName, info) =>
                 outInfoOauth1.write("\nin "+ srcFileName + ":\n")
                 if(!info.isEmpty)
                 info.foreach{
                   case x =>
                     outInfoOauth1.write(" \n" + x.resOrRec + x.typ + "   contains \n      " + x.strs.toString()) 
                 }
                                
             }
            
           }
           
           if(!oauth2codeInfo.isEmpty) {
              outInfoOauth2code.write("\n oauth2code infos are as follows \n")
              oauth2codeInfo.foreach{           
                case (srcFileName, info) =>
                 outInfoOauth2code.write("\nin "+ srcFileName + ":\n")
                 if(!info.isEmpty)
                 info.foreach{
                   case x =>
                     outInfoOauth2code.write(" \n" + x.resOrRec + x.typ + "   contains \n      " + x.strs.toString()) 
                 }
                                
             }
            
           }
 
      } finally {
      	outSso.close()
      	outInfoSso.close()
      	
      	outOauth.close()
      	outInfoOauth.close()
      	outOauth1.close()
      	outInfoOauth1.close()
      	outOauth2code.close()
      	outInfoOauth2code.close()
      }
    }
    
    override def toString : String = 
      "total: " + total +  
  		", sso login user count: " + ssoLoginUser.size +
  		", oauth user count: " + oauthUser.size +
  		", oauth1 user count: " + oauth1User.size +
  		", oauth2code user count: " + oauth2codeUser.size 
  }
  
  
  
  def main(args: Array[String]): Unit = {
    if(args.size != 2){
      System.err.print("Usage: source_path output_path")
      return
    }
    
    MessageCenter.msglevel = MessageCenter.MSG_LEVEL.CRITICAL
    
    val sourcePath = args(0)
    val outputPath = args(1)
    val socket = new AmandroidSocket
    //JawaCodeSource.preLoad(FileUtil.toUri(AndroidGlobalConfig.android_lib_dir), GlobalConfig.PILAR_FILE_EXT)
    //LibSideEffectProvider.init(new File(AndroidGlobalConfig.android_libsummary_dir + "/AndroidLibSideEffectResult.xml.zip"))
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
    files.foreach{
      file =>
        //if(file.contains("com.ridecharge.android.taximagic"))
        //if(!file.contains("com.mobilityflow.animatedweather.free")) // this apk keeps on running
        try{         
          msg_critical(TITLE, "####" + file + "#####")
        	Counter.total += 1
       	  
        	val outUri = socket.loadApk(file, outputPath, AndroidLibraryAPISummary)
          val man = AppInfoCollector.analyzeManifest(outUri + "AndroidManifest.xml")
        	val strs = msetEmpty[String]
          val rfp = new ResourceFileParser
        	rfp.parseResourceFile(file)
        	strs ++= rfp.getAllStrings
        	
        	//System.err.println("resource file parser output strings are " + strs.toString());
        	
          val strs2 = msetEmpty[String]
        	val arsc = new ARSCFileParser
        	arsc.parse(file)
        	strs2 ++= arsc.getGlobalStringPool.map(_._2)
          
          //System.err.println("ARSC file parser output strings are " + strs2.toString());
          
          strs ++=strs2
          
          
        	 val res_strs : Set[String] =
            if(!strs.isEmpty){
              strs.map{
                str =>
                  extract(str, regexSso)                   
              }.reduce(iunion[String])
            } else isetEmpty[String]
          
          if(!res_strs.isEmpty) {
            //System.err.println("sso login in " + file)
            // System.err.println("relevant strings in resource are " + res_strs.toString());
            
            Counter.ssoLoginUser += file
            val info : relStrs = relStrs("sso", "resource")
            info.strs ++= res_strs
            Counter.ssoInfo += (file -> (Counter.ssoInfo.getOrElse(file, Set()) + info))
            System.err.println("relevant strings in resource are " + res_strs.toString());
          }
          
          val res_strs_oauth : Set[String] =
            if(!strs.isEmpty){
              strs.map{
                str =>
                  extract(str, regexOauth)                   
              }.reduce(iunion[String])
            } else isetEmpty[String]
          
          if(!res_strs_oauth.isEmpty) {            
            Counter.oauthUser += file
            val info : relStrs = relStrs("oauth", "resource")
            info.strs ++= res_strs_oauth
            Counter.oauthInfo += (file -> (Counter.oauthInfo.getOrElse(file, Set()) + info))
            System.err.println("relevant strings in resource are " + res_strs_oauth.toString());
          }
          
           val res_strs_oauth1 : Set[String] =
            if(!strs.isEmpty){
              strs.map{
                str =>
                  extract(str, regexOauth1)                   
              }.reduce(iunion[String])
            } else isetEmpty[String]
          
          if(!res_strs_oauth1.isEmpty) {            
            Counter.oauth1User += file
            val info : relStrs = relStrs("oauth1", "resource")
            info.strs ++= res_strs_oauth1
            Counter.oauth1Info += (file -> (Counter.oauth1Info.getOrElse(file, Set()) + info))
            System.err.println("relevant strings in resource are " + res_strs_oauth1.toString());
          }
           
            val res_strs_oauth2code : Set[String] =
            if(!strs.isEmpty){
              strs.map{
                str =>
                  extract(str, regexOauth2code)                   
              }.reduce(iunion[String])
            } else isetEmpty[String]
          
          if(!res_strs_oauth2code.isEmpty) {            
            Counter.oauth2codeUser += file
            val info : relStrs = relStrs("oauth2code", "resource")
            info.strs ++= res_strs_oauth2code
            Counter.oauth2codeInfo += (file -> (Counter.oauth2codeInfo.getOrElse(file, Set()) + info))
            System.err.println("relevant strings in resource are " + res_strs_oauth2code.toString());
          }
          
          
        	val srcFile = new File(new URI(file))
        	val dexFile = APKFileResolver.getDexFile(file, FileUtil.toUri(srcFile.getParentFile()))
        	
        	// convert the dex file to the "pilar" form
        	val pilarFileUri = Dex2PilarConverter.convert(dexFile, FileUtil.toUri(srcFile.getParentFile()))
      		
        	//store the app's pilar code in AmandroidCodeSource which is organized record by record.
        	JawaCodeSource.load(pilarFileUri, GlobalConfig.PILAR_FILE_EXT, AndroidLibraryAPISummary)
      	
      	
        	(JawaCodeSource.getAppRecordsCodes).foreach{
        	  case (name, code) => 	    
        	    if(!extract(code, regexSso).isEmpty){         	     
        	      Counter.ssoLoginUser += file
        	      val info : relStrs = relStrs("sso", name)
        	      info.strs ++= extract(code, regexSso)
        	      println(info + " has: " + info.strs)
        	      Counter.ssoInfo += (file -> (Counter.ssoInfo.getOrElse(file, Set()) + info))
        	      System.err.println("relevant strings in app code" + "\n  record: " + name + "\n    " + extract(code, regexSso).toString());
        	    }
        	    
        	      if(!extract(code, regexOauth).isEmpty){         	      
        	      Counter.oauthUser += file
        	      val info : relStrs = relStrs("oauth", name)
        	      info.strs ++=  extract(code, regexOauth)
        	      println(info + " has: " + info.strs)
        	      Counter.oauthInfo += (file -> (Counter.oauthInfo.getOrElse(file, Set()) + info))
        	      System.err.println("relevant strings in app code"  + "\n  record: " + name + "\n    " + extract(code, regexOauth).toString());
        	    }
        	    
        	      
        	     if(!extract(code, regexOauth1).isEmpty){         	      
        	      Counter.oauth1User += file
        	      val info : relStrs = relStrs("oauth1", name)
        	      info.strs ++=  extract(code, regexOauth1)
        	      Counter.oauth1Info += (file -> (Counter.oauth1Info.getOrElse(file, Set()) + info))
        	      System.err.println("relevant strings in app code"  + "\n  record: " + name + "\n    " + extract(code, regexOauth1).toString());
        	    }
        	    
        	      if(!extract(code, regexOauth2code).isEmpty){         	      
        	      Counter.oauth2codeUser += file
        	      val info : relStrs = relStrs("oauth2code", name)
        	      info.strs ++=  extract(code, regexOauth2code)
        	      Counter.oauth2codeInfo += (file -> (Counter.oauth2codeInfo.getOrElse(file, Set()) + info))
        	      System.err.println("relevant strings in app code"  + "\n  record: " + name + "\n    " + extract(code, regexOauth2code).toString());
        	    }
        	    
          }
        	
        	(JawaCodeSource.getThirdPartyLibraryRecordsCodes).foreach{
        	  case (name, code) => 	    
        	    if(!extract(code, regexSso).isEmpty){ 
        	      Counter.ssoLoginUser += file
        	      val info : relStrs = relStrs("sso", name) 
        	      info.strs ++=  extract(code, regexSso)
        	      Counter.ssoInfo += (file -> (Counter.ssoInfo.getOrElse(file, Set()) + info))
        	      System.err.println("relevant strings in app lib code"  + "\n  record: " + name + "\n    " + extract(code, regexSso).toString());
        	    }
        	    
        	    if(!extract(code, regexOauth).isEmpty){         	      
        	      Counter.oauthUser += file
        	      val info : relStrs = relStrs("oauth", name) 
        	      info.strs ++=  extract(code, regexOauth)
        	      Counter.oauthInfo += (file -> (Counter.oauthInfo.getOrElse(file, Set()) + info))
        	      System.err.println("relevant strings in app lib code"  + "\n  record: " + name + "\n    " + extract(code, regexOauth).toString());
        	    }
        	    
        	    if(!extract(code, regexOauth1).isEmpty){         	      
        	      Counter.oauth1User += file
        	      val info : relStrs = relStrs("oauth1", name) 
        	      info.strs ++=  extract(code, regexOauth1)
        	      Counter.oauth1Info += (file -> (Counter.oauth1Info.getOrElse(file, Set()) + info))
        	      System.err.println("relevant strings in app lib code"  + "\n  record: " + name + "\n    " + extract(code, regexOauth1).toString());
        	    }
        	    
        	    if(!extract(code, regexOauth2code).isEmpty){         	      
        	      Counter.oauth2codeUser += file
        	      val info : relStrs = relStrs("oauth2code" ,name) 
        	      info.strs ++=  extract(code, regexOauth2code)
        	      Counter.oauth2codeInfo += (file -> (Counter.oauth2codeInfo.getOrElse(file, Set()) + info))
        	      System.err.println("relevant strings in app lib code"  + "\n  record: " + name + "\n    " + extract(code, regexOauth2code).toString());
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
  			  //Counter.write
  	    	msg_critical(TITLE, Counter.toString)
  	    	msg_critical(TITLE, "************************************\n")
      	}
    }
    Counter.write
  }
  
  
    // val regex = "(\\b((sso)|(SSO))\\b|\\b((([lL]og(\\s)*in)|([sS]ign(\\s)*on)|([rR]egister))(\\s)+(([uU]sing)|([vV]ia)|([tT]hrough))))"
    //val regex = "(((sso)|(SSO))|((([lL]og(\\s)?in)|([sS]ign(\\s)?on)|([rR]egister))\\s(([uU]sing)|([vV]ia)|([tT]hrough))))"
    //val regex = "(\\b((sso)|(SSO))\\b|\\b(([Ff]acebook)|([tT]witter)|([gG]oogle))(\\s)*([lL]ogin)\\b|\\b((([lL]og(\\s)*in)|([sS]ign(\\s)*on)|([rR]egister))(\\s)+(([bB]y)|([wW]ith)|([uU]sing)|([vV]ia)|([tT]hrough))))"
    //val regex = "(\\b((sso)|(SSO))\\b|\\b(([Ff]acebook)|([tT]witter)|([gG]oogle))(\\s)*([lL]ogin)\\b|\\b((([lL]og(\\s)*in)|([sS]ign(\\s)*on))(\\s)+(([bB]y)|([wW]ith)|([uU]sing)|([vV]ia)|([tT]hrough))))"
    
   val provider = "(([Ff]acebook)|([tT]witter)|([gG]oogle)|([lL]ive)|([mM]icrosoft)|([tT]umblr)|([lL]inkedin)|([yY]ahoo)|([fF]lickr))"
   val loginOpt = "(([lL]og(\\s)*in)|([sS]ign(\\s)*on)|([rR]egister))"
   val preposition = "(([bB]y)|([wW]ith)|([uU]sing)|([vV]ia)|([tT]hrough))"
   val keyWordSso =  "((sso)|(SSO)|([sS]ingle(\\s)*[sS]ign(\\s)*[oO]n))"
   
   val token = "((token)|(Token)|(TOKEN))"
   val code = "((code)|(Code)|(CODE))"
   val verifier = "(([vV]erifier)|([vV]eri)|([pP]in)|([pP]incode)|([pP]in(\\s)*[cC]ode))"
   val access = "((access)|(Access)|(ACCESS)|([aA]uth)|([Aa]uthorization))"
   val request = "(([rR]eq)|([Rr]equest)|(REQUEST))"
   val oauth = "((OAuth)|(Oauth)|(oauth)|(OAUTH))"
   
   val connect = "(\\s|_|\\.)*"
   val keyWordOauth = "("+ access + connect + token + ")" + "|" + oauth 
   val keyWordOauth1 = "("+ request + connect + token + ")" + "|" + "("+ "(" + access + "|" + oauth + ")" + connect + verifier + ")"
   val keyWordOauth2code = "("+ access + connect + code + ")"
   
   
   val regexSso = "(\\b" + keyWordSso + "\\b)" + "|" + "(\\b" + provider + "(\\s)*([lL]ogin)" + "\\b)" + "|" + "\\b(" + loginOpt + "(\\s)+" + preposition + "(\\s|\\w)*\\b" + provider + "\\b)"
   val regexOauth = "(\\b" + keyWordOauth + "\\b)"
   val regexOauth1 = "(\\b" + keyWordOauth1 + "\\b)"
   val regexOauth2code = "(\\b" + keyWordOauth2code + "\\b)"
   
  def extract(str : String, reg : String) : Set[String] = {
    var results : Set[String]  = Set()
    val p = Pattern.compile(reg)
    val m = p.matcher(str)
    while(m.find()) {
      val mStr = m.group()    
      results = results ++ Set(mStr)
    }
    results
  }
  
}