package org.sireum.amandroid.run.test

import org.sireum.util._
import org.sireum.amandroid._
import org.sireum.jawa.pilarParser.LightWeightPilarParser
import org.sireum.jawa.util.APKFileResolver
import org.sireum.amandroid.android.appInfo.AppInfoCollector
import java.io._
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis._
import org.sireum.amandroid.alir.interProcedural.taintAnalysis._
import org.sireum.jawa.util.StringFormConverter
import org.sireum.jawa.alir.interProcedural.dataDependenceAnalysis.InterproceduralDataDependenceAnalysis
import java.net.URI
import org.sireum.amandroid.alir.AppCenter
import org.sireum.amandroid.alir.dataRecorder.DataCollector
import org.sireum.amandroid.alir.dataRecorder.MetricRepo
import java.util.zip.ZipInputStream
import org.sireum.jawa.util.ResourceRetriever
import org.sireum.amandroid.alir.AndroidGlobalConfig
import org.sireum.jawa.MessageCenter._
import org.sireum.amandroid.alir.AndroidConstants
import org.sireum.jawa.JawaCodeSource
import org.sireum.jawa.Center
import org.sireum.jawa.ClassLoadManager
import org.sireum.amandroid.android.decompile.Dex2PilarConverter
import org.sireum.amandroid.android.util.AndroidLibraryAPISummary
import org.sireum.jawa.util.IgnoreException
import org.scalatest.exceptions.TestFailedDueToTimeoutException
import org.sireum.jawa.util.TimeOutException
import org.sireum.jawa.util.Timer
import org.sireum.jawa.alir.interProcedural.sideEffectAnalysis.InterProceduralSideEffectAnalysisResult
import org.sireum.jawa.alir.interProcedural.taintAnalysis.SourceAndSinkManager
import java.util.zip.GZIPOutputStream
import org.sireum.jawa.xml.AndroidXStream
import java.util.zip.GZIPInputStream
import org.sireum.jawa.alir.interProcedural.controlFlowGraph.InterproceduralControlFlowGraph
import org.sireum.jawa.alir.interProcedural.InterProceduralDataFlowGraph
import org.sireum.amandroid.alir.dataRecorder.AmandroidResult
import org.sireum.jawa.GlobalConfig
import org.sireum.jawa.MessageCenter
import org.sireum.jawa.alir.LibSideEffectProvider
import org.sireum.amandroid.android.parser.ResourceFileParser
import org.sireum.amandroid.android.parser.ARSCFileParser
import java.util.regex.Pattern

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object Test_run  {
  private final val TITLE = "Test_run"
  
  def main(args: Array[String]): Unit = {
//    if(args.size != 1){
//      System.err.print("Usage: source_path")
//      return
//    }
    
//    val sourcePath = args(0)
//    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
//    files.foreach{
//      file =>
//        msg_critical(TITLE, "####" + file + "#####")
//        val strs = msetEmpty[String]
//      	val rfp = new ResourceFileParser
//      	rfp.parseResourceFile(file)
//      	strs ++= rfp.getAllStrings
//      	val arsc = new ARSCFileParser
//      	arsc.parse(file)
//      	strs ++= arsc.getGlobalStringPool.map(_._2)
//      	println(strs)
//  		  System.gc()
//      	msg_critical(TITLE, "************************************\n")
//    }
    val strs = msetEmpty[String]
    
    strs += "sso"
    strs += "(.Login By) or SSO request_token"
    strs += "asbuiyqw9h login using facebook or twitter"
    strs += "asbuiyqw9h login by facebook or twitter shhxonx"
    strs += "login Via facebook or twitter"
    strs += "asgjsj Facebook      Login"
    strs += "twitter  Login"
    strs += "tumblerLogin oauth"
    strs += "twitterLogin login with your yahoo id"
    strs += "login with google id"
    
    val res_strs : Set[String] =
            if(!strs.isEmpty){
              strs.map{
                str =>
                  extract(str, regexSso)                 
              }.reduce(iunion[String])
            } else isetEmpty[String]
          
          if(!res_strs.isEmpty) {
            System.err.println("sso login in present")
            System.err.println("relevant strings are " + res_strs.toString());
            
          }
    
    val res_strs_oauth : Set[String] =
            if(!strs.isEmpty){
              strs.map{
                str =>
                  extract(str, regexOauth)                 
              }.reduce(iunion[String])
            } else isetEmpty[String]
          
          if(!res_strs_oauth.isEmpty) {
            System.err.println("oauth in present")
            System.err.println("relevant strings are " + res_strs_oauth.toString());
            
          }
    
    
    val testStr = "absmsdkjhdk </lpp02120-p];oauth verifier.;136-0u1aZLXI (login via Google)  ashihohnx;lnc access_token guowvdjla req.token access_veri sdhkbd"
    
    if(!extract(testStr, regexSso).isEmpty){  
       System.err.println("inside code: sso login is " + extract(testStr, regexSso))
    }
    else
      System.err.println("inside code: sso login is absent")
      
   if(!extract(testStr, regexOauth1).isEmpty){  
       System.err.println("inside code: oauth is " + extract(testStr, regexOauth1))
    }
    else
      System.err.println("inside code: oauth is absent")
    
  }
//   def extract(str : String) : Set[String] = {
//    val results = msetEmpty[String]
// 
//   val provider = "(([Ff]acebook)|([tT]witter)|([gG]oogle)|([lL]ive)|([mM]icrosoft)|([tT]umbler)|([lL]inkedin)|([yY]ahoo))"
//   val loginOpt = "(([lL]og(\\s)*in)|([sS]ign(\\s)*on)|([rR]egister))"
//   val preposition = "(([bB]y)|([wW]ith)|([uU]sing)|([vV]ia)|([tT]hrough))"
//   
//   
//   val regex = "(\\b((sso)|(SSO))\\b)" + "|" + "(\\b" + provider + "(\\s)*([lL]ogin)" + "\\b)" + "|" + "\\b(" + loginOpt + "(\\s)+" + preposition+ "(\\s|\\w)*\\b" + provider + "\\b)"
//   
//    
//    //val regex = "(sso) | (SSO) " 
//    val p = Pattern.compile(regex)
//    val m = p.matcher(str)
//    while(m.find()) {
//      var ssoStr = m.group()
//      
//      results.add(ssoStr)
//    }
//    results.toSet
//  }
   
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