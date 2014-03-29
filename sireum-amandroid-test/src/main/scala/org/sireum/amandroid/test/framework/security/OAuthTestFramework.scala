package org.sireum.amandroid.test.framework.security

import org.sireum.jawa.test.framework.TestFramework
import org.sireum.util._
import org.sireum.jawa.MessageCenter._
import org.sireum.amandroid.alir.AndroidGlobalConfig
import java.io.File
import org.sireum.jawa.util.APKFileResolver
import java.net.URI
import org.sireum.amandroid.android.decompile.Dex2PilarConverter
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis.AndroidRFAConfig
import org.sireum.jawa.JawaCodeSource
import org.sireum.amandroid.android.util.AndroidLibraryAPISummary
import org.sireum.amandroid.security.apiMisuse.InterestingApiCollector
import org.sireum.jawa.Center
import org.sireum.amandroid.security.apiMisuse.CryptographicMisuse
import org.sireum.amandroid.security.apiMisuse.CryptographicConstants
import org.sireum.amandroid.alir.AndroidConstants
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis.AndroidReachingFactsAnalysisConfig
import org.sireum.jawa.util.Timer
import org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis.AndroidReachingFactsAnalysis
import org.sireum.jawa.ClassLoadManager
import org.sireum.amandroid.alir.AppCenter
import org.sireum.jawa.util.TimeOutException
import org.sireum.jawa.util.IgnoreException
import org.sireum.jawa.alir.interProcedural.InterProceduralDataFlowGraph
import org.sireum.jawa.GlobalConfig
import java.io.PrintWriter

object OAuthCounter {
  var total = 0
  var haveresult = 0
  var facebookUrlUser = Set[String]()
  var oldfacebookUser = Set[String]()
  var newfacebookUser = Set[String]()
  
  var twitterUrlUser = Set[String]()
  var googleUrlUser = Set[String]()
  var googlesdkUser = Set[String]()
  
  var microsoftUrlUser = Set[String]()
  var linkedinUrlUser = Set[String]()
  var githubUrlUser = Set[String]()
  var dropboxUrlUser = Set[String]()
  
  var webviewUser = Set[String]()
  
  def write = {
    val outputDir = System.getenv(AndroidGlobalConfig.ANDROID_OUTPUT_DIR)
  	if(outputDir == null) throw new RuntimeException("Does not have env var: " + AndroidGlobalConfig.ANDROID_OUTPUT_DIR)
  	val appDataDirFile = new File(outputDir + "/LocAndTime")
  	if(!appDataDirFile.exists()) appDataDirFile.mkdirs()
  	val out = new PrintWriter(appDataDirFile + "/LocAndTime.txt")
    try{
//	    locTimeMap.foreach{
//	  	  case (fileName, (loc, time)) =>
//	  	    out.write(loc + ", " + time + "\n")
//	  	}
    } finally {
    	out.close()
    }
  }
  
  override def toString : String = "total: " + total + ", haveResult: " + haveresult + 
  		", facebookurl: " + facebookUrlUser.size +
  		", oldfacebookSdk: " + oldfacebookUser.size +
  		", newfacebookSdk: " + newfacebookUser.size +
  		", twitterurl: " + twitterUrlUser.size +
  		", googleurl: " + googleUrlUser.size +
  		", googleSdk: " + googlesdkUser.size +
  		", microsoftrul: " + microsoftUrlUser.size +
  		", linkedinurl: " + linkedinUrlUser.size +
  		", githuburl: " + githubUrlUser.size +
  		", dropboxurl: " + dropboxUrlUser.size +
  		", webviewUser: " + webviewUser.size
}

class OAuthTestFramework extends TestFramework {
  def Analyzing : this.type = this

  def title(s : String) : this.type = {
    _title = caseString + s
    this
  }

  def file(fileRes : FileResourceUri) =
    InterProceduralConfiguration(title, fileRes)
/**
 * does inter procedural analysis of an app
 * @param src is the uri of the apk file
 */
  case class InterProceduralConfiguration //
  (title : String,
   srcRes : FileResourceUri) {

    test(title) {
    	msg_critical("####" + title + "#####")
    	OAuthCounter.total += 1
    	// before starting the analysis of the current app, first reset the Center which may still hold info (of the resolved records) from the previous analysis
    	AndroidGlobalConfig.initJawaAlirInfoProvider
    	
    	val srcFile = new File(new URI(srcRes))
    	val dexFile = APKFileResolver.getDexFile(srcRes, FileUtil.toUri(srcFile.getParentFile()))
    	
    	// convert the dex file to the "pilar" form
    	val pilarFileUri = Dex2PilarConverter.convert(dexFile)
  		AndroidRFAConfig.setupCenter
    	//store the app's pilar code in AmandroidCodeSource which is organized record by record.
    	JawaCodeSource.load(pilarFileUri, GlobalConfig.PILAR_FILE_EXT, AndroidLibraryAPISummary)
    	
    	(JawaCodeSource.getAppRecordsCodes ++ JawaCodeSource.getAppUsingLibraryRecordsCodes).foreach{
    	  case (name, code) =>
    	    if(code.contains("m.facebook.com/dialog")){
    	      System.err.println("Facebook!")
    	      OAuthCounter.facebookUrlUser += srcRes
    	    }
    	    if(code.contains("Lcom/facebook/android/Facebook;.authorize:") ||
    	        code.contains("Lcom/facebook/android/Facebook;.dialog:")){
    	      System.err.println("old facebook sdk!")
    	      OAuthCounter.oldfacebookUser += srcRes
    	    }
    	    if(code.contains("com.facebook.Session")){
    	      System.err.println("new facebook sdk!")
    	      OAuthCounter.newfacebookUser += srcRes
    	    }
    	    
    	    if(code.contains("api.twitter.com/oauth")){
    	      System.err.println("Twitter url!")
    	      OAuthCounter.twitterUrlUser += srcRes
    	    }
    	    if(code.contains("accounts.google.com")){
    	      System.err.println("Google url!")
    	      OAuthCounter.googleUrlUser += srcRes
    	    }
    	    if(code.contains("Lcom/google/android/gms/plus/PlusClient;.connect:") ||
    	        code.contains("Lcom/google/android/gms/auth/GoogleAuthUtil;.getToken:")){
    	      System.err.println("Google sdk!")
    	      OAuthCounter.googlesdkUser += srcRes
    	    }
    	    
    	    if(code.contains("login.live.com")){
    	      System.err.println("MicroSoft url!")
    	      OAuthCounter.microsoftUrlUser += srcRes
    	    }
    	    if(code.contains("linkedin.com/uas/oauth")){
    	      System.err.println("Linkedin url!")
    	      OAuthCounter.linkedinUrlUser += srcRes
    	    }
    	    if(code.contains("github.com/login")){
    	      System.err.println("Github url!")
    	      OAuthCounter.githubUrlUser += srcRes
    	    }
    	    if(code.contains("dropbox.com/1/oauth2")){
    	      System.err.println("Dropbox url!")
    	      OAuthCounter.dropboxUrlUser += srcRes
    	    }
    	    if(code.contains(".loadUrl:(Ljava/lang/String;")){
    	      System.err.println("Webview!")
    	      OAuthCounter.webviewUser += srcRes
    	    }
    	    
    	    
    	    if(code.contains("client_id")){
    	      System.err.println("client_id found!")
    	    }
    	    if(code.contains("redirect_uri")){
    	      System.err.println("redirect_uri found!")
    	    }
    	    if(code.contains("response_type")){
    	      System.err.println("response_type found!")
    	    }
    	    if(code.contains("access_token")){
    	      System.err.println("access_token found!")
    	    }
    	}
    	
    	Center.reset
    	AppCenter.reset
    	// before starting the analysis of the current app, first clear the previous app's records' code from the AmandroidCodeSource
    	JawaCodeSource.clearAppRecordsCodes
		  System.gc()
    	msg_critical(OAuthCounter.toString)
    	msg_critical("************************************\n")
    }
  }

  protected var _title : String = null
  protected var num = 0
  protected def title() = if (_title == null) {
    num += 1
    "Analysis #" + num
  } else _title
}