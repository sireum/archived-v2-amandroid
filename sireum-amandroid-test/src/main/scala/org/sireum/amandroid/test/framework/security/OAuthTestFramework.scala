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
import org.sireum.jawa.Center
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
  
  var tweeterUrlUser = Set[String]()
  var googleUrlUser = Set[String]()
  var googlesdkUser = Set[String]()
  
  var microsoftUrlUser = Set[String]()
  var linkedinUrlUser = Set[String]()
  var githubUrlUser = Set[String]()
  var dropboxUrlUser = Set[String]()
  
  var webviewUser = Set[String]()
  
  var accessTokenUser = Set[String]()
  
  var oauthUser = Set[String]()
  
  def write = {
    val outputDir = AndroidGlobalConfig.amandroid_home + "/output"
    val appDataDirFile = new File(outputDir + "/oauthStat")
  	if(!appDataDirFile.exists()) appDataDirFile.mkdirs()
  	val out = new PrintWriter(appDataDirFile + "/summary.txt")
    try{
         if(facebookUrlUser.size > 0) {
            out.write("\n facebook url users are : \n")
            facebookUrlUser.foreach{
             srcFileName =>
               out.write(srcFileName + "\n")
           }
         }
         
         if(oldfacebookUser.size > 0) {
            out.write("\n old facebook users are : \n")
            oldfacebookUser.foreach{
             srcFileName =>
               out.write(srcFileName + "\n")
           }
         }
         
         if(newfacebookUser.size > 0) {
            out.write("\n new facebook users are : \n")
            newfacebookUser.foreach{
             srcFileName =>
               out.write(srcFileName + "\n")
           }
         }
         
         if(tweeterUrlUser.size > 0) {
            out.write("\n tweeter url users are : \n")
            tweeterUrlUser.foreach{
             srcFileName =>
              out.write(srcFileName + "\n")
            }
        }
         
         if(googleUrlUser.size > 0) {
            out.write("\n google url users are : \n")
            googleUrlUser.foreach{
             srcFileName =>
              out.write(srcFileName + "\n")
            }
        }

        if(googlesdkUser.size > 0) {
            out.write(" \n google sdk users are : \n")
            googlesdkUser.foreach{
             srcFileName =>
              out.write(srcFileName + "\n")
            }
        }
         
        
        if(webviewUser.size > 0) {
            out.write("\n webView users are : \n")
            webviewUser.foreach{
             srcFileName =>
              out.write(srcFileName + "\n")
            }
        }
        
        if(accessTokenUser.size > 0) {
            out.write("\n access token users are : \n")
            accessTokenUser.foreach{
             srcFileName =>
              out.write(srcFileName + "\n")
            }
        }
        
        if(webviewUser.size > 0 & oldfacebookUser.size > 0) {
            out.write("\n webView-and-old-facebook users are : \n")
            webviewUser.intersect(oldfacebookUser).foreach{
             srcFileName =>
              out.write(srcFileName + "\n")
            }
        }
        
        if(webviewUser.size > 0 & accessTokenUser.size > 0) {
            out.write("\n webView-and-access-token users are : \n")
            webviewUser.intersect(accessTokenUser).foreach{
             srcFileName =>
              out.write(srcFileName + "\n")
            }
        }
        

    } finally {
    	out.close()
    }
  }
  
  override def toString : String = "total: " + total + ", haveResult: " + haveresult + 
  		", facebookurl: " + facebookUrlUser.size +
  		", oldfacebookSdk: " + oldfacebookUser.size +
  		", newfacebookSdk: " + newfacebookUser.size +
  		", twitterurl: " + tweeterUrlUser.size +
  		", googleurl: " + googleUrlUser.size +
  		", googleSdk: " + googlesdkUser.size +
  		", microsoftrul: " + microsoftUrlUser.size +
  		", linkedinurl: " + linkedinUrlUser.size +
  		", githuburl: " + githubUrlUser.size +
  		", dropboxurl: " + dropboxUrlUser.size +
  		", webviewUser: " + webviewUser.size +
  		", accessTokenUser: " + accessTokenUser.size +
  		", facebookAndTweeterUrl:" + (facebookUrlUser.intersect(tweeterUrlUser)).size +
  		", webViewAndOldfacebookUser:" + (webviewUser.intersect(oldfacebookUser)).size +
  		", webViewAndAceessTokenUser:" + (webviewUser.intersect(accessTokenUser)).size +
  		", OAuthUser:" + oauthUser.size 
}

class OAuthTestFramework extends TestFramework {
  private final val TITLE = "OAuthTestFramework"
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
      try{
    	msg_critical(TITLE, "####" + title + "#####")
    	OAuthCounter.total += 1
    	// before starting the analysis of the current app, first init
    	AndroidGlobalConfig.initJawaAlirInfoProvider
    	
    	val srcFile = new File(new URI(srcRes))
    	val dexFile = APKFileResolver.getDexFile(srcRes, FileUtil.toUri(srcFile.getParentFile()))
    	
    	// convert the dex file to the "pilar" form
    	val pilarFileUri = Dex2PilarConverter.convert(dexFile)
  		AndroidRFAConfig.setupCenter
    	//store the app's pilar code in AmandroidCodeSource which is organized record by record.
    	JawaCodeSource.load(pilarFileUri, GlobalConfig.PILAR_FILE_EXT, AndroidLibraryAPISummary)
    	
    	
    	(JawaCodeSource.getAppRecordsCodes).foreach{
    	  case (name, code) => 	    
    	    if(code.contains("m.facebook.com")){  // m.facebook.com/dialog/oauth
    	      System.err.println("Facebook url in App code record " + name)
    	      OAuthCounter.facebookUrlUser += srcRes
    	      OAuthCounter.oauthUser += srcRes
    	    }
    	    if(code.contains("/Facebook;.authorize:") || // ("Lcom/facebook/android/Facebook;.authorize:")
    	        code.contains("/Facebook;.dialog:")){
    	      System.err.println("old facebook sdk in App code record " + name)
    	      OAuthCounter.oldfacebookUser += srcRes
    	      OAuthCounter.oauthUser += srcRes
    	    }
    	    if(code.contains("com.facebook.Session")){
    	      System.err.println("new facebook sdk in App code record " + name)
    	      OAuthCounter.newfacebookUser += srcRes
    	      OAuthCounter.oauthUser += srcRes
    	    }
    	    
    	    if(code.contains("api.twitter.com/oauth")){
    	      System.err.println("Twitter url in App code record " + name)
    	      OAuthCounter.tweeterUrlUser += srcRes
    	      OAuthCounter.oauthUser += srcRes
    	    }
    	    if(code.contains("accounts.google.com")){
    	      System.err.println("Google url in App code record " + name)
    	      OAuthCounter.googleUrlUser += srcRes
    	      OAuthCounter.oauthUser += srcRes
    	    }
    	    if(code.contains("Lcom/google/android/gms/plus/PlusClient;.connect:") ||
    	        code.contains("Lcom/google/android/gms/auth/GoogleAuthUtil;.getToken:")){
    	      System.err.println("Google sdk in App code record " + name)
    	      OAuthCounter.googlesdkUser += srcRes
    	      OAuthCounter.oauthUser += srcRes
    	    }
    	    
    	    if(code.contains(".live.com")){ // login.live.com
    	      System.err.println("live.com url in App code record " + name)
    	      OAuthCounter.microsoftUrlUser += srcRes
    	      OAuthCounter.oauthUser += srcRes
    	    }
    	    if(code.contains("linkedin.com")){  // linkedin.com/uas/oauth
    	      System.err.println("Linkedin url in App code record " + name)
    	      OAuthCounter.linkedinUrlUser += srcRes
    	      OAuthCounter.oauthUser += srcRes
    	    }
    	    if(code.contains("github.com")){  // github.com/login
    	      System.err.println("Github urlin App code record " + name)
    	      OAuthCounter.githubUrlUser += srcRes
    	      OAuthCounter.oauthUser += srcRes
    	    }
    	    if(code.contains("dropbox.com")){  // dropbox.com/1/oauth2
    	      System.err.println("Dropbox url in App code record " + name)
    	      OAuthCounter.dropboxUrlUser += srcRes
    	      OAuthCounter.oauthUser += srcRes
    	    }
    	    if(code.contains(".loadUrl:(Ljava/lang/String;")){
    	      System.err.println("Webview in App code record " + name)
    	      OAuthCounter.webviewUser += srcRes
    	    }
    	    
    	    if(code.contains("access_token")){
    	      System.err.println("access_token in App code record " + name)
    	      OAuthCounter.accessTokenUser += srcRes
    	    }
    	    
//    	    if(code.contains("client_id")){
//    	      System.err.println("client_id found in App code record " + name)
//    	    }
//    	    if(code.contains("redirect_uri")){
//    	      System.err.println("redirect_uri found in App code record " + name)
//    	    }
//    	    if(code.contains("response_type")){
//    	      System.err.println("response_type found in App code record " + name)
//    	    }
//    	    if(code.contains("access_token")){
//    	      System.err.println("access_token found in App code record " + name)
//    	    }
    	}
    	
    	
    	(JawaCodeSource.getAppUsingLibraryRecordsCodes).foreach{
    	  case (name, code) =>
    	    if(code.contains("m.facebook.com")){
    	      System.err.println("Facebook url in App Using Lib:" + name)
    	      OAuthCounter.facebookUrlUser += srcRes
    	      OAuthCounter.oauthUser += srcRes
    	    }
    	    if(code.contains("Lcom/facebook/android/Facebook;.authorize:") ||
    	        code.contains("Lcom/facebook/android/Facebook;.dialog:")){
    	      System.err.println("old facebook sdk! in App Using Lib:" + name)
    	      OAuthCounter.oldfacebookUser += srcRes
    	      OAuthCounter.oauthUser += srcRes
    	    }
    	    if(code.contains("com.facebook.Session")){
    	      System.err.println("new facebook sdk! in App Using Lib:" + name)
    	      OAuthCounter.newfacebookUser += srcRes
    	      OAuthCounter.oauthUser += srcRes
    	    }
    	    
    	    if(code.contains("api.twitter.com")){
    	      System.err.println("Twitter url! in App Using Lib:" + name)
    	      OAuthCounter.tweeterUrlUser += srcRes
    	      OAuthCounter.oauthUser += srcRes
    	    }
    	    if(code.contains("accounts.google.com")){
    	      System.err.println("Google url! in App Using Lib:" + name)
    	      OAuthCounter.googleUrlUser += srcRes
    	      OAuthCounter.oauthUser += srcRes
    	    }
    	    if(code.contains("Lcom/google/android/gms/plus/PlusClient;.connect:") ||
    	        code.contains("Lcom/google/android/gms/auth/GoogleAuthUtil;.getToken:")){
    	      System.err.println("Google sdk! in App Using Lib:" + name)
    	      OAuthCounter.googlesdkUser += srcRes
    	      OAuthCounter.oauthUser += srcRes
    	    }
    	    
    	    if(code.contains(".live.com")){
    	      System.err.println("live.com url! in App Using Lib:" + name)
    	      OAuthCounter.microsoftUrlUser += srcRes
    	      OAuthCounter.oauthUser += srcRes
    	    }
    	    if(code.contains("linkedin.com")){
    	      System.err.println("Linkedin url! in App Using Lib:" + name)
    	      OAuthCounter.linkedinUrlUser += srcRes
    	      OAuthCounter.oauthUser += srcRes
    	    }
    	    if(code.contains("github.com")){
    	      System.err.println("Github url! in App Using Lib:" + name)
    	      OAuthCounter.githubUrlUser += srcRes
    	      OAuthCounter.oauthUser += srcRes
    	    }
    	    if(code.contains("dropbox.com")){
    	      System.err.println("Dropbox url! in App Using Lib:" + name)
    	      OAuthCounter.dropboxUrlUser += srcRes
    	      OAuthCounter.oauthUser += srcRes
    	    }
    	    if(code.contains(".loadUrl:(Ljava/lang/String;")){
    	      System.err.println("Webview! in App Using Lib:" + name)
    	      OAuthCounter.webviewUser += srcRes
    	    }
    	    
    	     if(code.contains("access_token")){
    	      System.err.println("access_token in App code record " + name)
    	      OAuthCounter.accessTokenUser += srcRes
    	    }
    	    
//    	    if(code.contains("client_id")){
//    	      System.err.println("client_id found! in App Using Lib:" + name)
//    	    }
//    	    if(code.contains("redirect_uri")){
//    	      System.err.println("redirect_uri found! in App Using Lib:" + name)
//    	    }
//    	    if(code.contains("response_type")){
//    	      System.err.println("response_type found! in App Using Lib:" + name)
//    	    }
//    	    if(code.contains("access_token")){
//    	      System.err.println("access_token found! in App Using Lib:" + name)
//    	    }
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
			//OAuthCounter.write
	    	msg_critical(TITLE, OAuthCounter.toString)
	    	msg_critical(TITLE, "************************************\n")
      	}
    }
  }

  protected var _title : String = null
  protected var num = 0
  protected def title() = if (_title == null) {
    num += 1
    "Analysis #" + num
  } else _title
}