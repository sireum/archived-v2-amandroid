/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.run.adlib

import org.sireum.util.FileUtil
import org.sireum.jawa.Center
import org.sireum.amandroid.AppCenter
import org.sireum.jawa.JawaCodeSource
import org.sireum.amandroid.AndroidGlobalConfig
import java.io.File
import java.net.URI
import org.sireum.jawa.util.APKFileResolver
import org.sireum.amandroid.decompile.Dex2PilarConverter
import org.sireum.jawa.GlobalConfig
import org.sireum.jawa.DefaultLibraryAPISummary
import java.io.FileOutputStream
import java.io.PrintWriter

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object AdLibCounting {
  
  object Counter {
    var adfonic : Set[String] = Set()
    var winad : Set[String] = Set() 
    var admarvel : Set[String] = Set() 
    var appmedia : Set[String] = Set() 
    var smartadserver : Set[String] = Set() 
    var immob : Set[String] = Set() 
    var polar : Set[String] = Set() 
    var medialets : Set[String] = Set() 
    var baidu : Set[String] = Set() 
    var sense : Set[String] = Set() 
    var mads : Set[String] = Set() 
    var tapjoy : Set[String] = Set() 
    var innerActive : Set[String] = Set() 
    var zestadz : Set[String] = Set() 
    var admogo : Set[String] = Set() 
    var smaato : Set[String] = Set() 
    var daum : Set[String] = Set() 
    var inmobi : Set[String] = Set() 
    var imobile : Set[String] = Set() 
    var flurry : Set[String] = Set() 
    var youmi : Set[String] = Set() 
    var airad : Set[String] = Set() 
    var mopub : Set[String] = Set() 
    var millennialmedia : Set[String] = Set() 
    var adchina : Set[String] = Set() 
    var mobclix : Set[String] = Set() 
    var adlib_android : Set[String] = Set() 
    var vpon : Set[String] = Set() 
    var mobfox : Set[String] = Set() 
    var casee : Set[String] = Set() 
    var cauly : Set[String] = Set() 
    var adserver : Set[String] = Set() 
    var waps : Set[String] = Set() 
    var pontiflex : Set[String] = Set() 
    var googleads : Set[String] = Set() 
    var adwo : Set[String] = Set() 
    var adwhirl : Set[String] = Set() 
    var domob : Set[String] = Set() 
    var energysource : Set[String] = Set() 
    var madvertise : Set[String] = Set() 
    var madhouse : Set[String] = Set() 
    var qwapi : Set[String] = Set() 
    var mdotm : Set[String] = Set() 
    var guohead : Set[String] = Set() 
    var nexage : Set[String] = Set() 
    var wiyun : Set[String] = Set() 
    var mobisage : Set[String] = Set() 
    var sktelecom : Set[String] = Set() 
    var vdopia : Set[String] = Set() 
    var adlocal : Set[String] = Set() 
    var admob : Set[String] = Set() 
    var greystripe : Set[String] = Set() 
    var uplusad : Set[String] = Set() 
    var adagogo : Set[String] = Set() 
    var jumptap : Set[String] = Set() 
    var everbadge : Set[String] = Set()  
    var microad : Set[String] = Set() 
    var mobgold : Set[String] = Set()
    var yicha : Set[String] = Set() 
    var mobot : Set[String] = Set() 
    var mediba : Set[String] = Set() 
    var moolah : Set[String] = Set() 
    var adknowledge : Set[String] = Set() 
    var transpera : Set[String] = Set() 
    var adview : Set[String] = Set() 
    var adlantis : Set[String] = Set() 
    var airpush : Set[String] = Set() 
    var leadbolt : Set[String] = Set() 
    var wqmobile : Set[String] = Set() 
    var wooboo : Set[String] = Set()
    
    def output = {
      val outputpath = "/Volumes/ArgusGroup/Stash/outputs/adlib"
      val summaryStr : String = 
        "adfonic: " + Counter.adfonic.size + "\n" + 
  	    "winad: " + Counter.winad.size + "\n" +
  	    "admarvel: " + Counter.admarvel.size + "\n" +
  	    "appmedia: " + Counter.appmedia.size + "\n" +
  	    "smartadserver: " + Counter.smartadserver.size + "\n" +
  	    "immob: " + Counter.immob.size + "\n" +
  	    "polar: " + Counter.polar.size + "\n" +
  	    "medialets: " + Counter.medialets.size + "\n" +
  	    "baidu: " + Counter.baidu.size + "\n" +
  	    "sense: " + Counter.sense.size + "\n" +
  	    "mads: " + Counter.mads.size + "\n" +
  	    "tapjoy: " + Counter.tapjoy.size + "\n" +
  	    "innerActive: " + Counter.innerActive.size + "\n" +
  	    "zestadz: " + Counter.zestadz.size + "\n" +
  	    "admogo: " + Counter.admogo.size + "\n" +
  	    "smaato: " + Counter.smaato.size + "\n" +
  	    "daum: " + Counter.daum.size + "\n" +
  	    "inmobi: " + Counter.inmobi.size + "\n" +
  	    "imobile: " + Counter.imobile.size + "\n" +
  	    "flurry: " + Counter.flurry.size + "\n" +
  	    "youmi: " + Counter.youmi.size + "\n" +
  	    "airad: " + Counter.airad.size + "\n" +
  	    "mopub: " + Counter.mopub.size + "\n" +
  	    "millennialmedia: " + Counter.millennialmedia.size + "\n" +
  	    "adchina: " + Counter.adchina.size + "\n" +
  	    "mobclix: " + Counter.mobclix.size + "\n" +
  	    "adlib_android: " + Counter.adlib_android.size + "\n" +
  	    "vpon: " + Counter.vpon.size + "\n" +
  	    "mobfox: " + Counter.mobfox.size + "\n" +
  	    "casee: " + Counter.casee.size + "\n" +
  	    "cauly: " + Counter.cauly.size + "\n" +
  	    "adserver: " + Counter.adserver.size + "\n" +
  	    "waps: " + Counter.waps.size + "\n" +
  	    "pontiflex: " + Counter.pontiflex.size + "\n" +
  	    "googleads: " + Counter.googleads.size + "\n" +
  	    "adwo: " + Counter.adwo.size + "\n" +
  	    "adwhirl: " + Counter.adwhirl.size + "\n" +
  	    "domob: " + Counter.domob.size + "\n" +
  	    "energysource: " + Counter.energysource.size + "\n" +
  	    "madvertise: " + Counter.madvertise.size + "\n" +
  	    "madhouse: " + Counter.madhouse.size + "\n" +
  	    "qwapi: " + Counter.qwapi.size + "\n" +
  	    "mdotm: " + Counter.mdotm.size + "\n" +
  	    "guohead: " + Counter.guohead.size + "\n" +
  	    "nexage: " + Counter.nexage.size + "\n" +
  	    "wiyun: " + Counter.wiyun.size + "\n" +
  	    "mobisage: " + Counter.mobisage.size + "\n" +
  	    "sktelecom: " + Counter.sktelecom.size + "\n" +
  	    "vdopia: " + Counter.vdopia.size + "\n" +
  	    "adlocal: " + Counter.adlocal.size + "\n" +
  	    "admob: " + Counter.admob.size + "\n" +
  	    "greystripe: " + Counter.greystripe.size + "\n" +
  	    "uplusad: " + Counter.uplusad.size + "\n" +
  	    "adagogo: " + Counter.adagogo.size + "\n" +
  	    "jumptap: " + Counter.jumptap.size + "\n" +
  	    "everbadge: " + Counter.everbadge.size + "\n" +
  	    "microad: " + Counter.microad.size + "\n" +
  	    "mobgold: " + Counter.mobgold.size + "\n" +
  	    "yicha: " + Counter.yicha.size + "\n" +
  	    "mobot: " + Counter.mobot.size + "\n" +
  	    "mediba: " + Counter.mediba.size + "\n" +
  	    "moolah: " + Counter.moolah.size + "\n" +
  	    "adknowledge: " + Counter.adknowledge.size + "\n" +
  	    "transpera: " + Counter.transpera.size + "\n" +
  	    "adview: " + Counter.adview.size + "\n" +
  	    "adlantis: " + Counter.adlantis.size + "\n" +
  	    "airpush: " + Counter.airpush.size + "\n" +
  	    "leadbolt: " + Counter.leadbolt.size + "\n" +
  	    "wqmobile: " + Counter.wqmobile.size + "\n" +
  	    "wooboo: " + Counter.wooboo.size + "\n"
  	  
  	  val detailStr : String = 
        "adfonic: \n" + Counter.adfonic + "\n\n" +
  	    "winad: \n" + Counter.winad + "\n\n" +
  	    "admarvel: \n" + Counter.admarvel + "\n\n" +
  	    "appmedia: \n" + Counter.appmedia + "\n\n" +
  	    "smartadserver: \n" + Counter.smartadserver + "\n\n" +
  	    "immob: \n" + Counter.immob + "\n\n" +
  	    "polar: \n" + Counter.polar + "\n\n" +
  	    "medialets: \n" + Counter.medialets + "\n\n" +
  	    "baidu: \n" + Counter.baidu + "\n\n" +
  	    "sense: \n" + Counter.sense + "\n\n" +
  	    "mads: \n" + Counter.mads + "\n\n" +
  	    "tapjoy: \n" + Counter.tapjoy + "\n\n" +
  	    "innerActive: \n" + Counter.innerActive + "\n\n" +
  	    "zestadz: \n" + Counter.zestadz + "\n\n" +
  	    "admogo: \n" + Counter.admogo + "\n\n" +
  	    "smaato: \n" + Counter.smaato + "\n\n" +
  	    "daum: \n" + Counter.daum + "\n\n" +
  	    "inmobi: \n" + Counter.inmobi + "\n\n" +
  	    "imobile: \n" + Counter.imobile + "\n\n" +
  	    "flurry: \n" + Counter.flurry + "\n\n" +
  	    "youmi: \n" + Counter.youmi + "\n\n" +
  	    "airad: \n" + Counter.airad + "\n\n" +
  	    "mopub: \n" + Counter.mopub + "\n\n" +
  	    "millennialmedia: \n" + Counter.millennialmedia + "\n\n" +
  	    "adchina: \n" + Counter.adchina + "\n\n" +
  	    "mobclix: \n" + Counter.mobclix + "\n\n" +
  	    "adlib_android: \n" + Counter.adlib_android + "\n\n" +
  	    "vpon: \n" + Counter.vpon + "\n\n" +
  	    "mobfox: \n" + Counter.mobfox + "\n\n" +
  	    "casee: \n" + Counter.casee + "\n\n" +
  	    "cauly: \n" + Counter.cauly + "\n\n" +
  	    "adserver: \n" + Counter.adserver + "\n\n" +
  	    "waps: \n" + Counter.waps + "\n\n" +
  	    "pontiflex: \n" + Counter.pontiflex + "\n\n" +
  	    "googleads: \n" + Counter.googleads + "\n\n" +
  	    "adwo: \n" + Counter.adwo + "\n\n" +
  	    "adwhirl: \n" + Counter.adwhirl + "\n\n" +
  	    "domob: \n" + Counter.domob + "\n\n" +
  	    "energysource: \n" + Counter.energysource + "\n\n" +
  	    "madvertise: \n" + Counter.madvertise + "\n\n" +
  	    "madhouse: \n" + Counter.madhouse + "\n\n" +
  	    "qwapi: \n" + Counter.qwapi + "\n\n" +
  	    "mdotm: \n" + Counter.mdotm + "\n\n" +
  	    "guohead: \n" + Counter.guohead + "\n\n" +
  	    "nexage: \n" + Counter.nexage + "\n\n" +
  	    "wiyun: \n" + Counter.wiyun + "\n\n" +
  	    "mobisage: \n" + Counter.mobisage + "\n\n" +
  	    "sktelecom: \n" + Counter.sktelecom + "\n\n" +
  	    "vdopia: \n" + Counter.vdopia + "\n\n" +
  	    "adlocal: \n" + Counter.adlocal + "\n\n" +
  	    "admob: \n" + Counter.admob + "\n\n" +
  	    "greystripe: \n" + Counter.greystripe + "\n\n" +
  	    "uplusad: \n" + Counter.uplusad + "\n\n" +
  	    "adagogo: \n" + Counter.adagogo + "\n\n" +
  	    "jumptap: \n" + Counter.jumptap + "\n\n" +
  	    "everbadge: \n" + Counter.everbadge + "\n\n" +
  	    "microad: \n" + Counter.microad + "\n\n" +
  	    "mobgold: \n" + Counter.mobgold + "\n\n" +
  	    "yicha: \n" + Counter.yicha + "\n\n" +
  	    "mobot: \n" + Counter.mobot + "\n\n" +
  	    "mediba: \n" + Counter.mediba + "\n\n" +
  	    "moolah: \n" + Counter.moolah + "\n\n" +
  	    "adknowledge: \n" + Counter.adknowledge + "\n\n" +
  	    "transpera: \n" + Counter.transpera + "\n\n" +
  	    "adview: \n" + Counter.adview + "\n\n" +
  	    "adlantis: \n" + Counter.adlantis + "\n\n" +
  	    "airpush: \n" + Counter.airpush + "\n\n" +
  	    "leadbolt: \n" + Counter.leadbolt + "\n\n" +
  	    "wqmobile: \n" + Counter.wqmobile + "\n\n" +
  	    "wooboo: \n" + Counter.wooboo + "\n\n"
  	    
  	  val summaryfile = new File(outputpath + "/summary.txt")
      val sos = new PrintWriter(summaryfile)
      val detailfile = new File(outputpath + "/detail.txt")
      val dos = new PrintWriter(detailfile)
  	  try{
        sos.write(summaryStr)
        dos.write(detailStr)
  	  } finally {
        sos.close
        dos.close
  	  }
    }
  }
  
  def main(args: Array[String]): Unit = {
    if(args.size != 1){
      System.err.print("Usage: source_path")
      return
    }
    JawaCodeSource.preLoad(FileUtil.toUri(AndroidGlobalConfig.android_lib_dir), GlobalConfig.PILAR_FILE_EXT)
    val outputUri = FileUtil.toUri("/Volumes/ArgusGroup/Stash/outputs/adlib")
    val sourcePath = args(0)
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
    files.foreach{
	    file =>
	      try {
	      println("Analyzing " + file)
	      val apkFile = new File(new URI(file))
	      
	      val dexFileUri = APKFileResolver.getDexFile(file, outputUri)
	      val pilarRootUri = Dex2PilarConverter.convert(dexFileUri, outputUri)
		 
		  	val pilarFile = new File(new URI(pilarRootUri))
	  		
	    	//store the app's pilar code in AmandroidCodeSource which is organized record by record.
	    	JawaCodeSource.load(pilarRootUri, GlobalConfig.PILAR_FILE_EXT, DefaultLibraryAPISummary)
	    	// resolve each record of the app and stores the result in the Center which will be available throughout the analysis.
	    	JawaCodeSource.getAppRecordsCodes.keys foreach{
	    	  k =>
	    	    val rec = Center.resolveRecord(k, Center.ResolveLevel.HIERARCHY)
	    	    val pkg = rec.getPackageName
	    	    if(pkg.contains("adfonic")) Counter.adfonic += file
	    	    if(pkg.contains("winad")) Counter.winad += file
	    	    if(pkg.contains("admarvel")) Counter.admarvel += file
	    	    if(pkg.contains("appmedia")) Counter.appmedia += file
	    	    if(pkg.contains("smartadserver")) Counter.smartadserver += file
	    	    if(pkg.contains("immob")) Counter.immob += file
	    	    if(pkg.contains("polar")) Counter.polar += file
	    	    if(pkg.contains("medialets")) Counter.medialets += file
	    	    if(pkg.contains("baidu")) Counter.baidu += file
	    	    if(pkg.contains("sense")) Counter.sense += file
	    	    if(pkg.contains("mads")) Counter.mads += file
	    	    if(pkg.contains("tapjoy")) Counter.tapjoy += file
	    	    if(pkg.contains("innerActive")) Counter.innerActive += file
	    	    if(pkg.contains("zestadz")) Counter.zestadz += file
	    	    if(pkg.contains("admogo")) Counter.admogo += file
	    	    if(pkg.contains("smaato")) Counter.smaato += file
	    	    if(pkg.contains("daum")) Counter.daum += file
	    	    if(pkg.contains("inmobi")) Counter.inmobi += file
	    	    if(pkg.contains("imobile")) Counter.imobile += file
	    	    if(pkg.contains("flurry")) Counter.flurry += file
	    	    if(pkg.contains("youmi")) Counter.youmi += file
	    	    if(pkg.contains("airad")) Counter.airad += file
	    	    if(pkg.contains("mopub")) Counter.mopub += file
	    	    if(pkg.contains("millennialmedia")) Counter.millennialmedia += file
	    	    if(pkg.contains("adchina")) Counter.adchina += file
	    	    if(pkg.contains("mobclix")) Counter.mobclix += file
	    	    if(pkg.contains("adlib_android")) Counter.adlib_android += file
	    	    if(pkg.contains("vpon")) Counter.vpon += file
	    	    if(pkg.contains("mobfox")) Counter.mobfox += file
	    	    if(pkg.contains("casee")) Counter.casee += file
	    	    if(pkg.contains("cauly")) Counter.cauly += file
	    	    if(pkg.contains("adserver")) Counter.adserver += file
	    	    if(pkg.contains("waps")) Counter.waps += file
	    	    if(pkg.contains("pontiflex")) Counter.pontiflex += file
	    	    if(pkg.contains("googleads")) Counter.googleads += file
	    	    if(pkg.contains("adwo")) Counter.adwo += file
	    	    if(pkg.contains("adwhirl")) Counter.adwhirl += file
	    	    if(pkg.contains("domob")) Counter.domob += file
	    	    if(pkg.contains("energysource")) Counter.energysource += file
	    	    if(pkg.contains("madvertise")) Counter.madvertise += file
	    	    if(pkg.contains("madhouse")) Counter.madhouse += file
	    	    if(pkg.contains("qwapi")) Counter.qwapi += file
	    	    if(pkg.contains("mdotm")) Counter.mdotm += file
	    	    if(pkg.contains("guohead")) Counter.guohead += file
	    	    if(pkg.contains("nexage")) Counter.nexage += file
	    	    if(pkg.contains("wiyun")) Counter.wiyun += file
	    	    if(pkg.contains("mobisage")) Counter.mobisage += file
	    	    if(pkg.contains("sktelecom")) Counter.sktelecom += file
	    	    if(pkg.contains("vdopia")) Counter.vdopia += file
	    	    if(pkg.contains("adlocal")) Counter.adlocal += file
	    	    if(pkg.contains("admob")) Counter.admob += file
	    	    if(pkg.contains("greystripe")) Counter.greystripe += file
	    	    if(pkg.contains("uplusad")) Counter.uplusad += file
	    	    if(pkg.contains("adagogo")) Counter.adagogo += file
	    	    if(pkg.contains("jumptap")) Counter.jumptap += file
	    	    if(pkg.contains("everbadge")) Counter.everbadge += file
	    	    if(pkg.contains("microad")) Counter.microad += file
	    	    if(pkg.contains("mobgold")) Counter.mobgold += file
	    	    if(pkg.contains("yicha")) Counter.yicha += file
	    	    if(pkg.contains("mobot")) Counter.mobot += file
	    	    if(pkg.contains("mediba")) Counter.mediba += file
	    	    if(pkg.contains("moolah")) Counter.moolah += file
	    	    if(pkg.contains("adknowledge")) Counter.adknowledge += file
	    	    if(pkg.contains("transpera")) Counter.transpera += file
	    	    if(pkg.contains("adview")) Counter.adview += file
	    	    if(pkg.contains("adlantis")) Counter.adlantis += file
	    	    if(pkg.contains("airpush")) Counter.airpush += file
	    	    if(pkg.contains("leadbolt")) Counter.leadbolt += file
	    	    if(pkg.contains("wqmobile")) Counter.wqmobile += file
	    	    if(pkg.contains("wooboo")) Counter.wooboo += file
	    	  }
	      } catch {
	        case re : RuntimeException => 
	    	    re.printStackTrace()
	    	  case e : Exception =>
	    	    e.printStackTrace()
	    	} finally {
	    	  Center.reset
		    	AppCenter.reset
		    	JawaCodeSource.clearAppRecordsCodes
		    	Counter.output
		    	APKFileResolver.deleteOutputs(file, outputUri)
		    	System.gc
	    	}
    }
  }
}