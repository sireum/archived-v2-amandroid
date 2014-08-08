package org.sireum.amandroid.test.security

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.sireum.amandroid.alir.AndroidGlobalConfig
import org.sireum.jawa.JawaCodeSource
import org.sireum.amandroid.android.libPilarFiles.AndroidLibPilarFiles
import org.sireum.jawa.alir.LibSideEffectProvider
import org.sireum.amandroid.example.interprocedural.InterproceduralExamples
import org.sireum.amandroid.test.framework.security.OAuthTestFramework
import org.sireum.util.FileUtil
import org.sireum.jawa.GlobalConfig

@RunWith(classOf[JUnitRunner])
class OAuthTest extends OAuthTestFramework {
  var i = 0
  val androidLibDir = AndroidGlobalConfig.android_lib_dir
	//	JawaCodeSource.preLoad(FileUtil.toUri(androidLibDir), GlobalConfig.PILAR_FILE_EXT)
		
		//LibSideEffectProvider.init
		
//	  InterproceduralExamples.testAPKFiles.
//	  filter { s => s.endsWith("Crypto_ECB.apk") }.
//	  foreach { resfile =>
//	    Analyzing title resfile file resfile
//	  }
  InterproceduralExamples.popularAPKFiles.
//	  filter { s => s.contains("mobi.mgeek.TunnyBrowser.apk") }.
  foreach { resfile =>
    //if(i > 800 & i < 810) 
    Analyzing title resfile file resfile
    i+=1
  }
//		InterproceduralExamples.testFiles.
////	  filter { s => s.endsWith("acctsvcs.us.apk")}.
//	  foreach { resfile =>
////	    if(i < 10) 
//	    Analyzing title resfile file resfile
////	    i+=1
//	  }
//	  InterproceduralExamples.randomAPKFiles.
//	  //filter { s => s.endsWith("appinventor.ai_murathankilic.MobilZikir.apk") }.
//	  foreach { resfile =>
////	    if(i < 89) i += 1
////	    if(resfile.endsWith("com.sellbackyourbook.sellback.apk"))
//	    Analyzing title resfile file resfile
//	  }
//	  InterproceduralExamples.normalAPKFiles.
//	//  filter { s => s.name.endsWith("android-1.apk") }.
//	  foreach { resRet =>
//	//    if(i < 37) i += 1
//	    Analyzing title resRet.name file resRet
//	  }
//	  InterproceduralExamples.maliciousAPKRets.
//	//  filter { s => s.name.endsWith("86add.apk")}.
//	  foreach { resRet =>
//	//    if(i < 7) i += 1
//	    Analyzing title resRet.name file resRet
//	  }
//	  InterproceduralExamples.maliciousArborFiles.
////	  filter { s => s.endsWith("6ba36c93.apk")}.
//	  foreach { resfile =>
////	    if(i < 10) 
//	    Analyzing title resfile file (resfile, interPSEA)
////	    i+=1
//	  }
//	  InterproceduralExamples.benchAPKFiles.
//	  filter { s => s.endsWith("PrivateDataLeak2.apk") }.
//	  foreach { fileUri =>
//	    Analyzing title fileUri file fileUri
//	  }
//		InterproceduralExamples.benchExtendAPKFiles.
////		filter { s => s.endsWith("PrivateDataLeak3.apk") }.
//		foreach { fileUri =>
//		  Analyzing title fileUri file fileUri
//		}
}