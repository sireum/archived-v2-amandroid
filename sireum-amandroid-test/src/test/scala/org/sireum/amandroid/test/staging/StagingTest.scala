package org.sireum.amandroid.test.staging

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.sireum.amandroid.test.framework.droidBench.StagingTestFramework
import org.sireum.amandroid.alir.AndroidGlobalConfig
import org.sireum.jawa.JawaCodeSource
import org.sireum.jawa.alir.LibSideEffectProvider
import org.sireum.amandroid.android.libPilarFiles.AndroidLibPilarFiles
import org.sireum.amandroid.example.interprocedural.InterproceduralExamples
import org.sireum.util.FileUtil
import org.sireum.jawa.GlobalConfig


/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
@RunWith(classOf[JUnitRunner])
class StagingTest extends StagingTestFramework {
  var i = 0
  val androidLibDir = System.getenv(AndroidGlobalConfig.ANDROID_LIB_DIR)
  if(androidLibDir != null){
		JawaCodeSource.preLoad(FileUtil.toUri(androidLibDir), GlobalConfig.PILAR_FILE_EXT)
		
		LibSideEffectProvider.init
		
	  InterproceduralExamples.testAPKFiles.
	  filter { s => s.endsWith("14c03cd13616b0e5c34a5b864e1fac44.apk") }.
	  foreach { resfile =>
	    Analyzing title resfile file resfile
	  }
		
//		InterproceduralExamples.randomArborFiles.
////	  filter { s => s.endsWith("com.dropbox.android.apk") }.
//	  foreach { resfile =>
//	    Analyzing title resfile file resfile
//	  }
//		
		
//	  InterproceduralExamples.popularAPKFiles.
////	  filter { s => s.contains("la.droid.qr.apk") }.
//	  foreach { resfile =>
//	    if(i > 500) 
//	    Analyzing title resfile file resfile
//	    i+=1
//	  }
//		InterproceduralExamples.testFiles.
////	  filter { s => s.endsWith("acctsvcs.us.apk")}.
//	  foreach { resfile =>
////	    if(i < 10) 
//	    Analyzing title resfile file resfile
////	    i+=1
//	  }
//	  InterproceduralExamples.randomAPKFiles.
////	  filter { s => s.endsWith("enterprise.dmagent.apk") }.
//	  foreach { resfile =>
////	    if(i < 89) i += 1
//	    //if(resfile.endsWith("app.kazoebito.com.apk"))
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
//	    Analyzing title resfile file resfile
////	    i+=1
//	  }
//	  InterproceduralExamples.benchAPKFiles.
//	  filter { s => s.endsWith("PrivateDataLeak2.apk") }.
//	  foreach { fileUri =>
//	    Analyzing title fileUri file fileUri
//	  }
  } else {
    System.err.println("Does not have env var: " + AndroidGlobalConfig.ANDROID_LIB_DIR)
  }
}