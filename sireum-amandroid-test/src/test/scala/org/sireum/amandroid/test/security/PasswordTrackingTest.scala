package org.sireum.amandroid.test.security

import org.junit.runner.RunWith
import org.sireum.jawa.JawaCodeSource
import org.sireum.amandroid.example.interprocedural.InterproceduralExamples
import org.sireum.amandroid.android.libPilarFiles.AndroidLibPilarFiles
import org.sireum.amandroid.alir.AndroidGlobalConfig
import java.util.zip.GZIPInputStream
import java.io.FileInputStream
import org.sireum.jawa.xml.AndroidXStream
import org.sireum.jawa.alir.interProcedural.sideEffectAnalysis.InterProceduralSideEffectAnalysisResult
import org.scalatest.junit.JUnitRunner
import org.sireum.amandroid.test.framework.security.PasswordTrackingTestFramework

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
@RunWith(classOf[JUnitRunner])
class CompleteRFATest extends PasswordTrackingTestFramework {
  var i = 0
  val androidLibDir = System.getenv(AndroidGlobalConfig.ANDROID_LIB_DIR)
  if(androidLibDir != null){
		JawaCodeSource.preLoad(AndroidLibPilarFiles.pilarModelFiles(androidLibDir).toSet)
		
		val reader = new GZIPInputStream(new FileInputStream("/Volumes/hd/fgwei/Stash/Amandroid/LibSummary/AndroidLibSideEffectResult.xml.zip"))
    val interPSEA = AndroidXStream.fromXml(reader).asInstanceOf[InterProceduralSideEffectAnalysisResult]
    reader.close()
		
//	  InterproceduralExamples.testAPKFiles.
//	  filter { s => s.endsWith("PasswordPassTest.apk") }.
//	  foreach { resfile =>
//	    Analyzing title resfile file (resfile, interPSEA)
//	  }
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
	  InterproceduralExamples.randomAPKFiles.
//	  filter { s => s.endsWith("enterprise.dmagent.apk") }.
	  foreach { resfile =>
//	    if(i < 89) i += 1
	    //if(resfile.endsWith("app.kazoebito.com.apk"))
	    Analyzing title resfile file resfile
	  }
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
	  InterproceduralExamples.maliciousArborFiles.
//	  filter { s => s.endsWith("6ba36c93.apk")}.
	  foreach { resfile =>
//	    if(i < 10) 
	    Analyzing title resfile file resfile
//	    i+=1
	  }
//	  InterproceduralExamples.benchAPKFiles.
//	  filter { s => s.endsWith("PrivateDataLeak2.apk") }.
//	  foreach { fileUri =>
//	    Analyzing title fileUri file fileUri
//	  }
  } else {
    System.err.println("Does not have env var: " + AndroidGlobalConfig.ANDROID_LIB_DIR)
  }
}