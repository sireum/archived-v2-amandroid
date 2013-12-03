package org.sireum.amandroid.test.interprocedural

import org.sireum.amandroid.test.framework.interprocedural.CompleteRFATestFramework
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.sireum.jawa.JawaCodeSource
import org.sireum.amandroid.example.interprocedural.InterproceduralExamples
import org.sireum.amandroid.android.libPilarFiles.AndroidLibPilarFiles
import org.sireum.amandroid.alir.AndroidGlobalConfig

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
@RunWith(classOf[JUnitRunner])
class CompleteRFATest extends CompleteRFATestFramework {
  var i =0
  val androidLibDir = System.getenv(AndroidGlobalConfig.android_lib_dir)
  if(androidLibDir != null){
		JawaCodeSource.preLoad(AndroidLibPilarFiles.pilarModelFiles(androidLibDir).toSet)
	  InterproceduralExamples.testAPKFiles.
	  filter { s => s.endsWith("LocationFlow2.apk") }.
	  foreach { resfile =>
	    Analyzing title resfile file resfile
	  }
//	  InterproceduralExamples.normalAPKRets.
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
//	  InterproceduralExamples.maliciousArborRets.
//	  filter { s => s.name.endsWith("77cebd.apk")}.
//	  foreach { resRet =>
//	//    if(i > 69) 
//	    Analyzing title resRet.name file resRet
//	//    i+=1
//	  }
//	  InterproceduralExamples.benchAPKFiles.
//	//  filter { s => s.endsWith("IntentSink2.apk") }.
//	  foreach { fileUri =>
//	    Analyzing title fileUri file fileUri
//	  }
  } else {
    System.err.println("Does not have env var: " + AndroidGlobalConfig.android_lib_dir)
  }
}