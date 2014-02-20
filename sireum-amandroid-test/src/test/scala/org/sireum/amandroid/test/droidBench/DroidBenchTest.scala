package org.sireum.amandroid.test.droidBench

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.sireum.jawa.JawaCodeSource
import org.sireum.amandroid.example.interprocedural.InterproceduralExamples
import org.sireum.amandroid.android.libPilarFiles.AndroidLibPilarFiles
import org.sireum.amandroid.alir.AndroidGlobalConfig
import org.sireum.amandroid.test.framework.droidBench.DroidBenchTestFramework
import org.sireum.jawa.alir.LibSideEffectProvider

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
@RunWith(classOf[JUnitRunner])
class DroidBenchTest extends DroidBenchTestFramework {
  var i = 0
  val androidLibDir = System.getenv(AndroidGlobalConfig.ANDROID_LIB_DIR)
  if(androidLibDir != null){
		JawaCodeSource.preLoad(AndroidLibPilarFiles.pilarModelFiles(androidLibDir).toSet)
		LibSideEffectProvider.init
		
		val ints = Set("InterComponentCommunication_Explicit1.apk")
		
	  InterproceduralExamples.benchAPKFiles.
//	  filter { s => ints.exists(s.endsWith(_)) }.
	  foreach { fileUri =>
	    Analyzing title fileUri file fileUri
	  }
		
//		InterproceduralExamples.testAPKFiles.
//		filter { s => ints.exists(s.endsWith(_)) }.
//		foreach { fileUri =>
//		  Analyzing title fileUri file fileUri
//		}
		
//		InterproceduralExamples.benchExtendAPKFiles.
////		filter { s => ints.exists(s.endsWith(_)) }.
//		foreach { fileUri =>
//		  Analyzing title fileUri file fileUri
//		}
  } else {
    System.err.println("Does not have env var: " + AndroidGlobalConfig.ANDROID_LIB_DIR)
  }
}