package org.sireum.amandroid.test.droidBench

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.sireum.jawa.JawaCodeSource
import org.sireum.amandroid.example.interprocedural.InterproceduralExamples
import org.sireum.amandroid.android.libPilarFiles.AndroidLibPilarFiles
import org.sireum.amandroid.alir.AndroidGlobalConfig
import org.sireum.amandroid.test.framework.droidBench.DroidBenchTestFramework
import org.sireum.jawa.alir.LibSideEffectProvider
import org.sireum.util.FileUtil
import org.sireum.jawa.GlobalConfig

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
@RunWith(classOf[JUnitRunner])
class DroidBenchTest extends DroidBenchTestFramework {
  var i = 0
  val androidLibDir = AndroidGlobalConfig.android_lib_dir
	JawaCodeSource.preLoad(FileUtil.toUri(androidLibDir), GlobalConfig.PILAR_FILE_EXT)
//		LibSideEffectProvider.init
		
//		val ints = Set("AndroidSpecific_Library2.apk")
		
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
}