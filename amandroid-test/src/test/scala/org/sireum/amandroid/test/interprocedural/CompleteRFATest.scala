package org.sireum.amandroid.test.interprocedural

import org.sireum.amandroid.test.framework.interprocedural.CompleteRFATestFramework
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.sireum.amandroid.AmandroidCodeSource
import org.sireum.amandroid.example.interprocedural.InterproceduralExamples

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
@RunWith(classOf[JUnitRunner])
class CompleteRFATest extends CompleteRFATestFramework {
  var i = 0
	AmandroidCodeSource.preLoad
//  InterproceduralExamples.testAPKFiles.
//  filter { s => s.endsWith("BiggerWfgNP.apk") }.
//  foreach { fileUri =>
//    Analyzing title fileUri file fileUri
//  }
  InterproceduralExamples.normalAPKFiles.
//  filter { s => s.endsWith("v1-1.apk") }.
  foreach { fileUri =>
//    if(i < 10) i += 1
    Analyzing title fileUri file fileUri
  }
//  InterproceduralExamples.maliciousAPKFiles.
//  filter { s => s.endsWith("0fbf.apk")}.
//  foreach { fileUri =>
////    if(i < 7) i += 1
//    Analyzing title fileUri file fileUri
//  }
//  InterproceduralExamples.benchAPKFiles.
//  filter { s => s.endsWith("ActivityLifecycle1.apk") }.
//  foreach { fileUri =>
//    Analyzing title fileUri file fileUri
//  }
}