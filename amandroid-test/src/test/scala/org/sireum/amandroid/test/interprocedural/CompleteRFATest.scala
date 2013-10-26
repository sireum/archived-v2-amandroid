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
	AmandroidCodeSource.preLoad
  InterproceduralExamples.testAPKFiles.
  filter { s => s.endsWith("GlobalRDATest.apk") }.
  foreach { fileUri =>
    Analyzing title fileUri file fileUri
  }
//  InterproceduralExamples.ofgNormalAPKFiles.
////  filter { s => s.endsWith("BiggerWfgNP.apk") }.
//  foreach { fileUri =>
//    Analyzing title fileUri file fileUri
//  }
//  InterproceduralExamples.maliciousAPKFiles.
//  filter { s => !s.endsWith("b90b.apk") || !s.endsWith("2fdc.apk")}.
//  foreach { fileUri =>
//    Analyzing title fileUri file fileUri
//  }
//  InterproceduralExamples.benchAPKFiles.
//  filter { s => s.endsWith("ActivityCommunication1.apk") }.
//  foreach { fileUri =>
//    Analyzing title fileUri file fileUri
//  }
}