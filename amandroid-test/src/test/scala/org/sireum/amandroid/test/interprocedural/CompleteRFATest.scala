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
  filter { s => s.endsWith("InfoPassICCTest.apk") }.
  foreach { fileUri =>
    Analyzing title fileUri file fileUri
  }
//  InterproceduralExamples.ofgNormalAPKFiles.
////  filter { s => s.endsWith("BiggerWfgNP.apk") }.
//  foreach { fileUri =>
//    Analyzing title fileUri file fileUri
//  }
//  InterproceduralExamples.ofgMaliciousAPKFiles.
//  filter { s => s.endsWith("0fbf.apk") }.
//  foreach { fileUri =>
//    Analyzing title fileUri file fileUri
//  }
//  InterproceduralExamples.benchAPKFiles.
//  filter { s => s.endsWith("AnonymousClass1.apk") }.
//  foreach { fileUri =>
//    Analyzing title fileUri file fileUri
//  }
}