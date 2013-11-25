package org.sireum.amandroid.test.interprocedural

import org.sireum.amandroid.test.framework.interprocedural.CompleteRFATestFramework
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.sireum.amandroid.AmandroidCodeSource
import org.sireum.amandroid.example.interprocedural.InterproceduralExamples
import org.sireum.amandroid.android.libPilarFiles.AndroidLibPilarFiles

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
@RunWith(classOf[JUnitRunner])
class CompleteRFATest extends CompleteRFATestFramework {
  var i =0
	AmandroidCodeSource.preLoad(AndroidLibPilarFiles.pilarInputStreams)
  InterproceduralExamples.testAPKRets.
  filter { s => s.name.endsWith("LocationFlow2.apk") }.
  foreach { resRet =>
    Analyzing title resRet.name file resRet
  }
//  InterproceduralExamples.normalAPKRets.
////  filter { s => s.name.endsWith("android-1.apk") }.
//  foreach { resRet =>
////    if(i < 37) i += 1
//    Analyzing title resRet.name file resRet
//  }
//  InterproceduralExamples.maliciousAPKRets.
////  filter { s => s.name.endsWith("86add.apk")}.
//  foreach { resRet =>
////    if(i < 7) i += 1
//    Analyzing title resRet.name file resRet
//  }
//  InterproceduralExamples.maliciousArborRets.
//  filter { s => s.name.endsWith("77cebd.apk")}.
//  foreach { resRet =>
////    if(i > 69) 
//    Analyzing title resRet.name file resRet
////    i+=1
//  }
//  InterproceduralExamples.benchAPKFiles.
////  filter { s => s.endsWith("IntentSink2.apk") }.
//  foreach { fileUri =>
//    Analyzing title fileUri file fileUri
//  }
}