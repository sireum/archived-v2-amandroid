package org.sireum.amandroid.test.interprocedural

import org.sireum.amandroid.test.framework.interprocedural.CompleteRFATestFramework
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.sireum.amandroid.AmandroidCodeSource
import org.sireum.amandroid.example.interprocedural.InterproceduralExamples
import java.io.PrintWriter
import org.sireum.amandroid.android.libPilarFiles.AndroidLibPilarFiles

object Counter {
  var total = 0
  var oversize = 0
  var haveresult = 0
  override def toString : String = "total: " + total + ", oversize: " + oversize + ", haveResult: " + haveresult
}

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
//  InterproceduralExamples.normalAPKFiles.
//  filter { s => s.endsWith("android-1.apk") }.
//  foreach { fileUri =>
////    if(i < 37) i += 1
//    Analyzing title fileUri file fileUri
//  }
  InterproceduralExamples.maliciousAPKRets.
//  filter { s => s.name.endsWith("86add.apk")}.
  foreach { resRet =>
//    if(i < 7) i += 1
    Analyzing title resRet.name file resRet
  }
//  InterproceduralExamples.maliciousArborUrls.
////  filter { s => s.getPath().endsWith("0a29be.apk")}.
//  foreach { fileUri =>
////    if(i > 69) 
//    Analyzing title fileUri file fileUri
////    i+=1
//  }
//  InterproceduralExamples.benchAPKFiles.
////  filter { s => s.endsWith("IntentSink2.apk") }.
//  foreach { fileUri =>
//    Analyzing title fileUri file fileUri
//  }
}