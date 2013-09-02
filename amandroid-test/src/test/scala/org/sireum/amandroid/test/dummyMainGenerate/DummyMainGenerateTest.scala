package org.sireum.amandroid.test.dummyMainGenerate

import org.junit.runner._
import org.scalatest.junit.JUnitRunner
import org.sireum.amandroid.AmandroidCodeSource
import org.sireum.amandroid.test.framework.dummyMainGenerate.DummyMainGenerateTestFramework
import org.sireum.amandroid.example.dummyMainGenerate.DummyMainGenerateExamples


/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
@RunWith(classOf[JUnitRunner])
class DummyMainGenerateTest extends DummyMainGenerateTestFramework {
  AmandroidCodeSource.preLoad
	DummyMainGenerateExamples.apkModelFiles.
  filter { s => s.endsWith("68ac.apk") }.
  foreach { fileUri =>
    Analyzing title fileUri file fileUri
  }
	
}