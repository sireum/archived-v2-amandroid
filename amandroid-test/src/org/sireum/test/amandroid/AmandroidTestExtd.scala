package org.sireum.test.amandroid


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.sireum.dexexample.amandroid.AmandroidExamples
import org.sireum.test.framework.amandroid.AmandroidTestFrameWorkExtd


@RunWith(classOf[JUnitRunner])
class AmandroidTestExtd extends AmandroidTestFrameWorkExtd{
  AmandroidExamples.dexModelFiles.
  foreach{fileUri=>
    if(fileUri.indexOf("0eb4b") > 0)
    Analyzing title fileUri file fileUri
  }
}