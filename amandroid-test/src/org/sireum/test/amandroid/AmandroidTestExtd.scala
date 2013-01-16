package org.sireum.test.amandroid


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.sireum.test.framework.amandroid.AmandroidParserTestFrameWorkExtd
import org.sireum.dexexample.amandroid.AmandroidExamples


@RunWith(classOf[JUnitRunner])
class AmandroidParserTestExtd extends AmandroidParserTestFrameWorkExtd{
  AmandroidExamples.dexModelFiles.
  foreach{fileUri=>
    if(fileUri.indexOf("2d4865") > 0)
    Analyzing title fileUri file fileUri
  }
}