package org.sireum.test.amandroid

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.sireum.test.framework.amandroid.AmandroidParserTestFrameWork
import org.sireum.dexexample.amandroid.AmandroidExamples


@RunWith(classOf[JUnitRunner])
class AmandroidParserTest extends AmandroidParserTestFrameWork{
  AmandroidExamples.dexModelFiles.
  foreach{fileUri=>
//    if (fileUri.indexOf("a0d44d6865ff74b7") > 0)
    Analyzing title fileUri file fileUri
  }
}