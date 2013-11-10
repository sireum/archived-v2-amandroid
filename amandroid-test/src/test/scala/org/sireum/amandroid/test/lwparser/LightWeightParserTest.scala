package org.sireum.amandroid.test.lwparser

import org.junit.runner._
import org.scalatest.junit.JUnitRunner
import org.sireum.util._
import org.sireum.amandroid.example.lwparser.LightWeightParserExamples
import org.sireum.amandroid.test.framework.lwparser.LightWeightParserTestFramework

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
@RunWith(classOf[JUnitRunner])
class LightWeightParserTest extends LightWeightParserTestFramework {

  
  LightWeightParserExamples.pilarModelFiles.
//    filter { s => s.endsWith("Test.pilar") }.
    foreach { fileUri =>
//      Analyzing title fileUri file fileUri
    }
}