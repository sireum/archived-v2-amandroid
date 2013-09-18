package org.sireum.amandroid.test.interprocedural

import org.junit.runner.RunWith
import org.sireum.amandroid.example.interprocedural.InterproceduralExamples
import org.scalatest.junit.JUnitRunner
import org.sireum.amandroid.test.framework.interprocedural.ReachingFactsAnalysisTestFramework
import org.sireum.amandroid.AmandroidCodeSource

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
@RunWith(classOf[JUnitRunner])
class ReachingFactsAnalysisTest extends ReachingFactsAnalysisTestFramework {
  AmandroidCodeSource.preLoad
  InterproceduralExamples.ofgModelFiles.
    filter { s => s.endsWith("ActivityOncreate.pilar") }.
    foreach { fileUri =>
      Analyzing title fileUri file fileUri
    }
}