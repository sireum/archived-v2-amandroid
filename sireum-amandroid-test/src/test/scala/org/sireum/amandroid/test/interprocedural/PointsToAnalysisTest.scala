package org.sireum.amandroid.test.interprocedural

import org.junit.runner._
import org.scalatest.junit.JUnitRunner
import org.sireum.amandroid.example.interprocedural.InterproceduralExamples
import java.util.zip.GZIPInputStream
import java.io._
import org.sireum.util._
import org.sireum.amandroid.xml.AndroidXStream
import org.sireum.amandroid.android.cache.AndroidCacheFile
import org.sireum.amandroid.pilar.parser.LightWeightPilarParser
import org.sireum.amandroid.AmandroidCodeSource
import org.sireum.amandroid.test.framework.interprocedural.PointsToAnalysisTestFramework
import org.sireum.amandroid.android.libPilarFiles.AndroidLibPilarFiles

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */

@RunWith(classOf[JUnitRunner])
class PointsToAnalysisTest extends PointsToAnalysisTestFramework {
  AmandroidCodeSource.preLoad(AndroidLibPilarFiles.pilarInputStreams)
  InterproceduralExamples.modelFiles.
//    filter { s => s.endsWith("/StringLoopTest.pilar") }.
    foreach { fileUri =>
//      Analyzing title fileUri file fileUri
    }
}