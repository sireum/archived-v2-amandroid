package org.sireum.amandroid.test.lwparser

import org.sireum.util._
import org.sireum.amandroid.example.lwparser.LightWeightParserExamples
import org.sireum.test.framework.TestFramework
import org.sireum.pipeline._
import java.io.PrintWriter
import java.util.zip.GZIPInputStream
import java.io.File
import java.io.FileInputStream
import org.sireum.amandroid.Transform
import org.sireum.amandroid.AmandroidCodeSource

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object TransformTest extends ImplicitLogging {

  def main(args: Array[String]) {
    AmandroidCodeSource.preLoad
    val codes = AmandroidCodeSource.getRecordsCodes.values.toSet
    val transRes = Transform.getIntraProcedureResult(codes)
    println(transRes)
  }
  
}