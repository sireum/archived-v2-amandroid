package org.sireum.amandroid.test.lwparser

import org.sireum.util._
import org.sireum.amandroid.example.lwparser.LightWeightParserExamples
import org.sireum.pipeline._
import java.io.PrintWriter
import java.util.zip.GZIPInputStream
import java.io.File
import java.io.FileInputStream
import org.sireum.amandroid.Transform
import org.sireum.amandroid.AmandroidCodeSource
import org.sireum.amandroid.GlobalConfig
import org.sireum.amandroid.Mode

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object TransformTest extends ImplicitLogging {

  def main(args: Array[String]) {
    GlobalConfig.libFileDir = LightWeightParserExamples.PILAR_MODEL_DIR_URI
    GlobalConfig.mode = Mode.APP_ONLY
    AmandroidCodeSource.preLoad
    val codes = AmandroidCodeSource.getLibraryRecordsCodes.values.toSet
    val transRes = Transform.getIntraProcedureResult(codes)
    println(transRes)
  }
  
}