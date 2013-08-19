package org.sireum.amandroid.test.framework.lwparser

import org.sireum.test.framework.TestFramework
import org.sireum.pipeline._
import org.sireum.util._
import org.sireum.amandroid.pilar.parser.LightWeightPilarParser

	/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
trait LightWeightParserTestFramework extends TestFramework {

  def Analyzing : this.type = this

  def title(s : String) : this.type = {
    _title = caseString + s
    this
  }

  def model(code : String) =
    InterProceduralConfiguration(title, Left(code))

  def file(fileUri : FileResourceUri) =
    InterProceduralConfiguration(title, Right(fileUri))

  case class InterProceduralConfiguration //
  (title : String,
   srcs : Either[String, FileResourceUri]) {

    test(title) {
    	println("####" + title + "#####")
      LightWeightPilarParser(srcs)
    }
  }

  protected var _title : String = null
  protected var num = 0
  protected def title() = if (_title == null) {
    num += 1
    "Analysis #" + num
  } else _title

}