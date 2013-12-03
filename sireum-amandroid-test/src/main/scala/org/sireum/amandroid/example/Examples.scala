package org.sireum.amandroid.example

import org.sireum.util._

/**
 * @author <a href="mailto:robby@k-state.edu">Robby</a>
 */
trait Examples {
  val PILAR_FILE_EXT = ".plr"

  def sourceDirUri(path : String) = { 
    FileUtil.toUri(path)
  }

  def exampleFiles(dirUri : FileResourceUri,
                   ext : String = PILAR_FILE_EXT) : ISeq[FileResourceUri] =
    FileUtil.listFiles(dirUri, ext, true)
}