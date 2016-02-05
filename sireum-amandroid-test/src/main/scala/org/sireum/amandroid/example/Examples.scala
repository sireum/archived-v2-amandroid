package org.sireum.amandroid.example

import org.sireum.util._

/**
 * @author <a href="mailto:robby@k-state.edu">Robby</a>
 */
object Examples {
  val PILAR_FILE_EXT = ".pilar"

  def exampleFiles(dirUri : FileResourceUri,
                   ext : String = Examples.PILAR_FILE_EXT) : ISeq[FileResourceUri] =
    FileUtil.listFiles(dirUri, ext, true).map(_.
      replace("/bin/", "/src/test/resources/").
      replaceAll("/target/.*/test-classes/", "/src/test/resources/"))

  def sourceDirUri(claz : Class[_], path : String) = {
    FileUtil.fileUri(claz, path)
  }
}

/**
 * @author <a href="mailto:robby@k-state.edu">Robby</a>
 */
trait Examples {
  def sourceDirUri(claz : Class[_], path : String) =
    Examples.sourceDirUri(claz, path)

  def exampleFiles(dirUri : FileResourceUri,
                   ext : String = Examples.PILAR_FILE_EXT) : ISeq[FileResourceUri] =
    Examples.exampleFiles(dirUri, ext)
}