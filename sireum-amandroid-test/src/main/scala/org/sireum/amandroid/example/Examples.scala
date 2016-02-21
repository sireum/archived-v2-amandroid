/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Main Contributors:
 *    Fengguo Wei - Argus Lab @ University of South Florida
 *    Sankardas Roy - Bowling Green State University
 *    
 * Contributors:
 *    Robby - Santos Lab @ Kansas State University
 *    Wu Zhou - Fireeye
 *    Fengchi Lin - Chinese People's Public Security University
 ******************************************************************************/
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
