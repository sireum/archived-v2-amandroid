/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
package org.sireum.amandroid.example.dex

import org.sireum.jawa.util.MyFileUtil
import org.sireum.jawa.util.ResourceRetriever
import org.sireum.jawa.test.TestConfig
import org.sireum.util._
import org.sireum.amandroid.example.Examples

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object DexExamples extends Examples {
  val DEX_FILE_EXT = ".dex"
  val ODEX_FILE_EXT = ".odex"
  
  val GOOD_MODEL_DIR_URI = sourceDirUri(this.getClass, "./good/model/")
  
  def goodModelFiles = exampleFiles(GOOD_MODEL_DIR_URI) ++ exampleFiles(GOOD_MODEL_DIR_URI, ODEX_FILE_EXT)
  
  override def exampleFiles(dirUri : FileResourceUri,
      ext : String = DEX_FILE_EXT) : ISeq[FileResourceUri] =
    Examples.exampleFiles(dirUri, ext)
}
