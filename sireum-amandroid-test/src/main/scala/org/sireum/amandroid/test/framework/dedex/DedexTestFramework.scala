/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
package org.sireum.amandroid.test.framework.dedex

import org.sireum.util.FileResourceUri
import org.sireum.amandroid.test.framework.TestFramework
import java.io.RandomAccessFile
import org.sireum.util.FileUtil
import java.io.PrintStream
import org.sireum.amandroid.dedex.PilarDeDex

/**
 * @author fgwei
 */
class DedexTestFramework extends TestFramework {
  final val TITLE = "DedexTestFramework"
  
  def Analyzing: this.type = this

  def title(s: String): this.type = {
    _title = caseString + s
    this
  }

  def file(fileRes: FileResourceUri) =
    InterProceduralConfiguration(title, fileRes)

  case class InterProceduralConfiguration //
  (title: String,
   srcRes: FileResourceUri) {

    test(title) {
      println("#####" + title + "#####")
      val pdd = new PilarDeDex
      val dpsuri = FileUtil.toUri("/Users/fgwei/Downloads/dedexer/system/deps")
      pdd.decompile(srcRes, None, Some(dpsuri), _ => true, false, true)
    }
  }

  protected var _title: String = null
  protected var num = 0
  protected def title() = if (_title == null) {
    num += 1
    "Analysis #" + num
  } else _title
}
