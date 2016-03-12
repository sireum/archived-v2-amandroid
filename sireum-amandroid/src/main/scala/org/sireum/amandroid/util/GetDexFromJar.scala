/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
package org.sireum.amandroid.util

import org.sireum.util.FileUtil
import org.sireum.jawa.util.APKFileResolver

/**
 * @author fgwei
 */
object GetDexFromJar {
  def main(args: Array[String]): Unit = {
    val dir = args(0)
    val dirUri = FileUtil.toUri(dir)
    FileUtil.listFiles(dirUri, ".jar", true) foreach {
      jarUri =>
        APKFileResolver.getDexFile(jarUri, dirUri, false)
    }
  }
}
