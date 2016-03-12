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