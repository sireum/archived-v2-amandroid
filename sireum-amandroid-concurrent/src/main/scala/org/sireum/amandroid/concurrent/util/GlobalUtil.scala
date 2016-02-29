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
package org.sireum.amandroid.concurrent.util

import org.sireum.jawa.Reporter
import org.sireum.jawa.Global
import org.sireum.util._
import org.sireum.amandroid.AndroidGlobalConfig
import org.sireum.jawa.util.MyFileUtil
import org.sireum.jawa.Constants
import org.sireum.amandroid.util.AndroidLibraryAPISummary

object GlobalUtil {
  def buildGlobal(fileUri: FileResourceUri, reporter: Reporter, outApkUri: FileResourceUri, srcs: ISet[String]): Global = {
    val global = new Global(fileUri, reporter)
    global.setJavaLib(AndroidGlobalConfig.lib_files)
    srcs foreach {
      src =>
        val fileUri = MyFileUtil.appendFileName(outApkUri, src)
        if(FileUtil.toFile(fileUri).exists()) {
          //store the app's pilar code in AmandroidCodeSource which is organized class by class.
          global.load(fileUri, Constants.PILAR_FILE_EXT, AndroidLibraryAPISummary)
        }
    }
    global
  }
}
