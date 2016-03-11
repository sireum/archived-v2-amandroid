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
package org.sireum.amandroid.decompile

import org.sireum.util._
import brut.androlib.ApkDecoder
import java.net.URI
import java.io.File
import java.util.logging.Logger
import java.util.logging.LogManager
import brut.androlib.err.CantFindFrameworkResException
import org.sireum.jawa.util.IgnoreException
import org.sireum.jawa.util.MyFileUtil

object AmDecoder {
  final private val TITLE = "AmDecoder"
  /**
   *  Decode apk file and return outputpath
   *  @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
   */
  def decode(sourcePathUri: FileResourceUri, outputUri: FileResourceUri, createFolder: Boolean = true, forceDelete: Boolean = true): FileResourceUri = {
    // make it as quiet mode
    val logger = Logger.getLogger("")
    logger.getHandlers().foreach {
      h =>
        logger.removeHandler(h)
    }
    LogManager.getLogManager().reset()

    val apkFile = FileUtil.toFile(sourcePathUri)
    val outputDir = 
      if(createFolder){
        val dirName = try{apkFile.getName().substring(0, apkFile.getName().lastIndexOf("."))} catch {case e: IndexOutOfBoundsException => apkFile.getName()}
        val newUri = MyFileUtil.appendFileName(outputUri, dirName)
        FileUtil.toFile(newUri)
      } else {
        FileUtil.toFile(outputUri)
      }
    if(outputDir.exists() && !forceDelete) return FileUtil.toUri(outputDir)
    try {
      val decoder = new ApkDecoder
      decoder.setDecodeSources(0x0000) // DECODE_SOURCES_NONE = 0x0000
      decoder.setApkFile(apkFile)
      decoder.setOutDir(outputDir)
      decoder.setForceDelete(true)
      decoder.decode()
    } catch {
      case ie: InterruptedException => throw ie
      case fe: CantFindFrameworkResException =>
        System.err.println(TITLE + ": Can't find framework resources for package of id: " + fe.getPkgId + ". You must install proper framework files, see apk-tool website for more info.")
      case e: Exception =>
        System.err.println(TITLE + ": " + e.getMessage + ". See apk-tool website for more info.")
    }
    FileUtil.toUri(outputDir)
  }
}
