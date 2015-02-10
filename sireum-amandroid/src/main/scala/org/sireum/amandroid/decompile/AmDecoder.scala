/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.decompile

import org.sireum.util._
import brut.androlib.ApkDecoder
import java.net.URI
import java.io.File
import java.util.logging.Logger
import java.util.logging.LogManager
import brut.androlib.err.CantFindFrameworkResException
import org.sireum.jawa.MessageCenter._
import org.sireum.jawa.util.IgnoreException

object AmDecoder {
  final private val TITLE = "AmDecoder"
  /**
   *  Decode apk file and return outputpath
   *  @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
   */
  def decode(sourcePathUri : FileResourceUri, outputUri : FileResourceUri) : FileResourceUri = {
    // make it as quiet mode
    val logger = Logger.getLogger("")
    logger.getHandlers().foreach {
      h =>
        logger.removeHandler(h)
    }
    LogManager.getLogManager().reset();

    val apkFile = FileUtil.toFile(sourcePathUri)
    val dirName = apkFile.getName().substring(0, apkFile.getName().lastIndexOf("."))
    val outputDir = new File(new URI(outputUri + "/" + dirName))
    try{
      val decoder = new ApkDecoder
      decoder.setDecodeSources(0x0000) // DECODE_SOURCES_NONE = 0x0000
      decoder.setApkFile(apkFile)
      decoder.setOutDir(outputDir)
      decoder.setForceDelete(true)
      decoder.decode()
    } catch {
      case fe : CantFindFrameworkResException =>
        err_msg_critical(TITLE, "Can't find framework resources for package of id: " + fe.getPkgId + ". You must install proper framework files, see apk-tool website for more info.")
        throw new IgnoreException
    }
    FileUtil.toUri(outputDir)
  }
}