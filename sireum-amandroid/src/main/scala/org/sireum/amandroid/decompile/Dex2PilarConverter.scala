/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.decompile

import java.io._
import org.sireum.util._
import java.net.URI
import org.sireum.amandroid.AndroidGlobalConfig
import org.sireum.jawa.util.OsUtils
import org.sireum.amandroid.dedex.PilarDeDex
import org.sireum.jawa.ObjectType

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object Dex2PilarConverter {
  def convert(f: FileResourceUri, out: FileResourceUri, dpsuri: Option[FileResourceUri], recordFilter: (ObjectType => Boolean), dexLog: Boolean, debugMode: Boolean, forceDelete: Boolean): FileResourceUri = {
    if(!forceDelete && FileUtil.toFile(out).exists()) return out
    ConverterUtil.cleanDir(out)
    try {
      val pdd = new PilarDeDex
      pdd.decompile(f, Some(out), dpsuri, recordFilter, dexLog, debugMode)
    } catch {
      case ex: Exception =>
        System.err.println("Given file is not a decompilable file: " + f)
    }
    out
  }
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object ConverterUtil {

  def copy(srcUri: FileResourceUri, destUri: FileResourceUri) {
      def copyFile(f: File) {
        try {
          val fin = new FileInputStream(f)
          val dest = new File(new File(new URI(destUri)), f.getName())
          val fout = new FileOutputStream(dest)
          val buffer = new Array[Byte](1024)
          var bytesRead = fin.read(buffer)
          while (bytesRead > 0) {
            fout.write(buffer, 0, bytesRead)
            bytesRead = fin.read(buffer)
          }
          fin.close
          fout.close
        } catch {
          case e: Exception =>
            e.printStackTrace()
        }
      }

    val src = new File(new URI(srcUri))
    val dest = new File(new URI(destUri))

    if (src.exists() && src.isDirectory()) {
      src.listFiles().foreach { f =>
        if (f.isFile()) {
          copyFile(f)
        }
      }
    }
  }

  def cleanDir(dirUri: FileResourceUri) {
    val dir = new File(new URI(dirUri))
    if (dir.exists)
      dir.listFiles.foreach { f =>
        if (f.isDirectory()) {
          cleanDir(f.getAbsoluteFile.toURI.toASCIIString)
        }
        f.delete()
      }
  }
}