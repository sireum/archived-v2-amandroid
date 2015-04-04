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

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object Dex2PilarConverter {
  var dex2pilarFile = new File(System.getenv(AndroidGlobalConfig.SIREUM_HOME) + "/apps/amandroid/bin/newdex2pilar")
  if(!dex2pilarFile.exists()){
    dex2pilarFile = new File(AndroidGlobalConfig.android_dex2pilar_dir + "/newdex2pilar")
    if(!dex2pilarFile.exists()) throw new RuntimeException("Could not find dex2pilar from: " + AndroidGlobalConfig.android_dex2pilar_dir)
  }
  
	val dexdumputil = Util(dex2pilarFile)
	
	def convert(f : FileResourceUri, out : FileResourceUri) : FileResourceUri = {
	  if (f.endsWith("apk") || f.endsWith("dex") || f.endsWith("odex")) {
      val input = new URI(f)
      val output = new URI(out)
      val args = ilist("/bin/bash", "-c",
        dexdumputil.dexdump.getAbsolutePath() + " -d -f -h -p " + "-o " + output.getPath + " " + input.getPath())
      val clOutput = new Exec().run(200000, args, None, None)  // check last argument
//      println(clOutput)
      out // check if little type mismatch
    } else throw new RuntimeException("Given file is not a decompilable file: " + f)
	}
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
case class Util(dexdump : File) {

  def copy(srcUri : FileResourceUri, destUri : FileResourceUri) {
      def copyFile(f : File) {
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
          case e : Exception =>
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

  def cleanDir(dirUri : FileResourceUri) {
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