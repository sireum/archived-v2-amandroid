package org.sireum.amandroid.util

import org.sireum.util.FileResourceUri
import org.sireum.util.FileUtil
import java.util.zip.ZipFile
import java.io.File
import java.io.FileOutputStream

object APKFileResolver {
  
  /**
   * given an APK file uri, the following method returns the uri of the inner dex file
   */
	def getDexFile(src : FileResourceUri) : FileResourceUri = {
	  val f = new File(src.toString().substring(5))
      //create directory
      val dirName = f.getName().split("\\.")(0)
      val d = src.substring(src.indexOf("/"), src.lastIndexOf("/")+1)
      val dirc = new File(d + dirName)
      if(!dirc.exists()){
        dirc.mkdir()
      }
      //deal with apk file
      val apkName = f.getName()
      val apkFile = new ZipFile(f, ZipFile.OPEN_READ)
      val entries = apkFile.entries()
      while(entries.hasMoreElements()){
        val ze = entries.nextElement()
        if(ze.toString().endsWith(".dex")){
          val loadFile = new File(d+ze.getName())
          val ops = new FileOutputStream(d + dirName + "/" + dirName + ".dex")
          val ips = apkFile.getInputStream(ze)
          var reading = true
          while(reading){
            ips.read() match {
              case -1 => reading = false
              case c => ops.write(c)
            }
          }
          ops.flush()
          ips.close()
        }
      }
    
      FileUtil.toUri(d + dirName + "/" + dirName + ".dex")
	}
}