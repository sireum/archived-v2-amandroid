package org.sireum.amandroid.util

import org.sireum.util.FileResourceUri
import org.sireum.util.FileUtil
import java.util.zip._
import java.io.File
import java.io.FileOutputStream
import java.net.URL
import java.io.InputStream

object APKFileResolver {
  
  /**
   * given an APK file uri, the following method returns the uri of the inner dex file
   */
	def getDexFile(apkName : String, apkret : ResourceRetriever, outputPath : String) : FileResourceUri = {
    //create directory
	  val zipis = new ZipInputStream(apkret.getResourceStream)
    val dirName = apkName.substring(0, apkName.lastIndexOf("."))
    val dirc = new File(outputPath + "/" + dirName)
    if(!dirc.exists()){
      dirc.mkdirs()
    }
	  val ops = new FileOutputStream(dirc + "/" + dirName + ".dex")
    //resolve with apk file
    while(zipis.available() == 1){
      val ze = zipis.getNextEntry()
      if(ze != null)
	      if(ze.getName().endsWith(".dex")){
	        var reading = true
	        while(reading){
	          zipis.read() match {
	            case -1 => reading = false
	            case c => ops.write(c)
	          }
	        }
	      }
    }
	  ops.flush()
	  zipis.close()
    FileUtil.toUri(dirc + "/" + dirName + ".dex")
	}
}