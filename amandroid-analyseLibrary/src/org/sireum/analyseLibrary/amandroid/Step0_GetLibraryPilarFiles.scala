package org.sireum.analyseLibrary.amandroid

import java.net.URI
import org.sireum.util._
import org.sireum.androidLibraryFile.amandroid.AmandroidAndroidLibraryFiles
import java.io.File
import java.util.zip.ZipFile
import java.io.FileOutputStream
import org.sireum.amandroid.util.Util
import org.sireum.amandroid.util.Dex2PilarConverter

object Step0_GetLibraryPilarFiles {
  def main(args: Array[String]) {
  	val waittime = 200000
  	val srcFiles = mlistEmpty[FileResourceUri]
  	println(AmandroidAndroidLibraryFiles.DEX_MODEL_DIR_URI)
  	AmandroidAndroidLibraryFiles.dexModelFiles.foreach{
  	  s=>
        //deal with jar file
  	    val f = new File(s.toString().substring(5))
  	    //create directory
        val nameArray = f.getName().split("\\.")
        var dirName : String = ""
        for(i <- 0 until nameArray.length-1){
          dirName += nameArray(i)
        }
        val d = s.substring(s.indexOf("/"), s.lastIndexOf("/")+1)
        /*for start analysis from jar file*/
        val pilarDir = new File(d+"/pilar")
        if(!pilarDir.exists()){
          pilarDir.mkdir()
        }
        val apkName = f.getName()
        val apkFile = new ZipFile(f, ZipFile.OPEN_READ)
        val entries = apkFile.entries()
        while(entries.hasMoreElements()){
          val ze = entries.nextElement()
          if(ze.toString().endsWith(".dex")){
            val loadFile = new File(d+ze.getName())
            val ops = new FileOutputStream(d + "pilar/" + dirName + ".dex")
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
        srcFiles += FileUtil.toUri(d + "pilar/" + dirName + ".dex")
  	}
		srcFiles.foreach(s => {
	    Dex2PilarConverter.convert(s)
	  })
	  println("DONE!")
  }
  
}