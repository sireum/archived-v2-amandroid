package org.sireum.analyseLibrary.amandroid

import java.net.URI
import org.sireum.util._
import org.sireum.amandroid.module.Util
import org.sireum.androidLibraryFile.amandroid.AmandroidAndroidLibraryDexFiles

object Step0_GetLibraryPilarFiles {
  def main(args: Array[String]) {
  	val waittime = 200000
		AmandroidAndroidLibraryDexFiles.dexModelFiles.foreach(s => {
		  println(s)
	    if (s.endsWith("dex") || s.endsWith("odex")) {
	      val uri = new URI(s)
	        val args = ilist("/bin/bash", "-c",
	          Util.dexdump.getAbsolutePath() + " -d -f -h -p " + uri.getPath())
	        val clOutput = new Exec().run(waittime, args, None, None)  // check last argument
	    }
	  })
	  println("DONE!")
  }
  
}