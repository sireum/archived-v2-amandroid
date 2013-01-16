package org.sireum.amandroid.module

import org.sireum.pipeline.PipelineJob
import org.sireum.pipeline.PipelineJobModuleInfo
import org.sireum.util._
import java.io.File
import java.net.URI
import java.io.FileInputStream
import java.io.FileOutputStream

object Util {
  val base = new File("/Volumes/android/WORKING_DIRECTORY/out/host/darwin-x86/bin")
  val dexdump = new File(base, "dexdump")
  

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




class Dex2PilarWrapperDef (val job : PipelineJob, info : PipelineJobModuleInfo) extends Dex2PilarWrapperModule {

  val waittime = 200000
  val results = mlistEmpty[FileResourceUri]

  this.srcFiles.foreach(s => {
    if (s.endsWith("dex") || s.endsWith("odex")) {
      val uri = new URI(s)
        val args = ilist("/bin/bash", "-c",
          Util.dexdump.getAbsolutePath() + " -d -f -h -p " + uri.getPath())
        val clOutput = new Exec().run(waittime, args, None, None)  // check last argument
        //println(clOutput) // showing command line output

       
        val t = s.substring(0, s.lastIndexOf('.')).concat(".pilar") // check t = s - .dex + .pilar
       
        results += t // check if little type mismatch
    }
  })
  val x = results.toList
  
  this.sources_=(x.map(f => Right(f.toString())))  // "sources" a misnomer? note that this represents the list of output pilar files
}

