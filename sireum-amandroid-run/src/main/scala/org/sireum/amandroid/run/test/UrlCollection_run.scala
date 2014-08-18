package org.sireum.amandroid.run.test

import org.sireum.util._
import org.sireum.jawa.MessageCenter._
import java.io.File
import java.net.URI
import org.sireum.jawa.util.APKFileResolver
import org.sireum.amandroid.android.decompile.Dex2PilarConverter
import org.sireum.jawa.JawaCodeSource
import org.sireum.jawa.GlobalConfig
import org.sireum.jawa.DefaultLibraryAPISummary
import org.sireum.jawa.util.URLInString
import java.io.PrintWriter

object UrlCollection_run {
  private final val TITLE = "UrlCollection_run"
  
  def main(args: Array[String]): Unit = {
    if(args.size != 1){
      System.err.print("Usage: source_path")
      return
    }
    val outputpath = "/Volumes/ArgusGroup/Stash/outputs/url_collection"
    val outputUri = FileUtil.toUri(outputpath)
    val sourcePath = args(0)
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
    val results : MMap[String, Set[String]] = mmapEmpty
    files.foreach{
      file =>
        msg_critical(TITLE, "####" + file + "#####")
        val srcFile = new File(new URI(file))
      	val dexFile = APKFileResolver.getDexFile(file, FileUtil.toUri(srcFile.getParentFile()))
      	
      	// convert the dex file to the "pilar" form
      	val pilarRootUri = Dex2PilarConverter.convert(dexFile)
      	val pilarFile = new File(new URI(pilarRootUri))
      	//store the app's pilar code in AmandroidCodeSource which is organized record by record.
      	JawaCodeSource.load(pilarRootUri, GlobalConfig.PILAR_FILE_EXT, DefaultLibraryAPISummary)
      	val codes = JawaCodeSource.getAppRecordsCodes
      	val urls : Set[String] =
        	if(!codes.isEmpty){
          	codes.map{
              case (name, code) =>
                URLInString.extract(code)
            }.reduce(iunion[String])
        	} else isetEmpty[String]
        results += (file -> urls)
        val summaryfile = new File(outputpath + "/summary.txt")
        val sos = new PrintWriter(summaryfile)
        try{
          results.foreach{
            case (file, urls) =>
              sos.write(file + ":\n")
              urls.foreach{
                url =>
                  sos.write(url + "\n")
              }
              sos.write("\n\n")
          }
        } finally {
          sos.close()
          JawaCodeSource.clearAppRecordsCodes
  	    	APKFileResolver.deleteOutputs(file, outputUri)
  	    	System.gc
        }
    }
    
  }
}