package org.sireum.amandroid.run.test

import org.sireum.util._
import org.sireum.amandroid.parser.ResourceFileParser
import org.sireum.amandroid.parser.ARSCFileParser
import org.sireum.jawa.MessageCenter._

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object Test_run  {
  private final val TITLE = "Test_run"
  
  def main(args: Array[String]): Unit = {
    if(args.size != 1){
      System.err.print("Usage: source_path")
      return
    }
    
    val sourcePath = args(0)
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
    files.foreach{
      file =>
        msg_critical(TITLE, "####" + file + "#####")
        val strs = msetEmpty[String]
      	val rfp = new ResourceFileParser
      	rfp.parseResourceFile(file)
      	strs ++= rfp.getAllStrings
      	val arsc = new ARSCFileParser
      	arsc.parse(file)
      	strs ++= arsc.getGlobalStringPool.map(_._2)
      	println(strs)
  		  System.gc()
      	msg_critical(TITLE, "************************************\n")
    }
  }
}