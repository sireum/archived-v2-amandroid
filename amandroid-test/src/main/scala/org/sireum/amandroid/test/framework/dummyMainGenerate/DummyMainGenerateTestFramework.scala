package org.sireum.amandroid.test.framework.dummyMainGenerate

import org.sireum.test.framework.TestFramework
import org.sireum.util._
import java.io.File
import java.util.zip.ZipFile
import java.io.FileOutputStream
import org.sireum.pipeline.PipelineConfiguration
import org.sireum.pipeline.PipelineStage
import org.sireum.amandroid.module.Dex2PilarWrapperModule
import org.sireum.pipeline.PipelineJob
import org.sireum.amandroid.Center
import org.sireum.amandroid.AmandroidCodeSource
import org.sireum.amandroid.pilar.parser.LightWeightPilarParser
import org.sireum.amandroid.interProcedural.callGraph.CallGraphBuilder
import org.sireum.amandroid.android.appInfo.PrepareApp

trait DummyMainGenerateTestFramework extends TestFramework {

  def Analyzing : this.type = this

  def title(s : String) : this.type = {
    _title = caseString + s
    this
  }

  def file(fileUri : FileResourceUri) =
    Configuration(title, fileUri)

  case class Configuration //
  (title : String,
   src : FileResourceUri) {

    test(title) {
    	println("####" + title + "#####")
    	
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
    
      val dexFile = FileUtil.toUri(d + dirName + "/" + dirName + ".dex")
      
    	val job = PipelineJob()
      val options = job.properties
      Dex2PilarWrapperModule.setSrcFiles(options, Seq(dexFile))
      apkPipeline.compute(job)
    	
      val pilarFile = FileUtil.toUri(d + dirName + "/" + dirName + ".pilar")
      Center.reset
    	AmandroidCodeSource.clearAppRecordsCodes
    	LightWeightPilarParser(Right(pilarFile), AmandroidCodeSource.CodeType.APP)
    	AmandroidCodeSource.getAppRecordsCodes.keys foreach{
    	  k =>
    	    Center.resolveRecord(k, Center.ResolveLevel.BODIES)
    	}
    	val pre = new PrepareApp(f.toString)
		  pre.calculateEntrypoints("")
    	
		  val cg = new CallGraphBuilder().buildAppOnly(Some(pre))
		  
    	println("####End####")
    }
  }

  protected var _title : String = null
  protected var num = 0
  protected def title() = if (_title == null) {
    num += 1
    "Analysis #" + num
  } else _title
  
  
  protected val apkPipeline =
	    PipelineConfiguration(
      "App analysis pipeline",
      false,
      PipelineStage(
        "dex2pilar stage",
        false,
        Dex2PilarWrapperModule
      )
      )
  
}