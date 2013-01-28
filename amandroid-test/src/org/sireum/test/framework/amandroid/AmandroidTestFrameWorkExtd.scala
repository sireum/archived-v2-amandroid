package org.sireum.test.framework.amandroid


import org.sireum.test.framework._
import org.sireum.pilar.ast._
import org.sireum.pilar.parser._
import org.sireum.util._
import org.sireum.pipeline._
import org.sireum.amandroid.module.Dex2PilarWrapperModule
import org.sireum.core.module.AlirIntraProceduralModule
import java.io._
import java.util.zip.ZipFile
import org.sireum.core.module.ChunkingPilarParserModule
import org.sireum.amandroid.module.PilarAndroidSymbolResolverModule
import org.sireum.amandroid.module.AndroidInterIntraProceduralModule

// sankar introduces the following framework which adds one stage on top of AmandroidParserTestFrameWork 

trait AmandroidTestFrameWorkExtd extends TestFramework { 
  
  //////////////////////////////////////////////////////////////////////////////
  // Implemented Public Methods
  //////////////////////////////////////////////////////////////////////////////

  def Analyzing : this.type = this

  def title(s : String) : this.type = {
    _title = "Case " + casePrefix + ": " + s
    casePrefix = ""
    this
  }

  def file(fileUri : FileResourceUri) =
    AmandroidConfiguration(title, fileUri)

  //////////////////////////////////////////////////////////////////////////////
  // Public Case Classes
  //////////////////////////////////////////////////////////////////////////////

  case class AmandroidConfiguration //
  (title : String, srcs : FileResourceUri) {

    ////////////////////////////////////////////////////////////////////////////
    // Test Constructor
    ////////////////////////////////////////////////////////////////////////////

    test(title) {
      println("####" + title + "#####")
        val f = new File(srcs.toString().substring(5))
        //create directory
        val dirName = f.getName().split("\\.")(0)
        val d = srcs.substring(srcs.indexOf("/"), srcs.lastIndexOf("/")+1)
        val dirc = new File(d + dirName)
        val srcFiles = mlistEmpty[FileResourceUri]
        val graDir = new File(d+dirName+"/graphs")
        if(!dirc.exists()){
          dirc.mkdir()
          graDir.mkdir()
        }
        //deal with apk file
        val apkName = f.getName()
        val apkFile = new ZipFile(f, ZipFile.OPEN_READ)
        val entries = apkFile.entries()
        while(entries.hasMoreElements()){
          val ze = entries.nextElement()
          if(ze.toString().endsWith(".dex")){
            val loadFile = new File(d+ze.getName())
            val ops = new FileOutputStream(d + dirName + "/classes.dex")
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
        srcFiles += FileUtil.toUri(d + dirName + "/classes.dex")
        val job = PipelineJob()
        val options = job.properties
        Dex2PilarWrapperModule.setSrcFiles(options, srcFiles)
        
//        ChunkingPilarParserModule.setSources(options, ilist(Right(FileUtil.toUri(d+dirName+"/classes.pilar"))))
        
        PilarAndroidSymbolResolverModule.setParallel(options, false)
        PilarAndroidSymbolResolverModule.setHasExistingSymbolTable(options, None)
        PilarAndroidSymbolResolverModule.setHasExistingAndroidVirtualMethodTables(options, None)
        
        AndroidInterIntraProceduralModule.setParallel(options, true)
        AndroidInterIntraProceduralModule.setShouldBuildCfg(options, true)
        AndroidInterIntraProceduralModule.setShouldBuildCCfg(options, true)
        AndroidInterIntraProceduralModule.setShouldBuildSCfg(options, true)
        pipeline.compute(job)
        if(job.hasError){
          println("Error present: " + job.hasError)
          job.tags.foreach(f => println(f))
          job.lastStageInfo.tags.foreach(f => println(f))
        }
        val r = AlirIntraProceduralModule.getResult(options)
        
        //1. set graph file name
        //2. set output string
        //3. get each file
        //4. run command
        
        //1. get cfg
        val cfgFile = new File(d + dirName + "/graphs/cfg.dot")
        val outer1 = new FileOutputStream(cfgFile)
        val w1 = new OutputStreamWriter(outer1, "GBK")
        r.foreach { p =>
          val (procedureUri, result) = p
          
          
          
          result.cfg.toDot(w1)
          
          
          //2. get cdg
//          val cdgFile = new File(d + dirName + "/graphs/cdg.dot")
//          val outer2 = new FileOutputStream(cdgFile)
//          val w2 = new OutputStreamWriter(outer2, "GBK")
//          result.cdgOpt.get.toDot(w2)
//          w2.flush()       
//          val str2 = Array("dot", "-Tps", d + dirName + "/graphs/cdg.dot", "-o", d + dirName + "/graphs/cdg.ps")
//          val proc2 = Runtime.getRuntime().exec(str2)
          
          //3. get dfg
//          val dfgFile = new File(d + dirName + "/graphs/dfg.dot")
//          val outer3 = new FileOutputStream(dfgFile)
//          val w3 = new OutputStreamWriter(outer3, "GBK")
//          result.dfgOpt.get.toDot(w3)
//          w3.flush()
//          val str3 = Array("dot", "-Tps", d + dirName + "/graphs/dfg.dot", "-o", d + dirName + "/graphs/dfg.ps")
//          val proc3 = Runtime.getRuntime().exec(str3)
          
          //4. get idg
//          val idgFile = new File(d + dirName + "/graphs/idg.dot")
//          val outer4 = new FileOutputStream(idgFile)
//          val w4 = new OutputStreamWriter(outer4, "GBK")
//          result.idgOpt.get.toDot(w4)
//          w4.flush()
//          val str4 = Array("dot", "-Tps", d + dirName + "/graphs/idg.dot", "-o", d + dirName + "/graphs/idg.ps")
//          val proc4 = Runtime.getRuntime().exec(str4)
        }
        
        w1.flush()
        val str1 = Array("dot", "-Tps2", d + dirName + "/graphs/cfg.dot", "-o", d + dirName + "/graphs/cfg.ps")
        val proc1 = Runtime.getRuntime().exec(str1) 
        println("###############################################")
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Implemented Protected Methods and Fields
  //////////////////////////////////////////////////////////////////////////////

  protected var _title : String = null
  protected var num = 0
  protected def title() = if (_title == null) {
    num += 1
    "Analysis #" + num
  } else _title

  protected val pipeline =
    PipelineConfiguration(
      "dex2PilarAndParseAndProcedureLocationList test pipeline",
      false,
      PipelineStage(
        "dex2pilar stage",
        false,
        Dex2PilarWrapperModule
      ),
      PipelineStage(
        "Chunking pilar parsing stage",
        false,
        ChunkingPilarParserModule
      ),
      PipelineStage(
        "PilarAndroidSymbolResolverModule stage",
        false,
        PilarAndroidSymbolResolverModule
      ),
      PipelineStage(
      "Android InterIntraProcedural Analysis",
      false,
      AndroidInterIntraProceduralModule
      )
    )
}