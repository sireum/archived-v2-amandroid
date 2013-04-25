package org.sireum.analyseLibrary.framework.amandroid

/*
 * Fengguo Wei, Kansas State University. Implement this library analyse framework.
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>                           
*/

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
import org.sireum.pilar.symbol.SymbolTable
import org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables
import org.sireum.amandroid.xml.AndroidXStream
import org.sireum.amandroid.AndroidSymbolResolver.AndroidSymbolTableProducer
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.pilar.symbol.ProcedureSymbolTableData
import org.sireum.amandroid.AndroidSymbolResolver.AndroidSymbolTableData
import org.sireum.amandroid.module.AndroidInterIntraProcedural
import org.sireum.amandroid.module.AndroidInterIntraProceduralModule
import org.sireum.amandroid.scfg.CompressedControlFlowGraph
import org.sireum.amandroid.cache.AndroidCacheFile

trait AmandroidAnalyseLibraryFrameWork extends TestFramework { 
  
  //////////////////////////////////////////////////////////////////////////////
  // Implemented Public Methods
  //////////////////////////////////////////////////////////////////////////////

  def Analyzing : this.type = this

  def title(s : String) : this.type = {
    _title = caseString + s
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
        val nameArray = f.getName().split("\\.")
        var dirName : String = ""
        for(i <- 0 until nameArray.length-1){
          dirName += nameArray(i)
        }
        val d = srcs.substring(srcs.indexOf("/"), srcs.lastIndexOf("/")+1)
//        val dirc = new File(d + dirName)
        val srcFiles = mlistEmpty[FileResourceUri]
        val resDir = new File(d+"result")
        val ccfgDir = new File(d+"result/ccfgs")
//        if(!dirc.exists()){
//          dirc.mkdir()
//        }
        if(!resDir.exists()){
          resDir.mkdir()
        }
        if(!ccfgDir.exists()){
          ccfgDir.mkdir()
        }
        //deal with jar file
//        val apkName = f.getName()
//        val apkFile = new ZipFile(f, ZipFile.OPEN_READ)
//        val entries = apkFile.entries()
//        while(entries.hasMoreElements()){
//          val ze = entries.nextElement()
//          if(ze.toString().endsWith(".dex")){
//            val loadFile = new File(d+ze.getName())
//            val ops = new FileOutputStream(d + dirName + "/" + dirName + ".dex")
//            val ips = apkFile.getInputStream(ze)
//            var reading = true
//            while(reading){
//              ips.read() match {
//                case -1 => reading = false
//                case c => ops.write(c)
//              }
//            }
//            ops.flush()
//            ips.close()
//          }
//        }
        srcFiles += FileUtil.toUri(d + dirName + "/" + dirName + ".dex")
        
//        val stFile = new File(resDir + "/" + dirName + "SymbolTable.xml")
        
        val xStream = AndroidXStream
        xStream.xstream.alias("SymbolTable", classOf[SymbolTable])
        xStream.xstream.alias("AndroidVirtualMethodTables", classOf[AndroidVirtualMethodTables])
    
        val job = PipelineJob()
        val options = job.properties
//        Dex2PilarWrapperModule.setSrcFiles(options, srcFiles)

        ChunkingPilarParserModule.setSources(options, ilist(Right(FileUtil.toUri(d+ "/" +f.getName()))))
        
        PilarAndroidSymbolResolverModule.setParallel(options, false)
        PilarAndroidSymbolResolverModule.setHasExistingAndroidVirtualMethodTables(options, None)
        
        val aCache = new AndroidCacheFile[ResourceUri]
        val serializer : (Any, OutputStream) --> Unit = {
          case (v, o) =>
            xStream.toXml(v, o)
        }
        aCache.setRootDirectory(ccfgDir + "/")
        aCache.setValueSerializer(serializer, null)
        AndroidInterIntraProceduralModule.setParallel(options, false)
        AndroidInterIntraProceduralModule.setAndroidCache(options, aCache)
        AndroidInterIntraProceduralModule.setShouldBuildCCfg(options, true)
        AndroidInterIntraProceduralModule.setShouldBuildSCfg(options, false)
        AndroidInterIntraProceduralModule.setShouldBuildCSCfg(options, false)
        AndroidInterIntraProceduralModule.setAPIpermOpt(options, None)
        pipeline.compute(job)

        if(job.hasError){
          println("Error present: " + job.hasError)
          job.tags.foreach(f => println(f))
          job.lastStageInfo.tags.foreach(f => println(f))
        }
        
        println("pipeline done!")
          
        val avmtFile = new File(resDir + "/" + dirName + "VirtualMethodTables.xml")
        val outerAVMT = new FileOutputStream(avmtFile)
        val avmt = PilarAndroidSymbolResolverModule.getAndroidVirtualMethodTables(options)
        println("start convert AVMT to xml!")
        xStream.toXml(avmt, outerAVMT)
        
        println("start convert graph to xml!")
        val cCfgs = AndroidInterIntraProceduralModule.getIntraResult_cCfg(options)
        cCfgs.foreach(
          item =>
          {
//            aCache.save[CompressedControlFlowGraph[String]](item._1, item._2)
          }  
        )
        
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
      "Library analyse pipeline",
      false,
//      PipelineStage(
//        "dex2pilar stage",
//        false,
//        Dex2PilarWrapperModule
//      )
      PipelineStage(
        "Chunking pilar parsing stage",
        false,
        ChunkingPilarParserModule
      ),
      PipelineStage(
        "PilarAndroidSymbolResolverModule stage",
        false,
        PilarAndroidSymbolResolverModule
      )
      ,
      PipelineStage(
        "Android InterIntraProcedural Analysis",
        false,
        AndroidInterIntraProceduralModule
      )
    )
    
}