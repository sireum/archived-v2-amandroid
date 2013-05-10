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
import org.sireum.alir.ControlFlowGraph
import org.sireum.amandroid.module.AndroidInterIntraProcedural

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
        val srcFiles = mlistEmpty[FileResourceUri]
        /*for start analysis from jar file*/
        val pilarDir = new File(d + "pilar")
        val resDir = new File(pilarDir+"/result")
        val ccfgDir = new File(resDir+"/ccfgs")
        if(!pilarDir.exists()){
          resDir.mkdir()
        }
        if(!resDir.exists()){
          resDir.mkdir()
        }
        if(!ccfgDir.exists()){
          ccfgDir.mkdir()
        }
        /*end here*/
        /*for start analysis from pilar file*/
//        val resDir = new File(d+"result")
//        if(!resDir.exists()){
//          resDir.mkdir()
//        }

        /*end here*/
        
        val libInfoDir = new File(d+"result/libInfoDataBase")
        if(!libInfoDir.exists()){
          libInfoDir.mkdir()
        }
        
        //deal with jar file
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
//end here        
//        val stFile = new File(resDir + "/" + dirName + "SymbolTable.xml")
        
        val xStream = AndroidXStream
        xStream.xstream.alias("SymbolTable", classOf[SymbolTable])
        xStream.xstream.alias("AndroidVirtualMethodTables", classOf[AndroidVirtualMethodTables])
    
        val job = PipelineJob()
        val options = job.properties
        Dex2PilarWrapperModule.setSrcFiles(options, srcFiles)

        ChunkingPilarParserModule.setSources(options, ilist(Right(FileUtil.toUri(d+ "/" +f.getName()))))
        
        PilarAndroidSymbolResolverModule.setParallel(options, false)
        PilarAndroidSymbolResolverModule.setHasExistingAndroidVirtualMethodTables(options, None)
        
        val aCache = new AndroidCacheFile[ResourceUri]
        val serializer : (Any, OutputStream) --> Unit = {
          case (v, o) =>
            xStream.toXml(v, o)
        }
        aCache.setRootDirectory(libInfoDir + "/")
        aCache.setValueSerializer(serializer, null)
        AndroidInterIntraProceduralModule.setParallel(options, false)
        AndroidInterIntraProceduralModule.setAndroidCache(options, Some(aCache))
        AndroidInterIntraProceduralModule.setShouldBuildCfg(options, true)
        AndroidInterIntraProceduralModule.setShouldBuildRda(options, true)
        AndroidInterIntraProceduralModule.setShouldPreprocessOfg(options, true)
        AndroidInterIntraProceduralModule.setShouldBuildCCfg(options, true)
        pipeline.compute(job)

        if(job.hasError){
          println("Error present: " + job.hasError)
          job.tags.foreach{f =>
            f match{
              case t:LocationTag =>
                t.location match {
                  case lcl : LineColumnLocation => println("line --->" + lcl.line + " col --->" + lcl.column)
                  case _ =>
                }
              case _ =>
            }
            println(f)}
          job.lastStageInfo.tags.foreach(f => println(f))
        }
        
        println("pipeline done!")
          
        val avmtFile = new File(resDir + "/" + dirName + "VirtualMethodTables.xml")
        val outerAVMT = new FileOutputStream(avmtFile)
        val avmt = PilarAndroidSymbolResolverModule.getAndroidVirtualMethodTables(options)
        println("start convert AVMT to xml!")
        xStream.toXml(avmt, outerAVMT)
        
//        println("start convert cfgs, rdas, ofgs to xml!")
//        val intraResult = AndroidInterIntraProceduralModule.getIntraResult(options)
//        intraResult.keys.foreach(
//          key =>
//          {
//            aCache.save[AndroidInterIntraProcedural.CFG](key, "cfg", intraResult(key).cfg)
//            intraResult(key).rdaOpt match {
//              case Some(rda) =>
//                aCache.save[AndroidInterIntraProcedural.RDA](key, "rda", rda)
//              case None =>
//            }
//            intraResult(key).ofgOpt match {
//              case Some(ofg) =>
//                aCache.save[AndroidInterIntraProcedural.OFG](key, "ofg", ofg)
//              case None =>
//            }
//            intraResult(key).cCfgOpt match {
//              case Some(cCfg) =>
//                aCache.save[AndroidInterIntraProcedural.CCFG](key, "cCfg", cCfg)
//              case None =>
//            }
//          }  
//        )
        
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
      )
//      ,
//      PipelineStage(
//        "Android InterIntraProcedural Analysis",
//        false,
//        AndroidInterIntraProceduralModule
//      )
    )
    
}