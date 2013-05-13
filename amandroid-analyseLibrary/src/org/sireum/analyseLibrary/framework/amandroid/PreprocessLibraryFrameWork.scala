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
import org.sireum.amandroid.AndroidSymbolResolver.AndroidLibInfoTables
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
import org.sireum.amandroid.module.PilarAndroidSymbolResolverModule
import java.util.zip.GZIPInputStream
import org.sireum.amandroid.AndroidSymbolResolver.AndroidLibInfoTables

trait PreprocessLibraryFrameWork extends TestFramework { 
  
  //////////////////////////////////////////////////////////////////////////////
  // Implemented Public Methods
  //////////////////////////////////////////////////////////////////////////////

  def Analyzing : this.type = this

  def title(s : String) : this.type = {
    _title = caseString + s
    this
  }

  def file(fileUri : FileResourceUri, alit : AndroidLibInfoTables, aCache : AndroidCacheFile[ResourceUri]) =
    AmandroidConfiguration(title, fileUri, alit, aCache)

  //////////////////////////////////////////////////////////////////////////////
  // Public Case Classes
  //////////////////////////////////////////////////////////////////////////////

  case class AmandroidConfiguration //
  (title : String, srcs : FileResourceUri, alit : AndroidLibInfoTables, aCache : AndroidCacheFile[ResourceUri]) {

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
        /*for start analysis from pilar file*/
        val resDir = new File(d+"../result")
        if(!resDir.exists()){
          resDir.mkdir()
        }

        /*end here*/
        
        
    
        val job = PipelineJob()
        val options = job.properties

        ChunkingPilarParserModule.setSources(options, ilist(Right(FileUtil.toUri(d+ "/" +f.getName()))))
        PilarAndroidSymbolResolverModule.setHasExistingAndroidLibInfoTables(options, None)
        PilarAndroidSymbolResolverModule.setShouldBuildLibInfoTables(options, false)
        AndroidInterIntraProceduralModule.setAndroidLibInfoTables(options, alit)
        AndroidInterIntraProceduralModule.setParallel(options, true)
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
        
        println("start convert cfgs, rdas, ofgs to xml!")
        val intraResult = AndroidInterIntraProceduralModule.getIntraResult(options)
        intraResult.keys.foreach(
          key =>
          {
//            val ofg = aCache.load[AndroidInterIntraProcedural.OFG](key, "ofg")
//            val w = new java.io.PrintWriter(System.out, true)
//            ofg.toDot(w)
            
            aCache.save[AndroidInterIntraProcedural.CFG](key, "cfg", intraResult(key).cfg)
            intraResult(key).rdaOpt match {
              case Some(rda) =>
                val cfg = intraResult(key).cfg
                val es : MMap[org.sireum.alir.ControlFlowGraph.Node, ISet[(org.sireum.alir.Slot, org.sireum.alir.DefDesc)]] = mmapEmpty
                cfg.nodes.foreach(
                  node => {
                    es(node) = rda.entrySet(node)
                  }  
                )
                aCache.save[MMap[org.sireum.alir.ControlFlowGraph.Node, ISet[(org.sireum.alir.Slot, org.sireum.alir.DefDesc)]]](key, "rda", es)
              case None =>
            }
            intraResult(key).ofgOpt match {
              case Some(ofg) =>
//                ofg.toDot(w)
                aCache.save[AndroidInterIntraProcedural.OFG](key, "ofg", ofg)
              case None =>
            }
            intraResult(key).cCfgOpt match {
              case Some(cCfg) =>
                aCache.save[AndroidInterIntraProcedural.CCFG](key, "cCfg", cCfg)
              case None =>
            }
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