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
import org.sireum.amandroid.AndroidSymbolResolver.AndroidCompressedSymbolTable
import org.sireum.amandroid.module.AndroidInterIntraProcedural
import org.sireum.amandroid.module.AndroidInterIntraProceduralModule

trait AmandroidAnalyseLibraryFrameWork extends TestFramework { 
  
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
        val nameArray = f.getName().split("\\.")
        var dirName : String = ""
        for(i <- 0 until nameArray.length-1){
          dirName += nameArray(i)
        }
        val d = srcs.substring(srcs.indexOf("/"), srcs.lastIndexOf("/")+1)
//        val dirc = new File(d + dirName)
        val srcFiles = mlistEmpty[FileResourceUri]
        val resDir = new File(d+"result")
//        if(!dirc.exists()){
//          dirc.mkdir()
//        }
        if(!resDir.exists()){
          resDir.mkdir()
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
        
        val stFile = new File(resDir + "/" + dirName + "SymbolTable.xml")
        val avmtFile = new File(resDir + "/" + dirName + "VirtualMethodTables.xml")
        val graphFile = new File(resDir + "/" + dirName + "CCfgs.xml")
        
        val xStream = AndroidXStream
        xStream.xstream.alias("SymbolTable", classOf[SymbolTable])
        xStream.xstream.alias("AndroidVirtualMethodTables", classOf[AndroidVirtualMethodTables])
//        val (stData, pstsData) = xStream.fromXml(stFile).asInstanceOf[(AndroidSymbolTableData, MMap[ResourceUri, ProcedureSymbolTableData])]
//        val existingST : SymbolTable = null
//        existingST.asInstanceOf[AndroidSymbolTableProducer].tables.declaredSymbols ++= stData.declaredSymbols
//        val existingAVMT = xStream.fromXml(avmtFile).asInstanceOf[AndroidVirtualMethodTables]
        
        val job = PipelineJob()
        val options = job.properties
//        Dex2PilarWrapperModule.setSrcFiles(options, srcFiles)

        ChunkingPilarParserModule.setSources(options, ilist(Right(FileUtil.toUri(d+ "/" +f.getName()))))
        
        PilarAndroidSymbolResolverModule.setParallel(options, true)
        PilarAndroidSymbolResolverModule.setHasExistingSymbolTable(options, None)
        PilarAndroidSymbolResolverModule.setHasExistingAndroidVirtualMethodTables(options, None)
        
        AndroidInterIntraProceduralModule.setParallel(options, false)
        AndroidInterIntraProceduralModule.setShouldBuildCfg(options, true)
        AndroidInterIntraProceduralModule.setShouldBuildCCfg(options, true)
        AndroidInterIntraProceduralModule.setShouldBuildSCfg(options, false)
        pipeline.compute(job)

        if(job.hasError){
          println("Error present: " + job.hasError)
          job.tags.foreach(f => println(f))
          job.lastStageInfo.tags.foreach(f => println(f))
        }

        val st = PilarAndroidSymbolResolverModule.getSymbolTable(options)
        val outerST = new FileOutputStream(stFile)
        val wST = new OutputStreamWriter(outerST, "GBK")
        
        
        println("pipeline done!")
        
        
        val rfmFile = new File(resDir + "/recordFileMap.xml")
        
        // recordFileMap is a map from record name r to file name f
        // where f.table stores compressedSymbolTable f.vmtable stores vmtables
        // of r.
        val recordFileMap : MMap[ResourceUri, FileResourceUri] = mmapEmpty
        if(rfmFile.exists()){
           recordFileMap ++= xStream.fromXml(rfmFile).asInstanceOf[MMap[ResourceUri, FileResourceUri]]
        }
        
        st.asInstanceOf[AndroidSymbolTableProducer].tables.recordTable.keys.foreach(
          recordUri =>
            recordFileMap(recordUri) = FileUtil.toUri(d+ "/" +f.getName())
        )
        
        val outerRfm = new FileOutputStream(rfmFile)
        val wRfm = new OutputStreamWriter(outerRfm, "GBK")
        
        xStream.toXml(recordFileMap, wRfm)
        
        st.asInstanceOf[AndroidSymbolTableProducer].tables.procedureAbsTable.foreach(
          item =>
            {
              
              val pst = st.asInstanceOf[AndroidSymbolTableProducer].procedureSymbolTableProducer(item._1)
              val locations = pst.asInstanceOf[ProcedureSymbolTable].locations.toSeq
              
              var deleteFlag = false 
              val compression = Visitor.build({
                
                case j : JumpLocation => true
                
                case t : CallJump => { 
                  deleteFlag = false
                  false
                  }
                
                case _ => {
                  deleteFlag = true
                  false
                  }
              })
              
              for(i <- 0 until locations.size){
                val l = locations(i)
                compression(l)
                if(deleteFlag){
                  pst.tables.bodyTables match {
                    case Some(bt) => bt.locationTable(l.name match {
                      case Some(name) => name.name
                      case _ => null
                    }) = null
                    case _        => 
                  }
                }
                deleteFlag = false
              }
              st.asInstanceOf[AndroidSymbolTableProducer].tables.procedureAbsTable(item._1) = item._2.withoutBody
            }
        )
        val cST : AndroidCompressedSymbolTable = new AndroidCompressedSymbolTable()
        cST.stData = st.asInstanceOf[AndroidSymbolTableProducer].tables
//        st.asInstanceOf[AndroidSymbolTableProducer].tables.procedureAbsTable.foreach(
//          item =>
//            {
//              cST.pstsData(item._1) = st.asInstanceOf[AndroidSymbolTableProducer].procedureSymbolTableProducer(item._1).tables
//            })
        
        println("start convert ST to xml!")
        xStream.toXml(cST, wST)
        
        val outerAVMT = new FileOutputStream(avmtFile)
        val wAVMT = new OutputStreamWriter(outerAVMT, "GBK")
        val avmt = PilarAndroidSymbolResolverModule.getAndroidVirtualMethodTables(options)
        println("start convert AVMT to xml!")
        xStream.toXml(avmt, wAVMT)
        
        val outerGraph = new FileOutputStream(graphFile)
        val wGraph = new OutputStreamWriter(outerGraph, "GBK")
        val cCfgs = AndroidInterIntraProceduralModule.getIntraResult(options)
        println("start convert graph to xml!")
        xStream.xstream.toXML(cCfgs, wGraph)
        
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