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
import org.sireum.amandroid.module.SystemCFGModule
import org.sireum.pilar.symbol.SymbolTable
import org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables
import org.sireum.amandroid.xml.AndroidXStream
import org.sireum.amandroid.AndroidSymbolResolver.AndroidSymbolTableProducer
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.pilar.symbol.ProcedureSymbolTableData
import org.sireum.amandroid.AndroidSymbolResolver.AndroidSymbolTableData
import org.sireum.amandroid.AndroidSymbolResolver.AndroidCompressedSymbolTable

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
        val dirc = new File(d + dirName)
        val srcFiles = mlistEmpty[FileResourceUri]
        val resDir = new File(d+"result")
        if(!dirc.exists()){
          dirc.mkdir()
        }
        if(!resDir.exists()){
          resDir.mkdir()
        }
        //deal with jar file
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
        
        val stFile = new File(resDir + "/" + dirName + "SymbolTable.xml")
        val avmtFile = new File(resDir + "/" + dirName + "VirtualMethodTables.xml")
        
        val xStream = AndroidXStream
        xStream.xstream.alias("SymbolTable", classOf[SymbolTable])
        xStream.xstream.alias("AndroidVirtualMethodTables", classOf[AndroidVirtualMethodTables])
//        val (stData, pstsData) = xStream.fromXml(stFile).asInstanceOf[(AndroidSymbolTableData, MMap[ResourceUri, ProcedureSymbolTableData])]
//        val existingST : SymbolTable = null
//        existingST.asInstanceOf[AndroidSymbolTableProducer].tables.declaredSymbols ++= stData.declaredSymbols
//        val existingAVMT = xStream.fromXml(avmtFile).asInstanceOf[AndroidVirtualMethodTables]
        
        val job = PipelineJob()
        val options = job.properties
        Dex2PilarWrapperModule.setSrcFiles(options, srcFiles)
//        ChunkingPilarParserModule.setSources(options, ilist(Right(FileUtil.toUri(d+dirName+"/classes.pilar"))))
        
        PilarAndroidSymbolResolverModule.setParallel(options, false)
        PilarAndroidSymbolResolverModule.setHasExistingSymbolTable(options, None)
        PilarAndroidSymbolResolverModule.setHasExistingAndroidVirtualMethodTables(options, None)
        
        AlirIntraProceduralModule.setShouldBuildCfg(options, true)
        AlirIntraProceduralModule.setShouldBuildCdg(options, false)
        AlirIntraProceduralModule.setShouldBuildRda(options, false)
        AlirIntraProceduralModule.setShouldBuildDdg(options, false)
        AlirIntraProceduralModule.setShouldBuildPdg(options, false)
        AlirIntraProceduralModule.setShouldBuildDfg(options, false)
        AlirIntraProceduralModule.setShouldBuildIdg(options, false)
        AlirIntraProceduralModule.setProcedureAbsUriIterator(options, None)
        pipeline.compute(job)
        if(job.hasError){
          println("Error present: " + job.hasError)
          job.tags.foreach(f => println(f))
          job.lastStageInfo.tags.foreach(f => println(f))
        }

        val outerST = new FileOutputStream(stFile)
        val wST = new OutputStreamWriter(outerST, "GBK")
        val st = PilarAndroidSymbolResolverModule.getSymbolTable(options)

        
        
        
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
        st.asInstanceOf[AndroidSymbolTableProducer].tables.procedureAbsTable.foreach(
          item =>
            {
              cST.pstsData(item._1) = st.asInstanceOf[AndroidSymbolTableProducer].procedureSymbolTableProducer(item._1).tables
            })
        println("start convert xml!")
        xStream.toXml(cST, wST)
        
        val outerAVMT = new FileOutputStream(avmtFile)
        val wAVMT = new OutputStreamWriter(outerAVMT, "GBK")
        val avmt = PilarAndroidSymbolResolverModule.getAndroidVirtualMethodTables(options)
        
        xStream.toXml(avmt, wAVMT)
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
      ),
      PipelineStage(
        "Alir IntraProcedural Analysis",
        false,
        AlirIntraProceduralModule
      ),
      PipelineStage(
        "compresses CFGs, and builds the SCFG for Android",
        false,
        SystemCFGModule
      )
    )
}