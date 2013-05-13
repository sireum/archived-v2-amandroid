package org.sireum.analyseLibrary.framework.amandroid

import org.sireum.test.framework.TestFramework
import org.sireum.util._
import java.io._
import org.sireum.amandroid.xml.AndroidXStream
import org.sireum.amandroid.AndroidSymbolResolver._
import java.util.zip.GZIPOutputStream
import java.util.zip.GZIPInputStream
import java.util.zip.ZipFile
import org.sireum.pipeline.PipelineJob
import org.sireum.amandroid.module._
import org.sireum.core.module.ChunkingPilarParserModule
import org.sireum.pilar.symbol.SymbolTable
import org.sireum.pipeline.PipelineConfiguration
import org.sireum.pipeline.PipelineStage


class BuildLibInfoTableFramework extends TestFramework { 
  
  //////////////////////////////////////////////////////////////////////////////
  // Implemented Public Methods
  //////////////////////////////////////////////////////////////////////////////

  def Analyzing : this.type = this

  def title(s : String) : this.type = {
    _title = caseString + s
    this
  }

  def combine(fileUri : FileResourceUri) =
    CombineConfiguration(title, fileUri)
    
  def init(fileUri : FileResourceUri) =
    InitConfiguration(title, fileUri)
    
    
    
  case class InitConfiguration //
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
        if(!pilarDir.exists()){
          resDir.mkdir()
        }
        if(!resDir.exists()){
          resDir.mkdir()
        }
        /*end here*/
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
        xStream.xstream.alias("AndroidLibInfoTables", classOf[AndroidLibInfoTables])
    
        val job = PipelineJob()
        val options = job.properties
        Dex2PilarWrapperModule.setSrcFiles(options, srcFiles)
        ChunkingPilarParserModule.setSources(options, ilist(Right(FileUtil.toUri(d+ "/" +f.getName()))))
        PilarAndroidSymbolResolverModule.setParallel(options, false)
        PilarAndroidSymbolResolverModule.setHasExistingAndroidLibInfoTables(options, None)
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
          
        val alitFile = new File(resDir + "/" + dirName + "LibInfoTables.xml")
        val outerALIT = new FileOutputStream(alitFile)
        val avmtOpt = PilarAndroidSymbolResolverModule.getAndroidLibInfoTablesOpt(options)
        println("start convert ALIT to xml!")
        avmtOpt match {
          case Some(avmt) =>
            xStream.toXml(avmt, outerALIT)
          case None =>
        }
        
        
        println("###############################################")
    }
  }
  
  protected val pipeline =
    PipelineConfiguration(
      "libInfoTable building pipeline",
      false,
      PipelineStage(
        "dex2pilar stage",
        false,
        Dex2PilarWrapperModule
      )
      ,
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
    )
    

  //////////////////////////////////////////////////////////////////////////////
  // Public Case Classes
  //////////////////////////////////////////////////////////////////////////////

  case class CombineConfiguration //
  (title : String, srcs : FileResourceUri) {

    ////////////////////////////////////////////////////////////////////////////
    // Test Constructor
    ////////////////////////////////////////////////////////////////////////////

    test(title) {
      println("####" + title + "#####")
        val currentLibInfoTablesFile = new File(srcs.toString().substring(5))
        //create directory
        val nameArray = currentLibInfoTablesFile.getName().split("\\.")
        var dirName : String = ""
        for(i <- 0 until nameArray.length-1){
          dirName += nameArray(i)
        }
        val d = srcs.substring(srcs.indexOf("/"), srcs.lastIndexOf("/")+1)
        val srcFiles = mlistEmpty[FileResourceUri]
        
        val libDir = new File(System.getProperty("user.home") + "/AndroidLibData/")
        if(!libDir.exists()){
          libDir.mkdir()
        }
        val resDir = new File(libDir + "/libInfoTables/")
        if(!resDir.exists()){
          resDir.mkdir()
        }
        
        val xStream = AndroidXStream
        xStream.xstream.alias("AndroidLibInfoTables", classOf[AndroidLibInfoTables])
        
        val libInfoTablesFile = new File(resDir + "/libInfoTables.xml.zip")
        var libInfoTables : AndroidLibInfoTables = null
        if(libInfoTablesFile.exists()){
          val interAVMT = new GZIPInputStream(new FileInputStream(libInfoTablesFile))
          libInfoTables = xStream.fromXml(interAVMT).asInstanceOf[AndroidLibInfoTables]
          interAVMT.close()
        }
        
        val currentLibInfoTables = xStream.fromXml(currentLibInfoTablesFile).asInstanceOf[AndroidLibInfoTables]
        
        if(libInfoTables == null){
          libInfoTables = currentLibInfoTables
        } else {
          println("before merge: " + libInfoTables.asInstanceOf[AndroidLibInfoTablesProducer].tables.cannotFindRecordTable)
          // if libVmTables already have something means we need to merge.
          libInfoTables.mergeWith(currentLibInfoTables)
          println("after merge: " + libInfoTables.asInstanceOf[AndroidLibInfoTablesProducer].tables.cannotFindRecordTable)
        }
        
        val outerAVMT = new GZIPOutputStream(new FileOutputStream(libInfoTablesFile))
        
        println("start convert AVMT to xml!")
        xStream.toXml(libInfoTables, outerAVMT)
        outerAVMT.close()
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

}