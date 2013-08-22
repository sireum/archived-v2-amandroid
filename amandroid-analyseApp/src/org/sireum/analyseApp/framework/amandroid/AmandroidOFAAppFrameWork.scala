package org.sireum.analyseApp.framework.amandroid

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
import org.sireum.amandroid.module.AndroidIntraProceduralModule
import org.sireum.amandroid.xml.AndroidXStream
import org.sireum.alir.AlirIntraProceduralGraph
import org.sireum.amandroid.module.AndroidInterProceduralModule
import org.sireum.amandroid.module.AndroidApplicationPrepareModule
import org.sireum.amandroid.module.AndroidFixIntraProceduralModule
import org.sireum.amandroid.symbolResolver.AndroidLibInfoTables
import org.sireum.amandroid.android.cache.AndroidCacheFile

// sankar introduces the following framework which adds one stage on top of AmandroidParserTestFrameWork 

trait AmandroidOFAAppFrameWork extends TestFramework { 
  
  //////////////////////////////////////////////////////////////////////////////
  // Implemented Public Methods
  //////////////////////////////////////////////////////////////////////////////

  def Analyzing : this.type = this

  def title(s : String) : this.type = {
    _title = caseString + s
    this
  }

  def file(fileUri : FileResourceUri,
           libInfoTables : AndroidLibInfoTables,
           aCache : AndroidCacheFile[ResourceUri],
           startFromApk : Boolean) =
    AmandroidConfiguration(title, fileUri, libInfoTables, aCache, startFromApk)

  //////////////////////////////////////////////////////////////////////////////
  // Public Case Classes
  //////////////////////////////////////////////////////////////////////////////

  type VirtualLabel = String
  
  case class AmandroidConfiguration //
  (title : String,
   srcs : FileResourceUri,
   libInfoTables : AndroidLibInfoTables,
   aCache : AndroidCacheFile[ResourceUri],
   startFromApk : Boolean) {

    ////////////////////////////////////////////////////////////////////////////
    // Test Constructor
    ////////////////////////////////////////////////////////////////////////////

    test(title) {
      println("####" + title + "#####")
      if(startFromApk){
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
      
        srcFiles += FileUtil.toUri(d + dirName + "/" + dirName + ".dex")
        
        val job = PipelineJob()
        val options = job.properties
        Dex2PilarWrapperModule.setSrcFiles(options, srcFiles)
        
        ChunkingPilarParserModule.setSources(options, ilist(Right(FileUtil.toUri(d + dirName + "/" + dirName + ".pilar"))))
        PilarAndroidSymbolResolverModule.setParallel(options, true)
        PilarAndroidSymbolResolverModule.setHasExistingAndroidLibInfoTables(options, Some(libInfoTables))
        AndroidApplicationPrepareModule.setApkFileLocation(options, f.toString())
        AndroidIntraProceduralModule.setParallel(options, true)
        AndroidIntraProceduralModule.setAndroidCache(options, Some(aCache))
        AndroidIntraProceduralModule.setShouldBuildCfg(options, true)
        AndroidIntraProceduralModule.setShouldBuildRda(options, true)
        AndroidIntraProceduralModule.setShouldBuildCCfg(options, true)
        AndroidInterProceduralModule.setShouldBuildOFAsCfg(options, true)
        
        // experimental code starts which does not have any significant role now; later we will delete it after some related cleaning 
        
        val apiPermission : MMap[ResourceUri, MList[String]] = mmapEmpty
        val permList : MList[String] = mlistEmpty
        permList+=("permission")   // put the permission strings e.g. "NETWORKS" here
        val pUri : ResourceUri = "abc"  // replace "abc" by apiUri (e.g. pilar:/procedure/default/%5B%7Candroid::content::Context.startService%7C%5D/1/50/f9cb48df) later
        apiPermission(pUri) = permList
        AndroidIntraProceduralModule.setAPIpermOpt(options, Option(apiPermission)) 
        
        // experimental code ends
        
        apkPipeline.compute(job)
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
      } else {
        
        val f = new File(srcs.toString().substring(5))
        //create directory
        val dirName = f.getName().split("\\.")(0)
        val d = srcs.substring(srcs.indexOf("/"), srcs.lastIndexOf("/")+1)
        val apkFile = new File(srcs.substring(srcs.indexOf("/"), srcs.lastIndexOf("/")) + ".apk")
        println("apkfile--->" + apkFile)
        val job = PipelineJob()
        val options = job.properties
        
        ChunkingPilarParserModule.setSources(options, ilist(Right(FileUtil.toUri(d + "/" + dirName + ".pilar"))))
        PilarAndroidSymbolResolverModule.setParallel(options, false)
        PilarAndroidSymbolResolverModule.setHasExistingAndroidLibInfoTables(options, Some(libInfoTables))
        AndroidApplicationPrepareModule.setApkFileLocation(options, apkFile.toString())
        AndroidIntraProceduralModule.setParallel(options, false)
        AndroidIntraProceduralModule.setAndroidCache(options, Some(aCache))
        AndroidIntraProceduralModule.setShouldBuildCfg(options, true)
        AndroidIntraProceduralModule.setShouldBuildRda(options, true)
        AndroidIntraProceduralModule.setShouldBuildCCfg(options, true)
        AndroidInterProceduralModule.setShouldBuildOFAsCfg(options, true)
        
        // experimental code starts which does not have any significant role now; later we will delete it after some related cleaning 
        
        val apiPermission : MMap[ResourceUri, MList[String]] = mmapEmpty
        val permList : MList[String] = mlistEmpty
        permList+=("permission")   // put the permission strings e.g. "NETWORKS" here
        val pUri : ResourceUri = "abc"  // replace "abc" by apiUri (e.g. pilar:/procedure/default/%5B%7Candroid::content::Context.startService%7C%5D/1/50/f9cb48df) later
        apiPermission(pUri) = permList
        AndroidIntraProceduralModule.setAPIpermOpt(options, Option(apiPermission)) 
        
        // experimental code ends
        
        pilarPipeline.compute(job)
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
        
      }
//        val r = AndroidInterIntraProceduralModule.getInterResult(options)
        
 
        
        //1. get Cfg
//        val CfgFile = new File(d + dirName + "/graphs/Cfg.graph")
//        val outer = new FileOutputStream(CfgFile)
//        val w = new OutputStreamWriter(outer, "GBK")
//        val w = new PrintWriter(System.out, true)
//        val cfgs = AndroidInterIntraProceduralModule.getIntraResult(options)
//        cfgs.foreach{case(k, v) => println(k) ;if(k.contains("dummyMain")) v.cfg.toDot(w)}
        
//        val csCfg = AndroidInterIntraProceduralModule.getInterResult(options) match {
//          case Some(s) => s
//          case None => null
//        }
//        sCfg.toDot(w)
//        w.flush()
//        val str = Array("dot", "-Tps2", d + dirName + "/graphs/sCfg.dot", "-o", d + dirName + "/graphs/sCfg.ps")
//        val proc = Runtime.getRuntime().exec(str) 
        
        println("###############################################")
        
        //get csCfg
//        val csCfgFile = new File(d + dirName + "/graphs/csCfg.graph")
//        val outer = new FileOutputStream(csCfgFile)
//        val w = new OutputStreamWriter(outer, "GBK")
//        w.write(csCfg.toString())
// //       println(csCfg.toString())
//        println(" csCfg saving is done which is same as sCfg now because no compression was made")
    }
    
    
    protected val apkPipeline =
	    PipelineConfiguration(
      "App analysis pipeline",
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
      ,
      PipelineStage(
      "Android IntraProcedural Analysis",
      false,
      AndroidIntraProceduralModule
      )
      ,
      PipelineStage(
        "AndroidApplicationPrepareModule stage",
        false,
        AndroidApplicationPrepareModule
      )
      ,
      PipelineStage(
        "AndroidFixIntraProceduralModule stage",
        false,
        AndroidFixIntraProceduralModule
      )
      ,
      PipelineStage(
      "Android InterProcedural Analysis",
      false,
      AndroidInterProceduralModule
      )
    )
	    
	 protected val pilarPipeline =
	    PipelineConfiguration(
      "App analysis pipeline",
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
      "Android IntraProcedural Analysis",
      false,
      AndroidIntraProceduralModule
      )
      ,
      PipelineStage(
        "AndroidApplicationPrepareModule stage",
        false,
        AndroidApplicationPrepareModule
      )
      ,
      PipelineStage(
        "AndroidFixIntraProceduralModule stage",
        false,
        AndroidFixIntraProceduralModule
      )
      ,
      PipelineStage(
      "Android InterProcedural Analysis",
      false,
      AndroidInterProceduralModule
      )
    )
    
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