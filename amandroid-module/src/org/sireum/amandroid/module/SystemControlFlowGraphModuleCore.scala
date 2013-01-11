package org.sireum.amandroid.module

import org.sireum.util._
import org.sireum.pipeline._
import java.lang.String
import org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables
import org.sireum.amandroid.module.AndroidInterProcedural.AndroidInterProceduralAnalysisResult
import org.sireum.core.module.AlirIntraProcedural.AlirIntraproceduralAnalysisResult
import org.sireum.pilar.symbol.SymbolTable
import scala.collection.mutable.Map

object SystemCFGModule extends PipelineModule {
  def title = "compress CFGs, and build the SCFG for Android"
  def origin = classOf[SystemCFG]

  val globalParallelKey = "Global.parallel"
  val globalAndroidresultKey = "Global.androidresult"
  val globalAndroidVirtualMethodTablesKey = "Global.androidVirtualMethodTables"
  val globalResultKey = "Global.result"
  val globalSymbolTableKey = "Global.symbolTable"

  def compute(job : PipelineJob, info : PipelineJobModuleInfo) : MBuffer[Tag] = {
    val tags = marrayEmpty[Tag]
    try {
      val module = Class.forName("org.sireum.amandroid.module.SystemCFGDef")
      val cons = module.getConstructors()(0)
      val params = Array[AnyRef](job, info)
      val inst = cons.newInstance(params : _*)
    } catch {
      case e =>
        e.printStackTrace
        tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker, e.getMessage);
    }
    return tags
  }

  override def initialize(job : PipelineJob) {
  }

  override def validPipeline(stage : PipelineStage, job : PipelineJob) : MBuffer[Tag] = {
    val tags = marrayEmpty[Tag]
    val deps = ilist[PipelineModule]()
    deps.foreach(d =>
      if(stage.modules.contains(d)){
        tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "'" + this.title + "' depends on '" + d.title + "' yet both were found in stage '" + stage.title + "'"
        )
      }
    )
    return tags
  }

  def inputDefined (job : PipelineJob) : MBuffer[Tag] = {
    val tags = marrayEmpty[Tag]
    var _result : scala.Option[AnyRef] = None
    var _resultKey : scala.Option[String] = None

    val keylistresult = List(SystemCFGModule.globalResultKey)
    keylistresult.foreach(key => 
      if(job ? key) { 
        if(_result.isEmpty) {
          _result = Some(job(key))
          _resultKey = Some(key)
        }
        if(!(job(key).asInstanceOf[AnyRef] eq _result.get)) {
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': 'result' keys '" + _resultKey.get + " and '" + key + "' point to different objects.")
        }
      }
    )

    _result match{
      case Some(x) =>
        if(!x.isInstanceOf[scala.collection.mutable.Map[java.lang.String, org.sireum.core.module.AlirIntraProcedural.AlirIntraproceduralAnalysisResult]]){
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': Wrong type found for 'result'.  Expecting 'scala.collection.mutable.Map[java.lang.String, org.sireum.core.module.AlirIntraProcedural.AlirIntraproceduralAnalysisResult]' but found '" + x.getClass.toString + "'")
        }
      case None =>
        tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
          "Input error for '" + this.title + "': No value found for 'result'")       
    }
    var _parallel : scala.Option[AnyRef] = None
    var _parallelKey : scala.Option[String] = None

    val keylistparallel = List(SystemCFGModule.globalParallelKey)
    keylistparallel.foreach(key => 
      if(job ? key) { 
        if(_parallel.isEmpty) {
          _parallel = Some(job(key))
          _parallelKey = Some(key)
        }
        if(!(job(key).asInstanceOf[AnyRef] eq _parallel.get)) {
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': 'parallel' keys '" + _parallelKey.get + " and '" + key + "' point to different objects.")
        }
      }
    )

    _parallel match{
      case Some(x) =>
        if(!x.isInstanceOf[scala.Boolean]){
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': Wrong type found for 'parallel'.  Expecting 'scala.Boolean' but found '" + x.getClass.toString + "'")
        }
      case None =>
        tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
          "Input error for '" + this.title + "': No value found for 'parallel'")       
    }
    var _symbolTable : scala.Option[AnyRef] = None
    var _symbolTableKey : scala.Option[String] = None

    val keylistsymbolTable = List(SystemCFGModule.globalSymbolTableKey)
    keylistsymbolTable.foreach(key => 
      if(job ? key) { 
        if(_symbolTable.isEmpty) {
          _symbolTable = Some(job(key))
          _symbolTableKey = Some(key)
        }
        if(!(job(key).asInstanceOf[AnyRef] eq _symbolTable.get)) {
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': 'symbolTable' keys '" + _symbolTableKey.get + " and '" + key + "' point to different objects.")
        }
      }
    )

    _symbolTable match{
      case Some(x) =>
        if(!x.isInstanceOf[org.sireum.pilar.symbol.SymbolTable]){
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': Wrong type found for 'symbolTable'.  Expecting 'org.sireum.pilar.symbol.SymbolTable' but found '" + x.getClass.toString + "'")
        }
      case None =>
        tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
          "Input error for '" + this.title + "': No value found for 'symbolTable'")       
    }
    var _androidVirtualMethodTables : scala.Option[AnyRef] = None
    var _androidVirtualMethodTablesKey : scala.Option[String] = None

    val keylistandroidVirtualMethodTables = List(SystemCFGModule.globalAndroidVirtualMethodTablesKey)
    keylistandroidVirtualMethodTables.foreach(key => 
      if(job ? key) { 
        if(_androidVirtualMethodTables.isEmpty) {
          _androidVirtualMethodTables = Some(job(key))
          _androidVirtualMethodTablesKey = Some(key)
        }
        if(!(job(key).asInstanceOf[AnyRef] eq _androidVirtualMethodTables.get)) {
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': 'androidVirtualMethodTables' keys '" + _androidVirtualMethodTablesKey.get + " and '" + key + "' point to different objects.")
        }
      }
    )

    _androidVirtualMethodTables match{
      case Some(x) =>
        if(!x.isInstanceOf[org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables]){
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': Wrong type found for 'androidVirtualMethodTables'.  Expecting 'org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables' but found '" + x.getClass.toString + "'")
        }
      case None =>
        tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
          "Input error for '" + this.title + "': No value found for 'androidVirtualMethodTables'")       
    }
    return tags
  }

  def outputDefined (job : PipelineJob) : MBuffer[Tag] = {
    val tags = marrayEmpty[Tag]
    if(!(job ? SystemCFGModule.globalAndroidresultKey)) {
      tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
        "Output error for '" + this.title + "': No entry found for 'androidresult'. Expecting (SystemCFGModule.globalAndroidresultKey)") 
    }

    if(job ? SystemCFGModule.globalAndroidresultKey && !job(SystemCFGModule.globalAndroidresultKey).isInstanceOf[org.sireum.amandroid.module.AndroidInterProcedural.AndroidInterProceduralAnalysisResult]) {
      tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker, 
        "Output error for '" + this.title + "': Wrong type found for SystemCFGModule.globalAndroidresultKey.  Expecting 'org.sireum.amandroid.module.AndroidInterProcedural.AndroidInterProceduralAnalysisResult' but found '" + 
        job(SystemCFGModule.globalAndroidresultKey).getClass.toString + "'")
    } 
    return tags
  }

  def modGetResult (options : scala.collection.Map[Property.Key, Any]) : scala.collection.mutable.Map[java.lang.String, org.sireum.core.module.AlirIntraProcedural.AlirIntraproceduralAnalysisResult] = {
    if (options.contains(SystemCFGModule.globalResultKey)) {
       return options(SystemCFGModule.globalResultKey).asInstanceOf[scala.collection.mutable.Map[java.lang.String, org.sireum.core.module.AlirIntraProcedural.AlirIntraproceduralAnalysisResult]]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def setResult (options : MMap[Property.Key, Any], result : scala.collection.mutable.Map[java.lang.String, org.sireum.core.module.AlirIntraProcedural.AlirIntraproceduralAnalysisResult]) : MMap[Property.Key, Any] = {

    options(SystemCFGModule.globalResultKey) = result

    return options
  }

  def modGetParallel (options : scala.collection.Map[Property.Key, Any]) : scala.Boolean = {
    if (options.contains(SystemCFGModule.globalParallelKey)) {
       return options(SystemCFGModule.globalParallelKey).asInstanceOf[scala.Boolean]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def setParallel (options : MMap[Property.Key, Any], parallel : scala.Boolean) : MMap[Property.Key, Any] = {

    options(SystemCFGModule.globalParallelKey) = parallel

    return options
  }

  def modGetSymbolTable (options : scala.collection.Map[Property.Key, Any]) : org.sireum.pilar.symbol.SymbolTable = {
    if (options.contains(SystemCFGModule.globalSymbolTableKey)) {
       return options(SystemCFGModule.globalSymbolTableKey).asInstanceOf[org.sireum.pilar.symbol.SymbolTable]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def setSymbolTable (options : MMap[Property.Key, Any], symbolTable : org.sireum.pilar.symbol.SymbolTable) : MMap[Property.Key, Any] = {

    options(SystemCFGModule.globalSymbolTableKey) = symbolTable

    return options
  }

  def modGetAndroidVirtualMethodTables (options : scala.collection.Map[Property.Key, Any]) : org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables = {
    if (options.contains(SystemCFGModule.globalAndroidVirtualMethodTablesKey)) {
       return options(SystemCFGModule.globalAndroidVirtualMethodTablesKey).asInstanceOf[org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def setAndroidVirtualMethodTables (options : MMap[Property.Key, Any], androidVirtualMethodTables : org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables) : MMap[Property.Key, Any] = {

    options(SystemCFGModule.globalAndroidVirtualMethodTablesKey) = androidVirtualMethodTables

    return options
  }

  def getAndroidresult (options : scala.collection.Map[Property.Key, Any]) : org.sireum.amandroid.module.AndroidInterProcedural.AndroidInterProceduralAnalysisResult = {
    if (options.contains(SystemCFGModule.globalAndroidresultKey)) {
       return options(SystemCFGModule.globalAndroidresultKey).asInstanceOf[org.sireum.amandroid.module.AndroidInterProcedural.AndroidInterProceduralAnalysisResult]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def modSetAndroidresult (options : MMap[Property.Key, Any], androidresult : org.sireum.amandroid.module.AndroidInterProcedural.AndroidInterProceduralAnalysisResult) : MMap[Property.Key, Any] = {

    options(SystemCFGModule.globalAndroidresultKey) = androidresult

    return options
  }
}

trait SystemCFGModule {
  def job : PipelineJob

  def result : scala.collection.mutable.Map[java.lang.String, org.sireum.core.module.AlirIntraProcedural.AlirIntraproceduralAnalysisResult] = SystemCFGModule.modGetResult(job.properties)

  def parallel : scala.Boolean = SystemCFGModule.modGetParallel(job.properties)

  def symbolTable : org.sireum.pilar.symbol.SymbolTable = SystemCFGModule.modGetSymbolTable(job.properties)

  def androidVirtualMethodTables : org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables = SystemCFGModule.modGetAndroidVirtualMethodTables(job.properties)


  def androidresult_=(androidresult : org.sireum.amandroid.module.AndroidInterProcedural.AndroidInterProceduralAnalysisResult) { SystemCFGModule.modSetAndroidresult(job.properties, androidresult) }
}