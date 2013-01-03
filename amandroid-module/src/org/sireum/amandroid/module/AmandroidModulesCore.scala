package org.sireum.amandroid.module

import org.sireum.util._
import org.sireum.pipeline._
import org.sireum.pilar.ast.Model
import org.sireum.pilar.symbol.SymbolTable
import scala.collection.immutable.Seq
import org.sireum.core.module.PilarParser
import org.sireum.core.module.PilarParserModule


object PilarAndroidSymbolResolverModule extends PipelineModule {
  def title = "Pilar Symbol Resolver for Android"
  def origin = classOf[PilarAndroidSymbolResolver]

  val globalParallelKey = "Global.parallel"
  val globalModelsKey = "Global.models"
  val modelsKey = "PilarAndroidSymbolResolver.models"
  val globalSymbolTableKey = "Global.symbolTable"

  def compute(job : PipelineJob, info : PipelineJobModuleInfo) : MBuffer[Tag] = {
    val tags = marrayEmpty[Tag]
    try {
      val module = Class.forName("org.sireum.amandroid.module.PilarAndroidSymbolResolverDef")
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
    val deps = ilist[PipelineModule](PilarParserModule)
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
    var _parallel : scala.Option[AnyRef] = None
    var _parallelKey : scala.Option[String] = None

    val keylistparallel = List(PilarAndroidSymbolResolverModule.globalParallelKey)
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
    var _models : scala.Option[AnyRef] = None
    var _modelsKey : scala.Option[String] = None

    val keylistmodels = List(PilarAndroidSymbolResolverModule.globalModelsKey, PilarParserModule.modelsKey)
    keylistmodels.foreach(key => 
      if(job ? key) { 
        if(_models.isEmpty) {
          _models = Some(job(key))
          _modelsKey = Some(key)
        }
        if(!(job(key).asInstanceOf[AnyRef] eq _models.get)) {
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': 'models' keys '" + _modelsKey.get + " and '" + key + "' point to different objects.")
        }
      }
    )

    _models match{
      case Some(x) =>
        if(!x.isInstanceOf[scala.collection.immutable.Seq[org.sireum.pilar.ast.Model]]){
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': Wrong type found for 'models'.  Expecting 'scala.collection.immutable.Seq[org.sireum.pilar.ast.Model]' but found '" + x.getClass.toString + "'")
        }
      case None =>
        tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
          "Input error for '" + this.title + "': No value found for 'models'")       
    }
    return tags
  }

  def outputDefined (job : PipelineJob) : MBuffer[Tag] = {
    val tags = marrayEmpty[Tag]
    if(!(job ? PilarAndroidSymbolResolverModule.modelsKey) && !(job ? PilarAndroidSymbolResolverModule.globalModelsKey)) {
      tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
        "Output error for '" + this.title + "': No entry found for 'models'. Expecting (PilarAndroidSymbolResolverModule.modelsKey or PilarAndroidSymbolResolverModule.globalModelsKey)") 
    }

    if(job ? PilarAndroidSymbolResolverModule.modelsKey && !job(PilarAndroidSymbolResolverModule.modelsKey).isInstanceOf[scala.collection.immutable.Seq[org.sireum.pilar.ast.Model]]) {
      tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker, 
        "Output error for '" + this.title + "': Wrong type found for PilarAndroidSymbolResolverModule.modelsKey.  Expecting 'scala.collection.immutable.Seq[org.sireum.pilar.ast.Model]' but found '" + 
        job(PilarAndroidSymbolResolverModule.modelsKey).getClass.toString + "'")
    } 

    if(job ? PilarAndroidSymbolResolverModule.globalModelsKey && !job(PilarAndroidSymbolResolverModule.globalModelsKey).isInstanceOf[scala.collection.immutable.Seq[org.sireum.pilar.ast.Model]]) {
      tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker, 
        "Output error for '" + this.title + "': Wrong type found for PilarAndroidSymbolResolverModule.globalModelsKey.  Expecting 'scala.collection.immutable.Seq[org.sireum.pilar.ast.Model]' but found '" + 
        job(PilarAndroidSymbolResolverModule.globalModelsKey).getClass.toString + "'")
    } 

    if(!(job ? PilarAndroidSymbolResolverModule.globalSymbolTableKey)) {
      tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
        "Output error for '" + this.title + "': No entry found for 'symbolTable'. Expecting (PilarAndroidSymbolResolverModule.globalSymbolTableKey)") 
    }

    if(job ? PilarAndroidSymbolResolverModule.globalSymbolTableKey && !job(PilarAndroidSymbolResolverModule.globalSymbolTableKey).isInstanceOf[org.sireum.pilar.symbol.SymbolTable]) {
      tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker, 
        "Output error for '" + this.title + "': Wrong type found for PilarAndroidSymbolResolverModule.globalSymbolTableKey.  Expecting 'org.sireum.pilar.symbol.SymbolTable' but found '" + 
        job(PilarAndroidSymbolResolverModule.globalSymbolTableKey).getClass.toString + "'")
    } 
    return tags
  }

  def modGetParallel (options : scala.collection.Map[Property.Key, Any]) : scala.Boolean = {
    if (options.contains(PilarAndroidSymbolResolverModule.globalParallelKey)) {
       return options(PilarAndroidSymbolResolverModule.globalParallelKey).asInstanceOf[scala.Boolean]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def setParallel (options : MMap[Property.Key, Any], parallel : scala.Boolean) : MMap[Property.Key, Any] = {

    options(PilarAndroidSymbolResolverModule.globalParallelKey) = parallel

    return options
  }

  def modGetModels (options : scala.collection.Map[Property.Key, Any]) : scala.collection.immutable.Seq[org.sireum.pilar.ast.Model] = {
    if (options.contains(PilarAndroidSymbolResolverModule.globalModelsKey)) {
       return options(PilarAndroidSymbolResolverModule.globalModelsKey).asInstanceOf[scala.collection.immutable.Seq[org.sireum.pilar.ast.Model]]
    }
    if (options.contains(PilarParserModule.modelsKey)) {
       return options(PilarParserModule.modelsKey).asInstanceOf[scala.collection.immutable.Seq[org.sireum.pilar.ast.Model]]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def setModels (options : MMap[Property.Key, Any], models : scala.collection.immutable.Seq[org.sireum.pilar.ast.Model]) : MMap[Property.Key, Any] = {

    options(PilarAndroidSymbolResolverModule.globalModelsKey) = models
    options(PilarParserModule.modelsKey) = models

    return options
  }

  def getModels (options : scala.collection.Map[Property.Key, Any]) : scala.collection.immutable.Seq[org.sireum.pilar.ast.Model] = {
    if (options.contains(PilarAndroidSymbolResolverModule.globalModelsKey)) {
       return options(PilarAndroidSymbolResolverModule.globalModelsKey).asInstanceOf[scala.collection.immutable.Seq[org.sireum.pilar.ast.Model]]
    }
    if (options.contains(PilarAndroidSymbolResolverModule.modelsKey)) {
       return options(PilarAndroidSymbolResolverModule.modelsKey).asInstanceOf[scala.collection.immutable.Seq[org.sireum.pilar.ast.Model]]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def modSetModels (options : MMap[Property.Key, Any], models : scala.collection.immutable.Seq[org.sireum.pilar.ast.Model]) : MMap[Property.Key, Any] = {

    options(PilarAndroidSymbolResolverModule.globalModelsKey) = models
    options(PilarAndroidSymbolResolverModule.modelsKey) = models

    return options
  }

  def getSymbolTable (options : scala.collection.Map[Property.Key, Any]) : org.sireum.pilar.symbol.SymbolTable = {
    if (options.contains(PilarAndroidSymbolResolverModule.globalSymbolTableKey)) {
       return options(PilarAndroidSymbolResolverModule.globalSymbolTableKey).asInstanceOf[org.sireum.pilar.symbol.SymbolTable]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def modSetSymbolTable (options : MMap[Property.Key, Any], symbolTable : org.sireum.pilar.symbol.SymbolTable) : MMap[Property.Key, Any] = {

    options(PilarAndroidSymbolResolverModule.globalSymbolTableKey) = symbolTable

    return options
  }
}

trait PilarAndroidSymbolResolverModule {
  def job : PipelineJob

  def parallel : scala.Boolean = PilarAndroidSymbolResolverModule.modGetParallel(job.properties)

  def models : scala.collection.immutable.Seq[org.sireum.pilar.ast.Model] = PilarAndroidSymbolResolverModule.modGetModels(job.properties)


  def models_=(models : scala.collection.immutable.Seq[org.sireum.pilar.ast.Model]) { PilarAndroidSymbolResolverModule.modSetModels(job.properties, models) }


  def symbolTable_=(symbolTable : org.sireum.pilar.symbol.SymbolTable) { PilarAndroidSymbolResolverModule.modSetSymbolTable(job.properties, symbolTable) }
}