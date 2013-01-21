package org.sireum.amandroid.module

import org.sireum.util._
import org.sireum.pipeline._
import org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables
import org.sireum.pilar.ast.Model
import org.sireum.pilar.symbol.SymbolTable
import scala.Option
import scala.collection.immutable.Seq
import org.sireum.core.module.PilarParserModule

object PilarAndroidSymbolResolverModule extends PipelineModule {
  def title = "Pilar Symbol Resolver for Android"
  def origin = classOf[PilarAndroidSymbolResolver]

  val globalParallelKey = "Global.parallel"
  val globalAndroidVirtualMethodTablesKey = "Global.androidVirtualMethodTables"
  val globalHasExistingAndroidVirtualMethodTablesKey = "Global.hasExistingAndroidVirtualMethodTables"
  val globalModelsKey = "Global.models"
  val globalHasExistingSymbolTableKey = "Global.hasExistingSymbolTable"
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
    var _hasExistingSymbolTable : scala.Option[AnyRef] = None
    var _hasExistingSymbolTableKey : scala.Option[String] = None

    val keylisthasExistingSymbolTable = List(PilarAndroidSymbolResolverModule.globalHasExistingSymbolTableKey)
    keylisthasExistingSymbolTable.foreach(key => 
      if(job ? key) { 
        if(_hasExistingSymbolTable.isEmpty) {
          _hasExistingSymbolTable = Some(job(key))
          _hasExistingSymbolTableKey = Some(key)
        }
        if(!(job(key).asInstanceOf[AnyRef] eq _hasExistingSymbolTable.get)) {
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': 'hasExistingSymbolTable' keys '" + _hasExistingSymbolTableKey.get + " and '" + key + "' point to different objects.")
        }
      }
    )

    _hasExistingSymbolTable match{
      case Some(x) =>
        if(!x.isInstanceOf[scala.Option[org.sireum.pilar.symbol.SymbolTable]]){
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': Wrong type found for 'hasExistingSymbolTable'.  Expecting 'scala.Option[org.sireum.pilar.symbol.SymbolTable]' but found '" + x.getClass.toString + "'")
        }
      case None =>
        tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
          "Input error for '" + this.title + "': No value found for 'hasExistingSymbolTable'")       
    }
    var _hasExistingAndroidVirtualMethodTables : scala.Option[AnyRef] = None
    var _hasExistingAndroidVirtualMethodTablesKey : scala.Option[String] = None

    val keylisthasExistingAndroidVirtualMethodTables = List(PilarAndroidSymbolResolverModule.globalHasExistingAndroidVirtualMethodTablesKey)
    keylisthasExistingAndroidVirtualMethodTables.foreach(key => 
      if(job ? key) { 
        if(_hasExistingAndroidVirtualMethodTables.isEmpty) {
          _hasExistingAndroidVirtualMethodTables = Some(job(key))
          _hasExistingAndroidVirtualMethodTablesKey = Some(key)
        }
        if(!(job(key).asInstanceOf[AnyRef] eq _hasExistingAndroidVirtualMethodTables.get)) {
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': 'hasExistingAndroidVirtualMethodTables' keys '" + _hasExistingAndroidVirtualMethodTablesKey.get + " and '" + key + "' point to different objects.")
        }
      }
    )

    _hasExistingAndroidVirtualMethodTables match{
      case Some(x) =>
        if(!x.isInstanceOf[scala.Option[org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables]]){
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': Wrong type found for 'hasExistingAndroidVirtualMethodTables'.  Expecting 'scala.Option[org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables]' but found '" + x.getClass.toString + "'")
        }
      case None =>
        tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
          "Input error for '" + this.title + "': No value found for 'hasExistingAndroidVirtualMethodTables'")       
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

    if(!(job ? PilarAndroidSymbolResolverModule.globalAndroidVirtualMethodTablesKey)) {
      tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
        "Output error for '" + this.title + "': No entry found for 'androidVirtualMethodTables'. Expecting (PilarAndroidSymbolResolverModule.globalAndroidVirtualMethodTablesKey)") 
    }

    if(job ? PilarAndroidSymbolResolverModule.globalAndroidVirtualMethodTablesKey && !job(PilarAndroidSymbolResolverModule.globalAndroidVirtualMethodTablesKey).isInstanceOf[org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables]) {
      tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker, 
        "Output error for '" + this.title + "': Wrong type found for PilarAndroidSymbolResolverModule.globalAndroidVirtualMethodTablesKey.  Expecting 'org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables' but found '" + 
        job(PilarAndroidSymbolResolverModule.globalAndroidVirtualMethodTablesKey).getClass.toString + "'")
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

  def modGetHasExistingSymbolTable (options : scala.collection.Map[Property.Key, Any]) : scala.Option[org.sireum.pilar.symbol.SymbolTable] = {
    if (options.contains(PilarAndroidSymbolResolverModule.globalHasExistingSymbolTableKey)) {
       return options(PilarAndroidSymbolResolverModule.globalHasExistingSymbolTableKey).asInstanceOf[scala.Option[org.sireum.pilar.symbol.SymbolTable]]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def setHasExistingSymbolTable (options : MMap[Property.Key, Any], hasExistingSymbolTable : scala.Option[org.sireum.pilar.symbol.SymbolTable]) : MMap[Property.Key, Any] = {

    options(PilarAndroidSymbolResolverModule.globalHasExistingSymbolTableKey) = hasExistingSymbolTable

    return options
  }

  def modGetHasExistingAndroidVirtualMethodTables (options : scala.collection.Map[Property.Key, Any]) : scala.Option[org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables] = {
    if (options.contains(PilarAndroidSymbolResolverModule.globalHasExistingAndroidVirtualMethodTablesKey)) {
       return options(PilarAndroidSymbolResolverModule.globalHasExistingAndroidVirtualMethodTablesKey).asInstanceOf[scala.Option[org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables]]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def setHasExistingAndroidVirtualMethodTables (options : MMap[Property.Key, Any], hasExistingAndroidVirtualMethodTables : scala.Option[org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables]) : MMap[Property.Key, Any] = {

    options(PilarAndroidSymbolResolverModule.globalHasExistingAndroidVirtualMethodTablesKey) = hasExistingAndroidVirtualMethodTables

    return options
  }

  def getAndroidVirtualMethodTables (options : scala.collection.Map[Property.Key, Any]) : org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables = {
    if (options.contains(PilarAndroidSymbolResolverModule.globalAndroidVirtualMethodTablesKey)) {
       return options(PilarAndroidSymbolResolverModule.globalAndroidVirtualMethodTablesKey).asInstanceOf[org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def modSetAndroidVirtualMethodTables (options : MMap[Property.Key, Any], androidVirtualMethodTables : org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables) : MMap[Property.Key, Any] = {

    options(PilarAndroidSymbolResolverModule.globalAndroidVirtualMethodTablesKey) = androidVirtualMethodTables

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

  def models : scala.collection.immutable.Seq[org.sireum.pilar.ast.Model] = PilarAndroidSymbolResolverModule.modGetModels(job.properties)


  def models_=(models : scala.collection.immutable.Seq[org.sireum.pilar.ast.Model]) { PilarAndroidSymbolResolverModule.modSetModels(job.properties, models) }

  def parallel : scala.Boolean = PilarAndroidSymbolResolverModule.modGetParallel(job.properties)

  def hasExistingSymbolTable : scala.Option[org.sireum.pilar.symbol.SymbolTable] = PilarAndroidSymbolResolverModule.modGetHasExistingSymbolTable(job.properties)

  def hasExistingAndroidVirtualMethodTables : scala.Option[org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables] = PilarAndroidSymbolResolverModule.modGetHasExistingAndroidVirtualMethodTables(job.properties)


  def androidVirtualMethodTables_=(androidVirtualMethodTables : org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables) { PilarAndroidSymbolResolverModule.modSetAndroidVirtualMethodTables(job.properties, androidVirtualMethodTables) }


  def symbolTable_=(symbolTable : org.sireum.pilar.symbol.SymbolTable) { PilarAndroidSymbolResolverModule.modSetSymbolTable(job.properties, symbolTable) }
}