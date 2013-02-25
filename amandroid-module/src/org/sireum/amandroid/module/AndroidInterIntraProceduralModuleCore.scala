package org.sireum.amandroid.module

import org.sireum.util._
import org.sireum.pipeline._
import java.lang.String
import org.sireum.alir.AlirIntraProceduralNode
import org.sireum.alir.ControlFlowGraph
import org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables
import org.sireum.amandroid.cache.AndroidCacheFile
import org.sireum.amandroid.scfg.CompressedControlFlowGraph
import org.sireum.amandroid.scfg.SystemControlFlowGraph
import org.sireum.pilar.ast.LocationDecl
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.pilar.symbol.SymbolTable
import scala.Function2
import scala.Option
import scala.collection.mutable.Map

object AndroidInterIntraProceduralModule extends PipelineModule {
  def title = "Android InterIntraProcedural Module"
  def origin = classOf[AndroidInterIntraProcedural]

  val globalParallelKey = "Global.parallel"
  val globalShouldBuildCCfgKey = "Global.shouldBuildCCfg"
  val globalShouldBuildCSCfgKey = "Global.shouldBuildCSCfg"
  val globalInterResultKey = "Global.interResult"
  val globalAndroidVirtualMethodTablesKey = "Global.androidVirtualMethodTables"
  val globalAPIpermOptKey = "Global.APIpermOpt"
  val globalAndroidCacheKey = "Global.androidCache"
  val globalIntraResultKey = "Global.intraResult"
  val globalShouldIncludeFlowFunctionKey = "Global.shouldIncludeFlowFunction"
  val globalShouldBuildSCfgKey = "Global.shouldBuildSCfg"
  val globalSymbolTableKey = "Global.symbolTable"

  def compute(job : PipelineJob, info : PipelineJobModuleInfo) : MBuffer[Tag] = {
    val tags = marrayEmpty[Tag]
    try {
      val module = Class.forName("org.sireum.amandroid.module.AndroidInterIntraProceduralDef")
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
    if(!(job ? AndroidInterIntraProceduralModule.globalShouldIncludeFlowFunctionKey)) {
      val shouldIncludeFlowFunction = Class.forName("org.sireum.amandroid.module.AndroidInterIntraProcedural").getDeclaredMethod("$lessinit$greater$default$10").invoke(null).asInstanceOf[scala.Function2[org.sireum.pilar.ast.LocationDecl, scala.collection.Iterable[org.sireum.pilar.ast.CatchClause], scala.Tuple2[scala.collection.Iterable[org.sireum.pilar.ast.CatchClause], scala.Boolean]]]
      setShouldIncludeFlowFunction(job.properties, shouldIncludeFlowFunction)
    }
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
    var _parallel : scala.Option[AnyRef] = None
    var _parallelKey : scala.Option[String] = None

    val keylistparallel = List(AndroidInterIntraProceduralModule.globalParallelKey)
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

    val keylistsymbolTable = List(AndroidInterIntraProceduralModule.globalSymbolTableKey)
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

    val keylistandroidVirtualMethodTables = List(AndroidInterIntraProceduralModule.globalAndroidVirtualMethodTablesKey)
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
    var _androidCache : scala.Option[AnyRef] = None
    var _androidCacheKey : scala.Option[String] = None

    val keylistandroidCache = List(AndroidInterIntraProceduralModule.globalAndroidCacheKey)
    keylistandroidCache.foreach(key => 
      if(job ? key) { 
        if(_androidCache.isEmpty) {
          _androidCache = Some(job(key))
          _androidCacheKey = Some(key)
        }
        if(!(job(key).asInstanceOf[AnyRef] eq _androidCache.get)) {
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': 'androidCache' keys '" + _androidCacheKey.get + " and '" + key + "' point to different objects.")
        }
      }
    )

    _androidCache match{
      case Some(x) =>
        if(!x.isInstanceOf[org.sireum.amandroid.cache.AndroidCacheFile[java.lang.String]]){
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': Wrong type found for 'androidCache'.  Expecting 'org.sireum.amandroid.cache.AndroidCacheFile[java.lang.String]' but found '" + x.getClass.toString + "'")
        }
      case None =>
        tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
          "Input error for '" + this.title + "': No value found for 'androidCache'")       
    }
    var _shouldBuildCCfg : scala.Option[AnyRef] = None
    var _shouldBuildCCfgKey : scala.Option[String] = None

    val keylistshouldBuildCCfg = List(AndroidInterIntraProceduralModule.globalShouldBuildCCfgKey)
    keylistshouldBuildCCfg.foreach(key => 
      if(job ? key) { 
        if(_shouldBuildCCfg.isEmpty) {
          _shouldBuildCCfg = Some(job(key))
          _shouldBuildCCfgKey = Some(key)
        }
        if(!(job(key).asInstanceOf[AnyRef] eq _shouldBuildCCfg.get)) {
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': 'shouldBuildCCfg' keys '" + _shouldBuildCCfgKey.get + " and '" + key + "' point to different objects.")
        }
      }
    )

    _shouldBuildCCfg match{
      case Some(x) =>
        if(!x.isInstanceOf[scala.Boolean]){
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': Wrong type found for 'shouldBuildCCfg'.  Expecting 'scala.Boolean' but found '" + x.getClass.toString + "'")
        }
      case None =>
        tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
          "Input error for '" + this.title + "': No value found for 'shouldBuildCCfg'")       
    }
    var _shouldBuildSCfg : scala.Option[AnyRef] = None
    var _shouldBuildSCfgKey : scala.Option[String] = None

    val keylistshouldBuildSCfg = List(AndroidInterIntraProceduralModule.globalShouldBuildSCfgKey)
    keylistshouldBuildSCfg.foreach(key => 
      if(job ? key) { 
        if(_shouldBuildSCfg.isEmpty) {
          _shouldBuildSCfg = Some(job(key))
          _shouldBuildSCfgKey = Some(key)
        }
        if(!(job(key).asInstanceOf[AnyRef] eq _shouldBuildSCfg.get)) {
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': 'shouldBuildSCfg' keys '" + _shouldBuildSCfgKey.get + " and '" + key + "' point to different objects.")
        }
      }
    )

    _shouldBuildSCfg match{
      case Some(x) =>
        if(!x.isInstanceOf[scala.Boolean]){
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': Wrong type found for 'shouldBuildSCfg'.  Expecting 'scala.Boolean' but found '" + x.getClass.toString + "'")
        }
      case None =>
        tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
          "Input error for '" + this.title + "': No value found for 'shouldBuildSCfg'")       
    }
    var _shouldBuildCSCfg : scala.Option[AnyRef] = None
    var _shouldBuildCSCfgKey : scala.Option[String] = None

    val keylistshouldBuildCSCfg = List(AndroidInterIntraProceduralModule.globalShouldBuildCSCfgKey)
    keylistshouldBuildCSCfg.foreach(key => 
      if(job ? key) { 
        if(_shouldBuildCSCfg.isEmpty) {
          _shouldBuildCSCfg = Some(job(key))
          _shouldBuildCSCfgKey = Some(key)
        }
        if(!(job(key).asInstanceOf[AnyRef] eq _shouldBuildCSCfg.get)) {
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': 'shouldBuildCSCfg' keys '" + _shouldBuildCSCfgKey.get + " and '" + key + "' point to different objects.")
        }
      }
    )

    _shouldBuildCSCfg match{
      case Some(x) =>
        if(!x.isInstanceOf[scala.Boolean]){
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': Wrong type found for 'shouldBuildCSCfg'.  Expecting 'scala.Boolean' but found '" + x.getClass.toString + "'")
        }
      case None =>
        tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
          "Input error for '" + this.title + "': No value found for 'shouldBuildCSCfg'")       
    }
    var _APIpermOpt : scala.Option[AnyRef] = None
    var _APIpermOptKey : scala.Option[String] = None

    val keylistAPIpermOpt = List(AndroidInterIntraProceduralModule.globalAPIpermOptKey)
    keylistAPIpermOpt.foreach(key => 
      if(job ? key) { 
        if(_APIpermOpt.isEmpty) {
          _APIpermOpt = Some(job(key))
          _APIpermOptKey = Some(key)
        }
        if(!(job(key).asInstanceOf[AnyRef] eq _APIpermOpt.get)) {
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': 'APIpermOpt' keys '" + _APIpermOptKey.get + " and '" + key + "' point to different objects.")
        }
      }
    )

    _APIpermOpt match{
      case Some(x) =>
        if(!x.isInstanceOf[scala.Option[scala.collection.mutable.Map[java.lang.String, scala.collection.mutable.ListBuffer[java.lang.String]]]]){
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': Wrong type found for 'APIpermOpt'.  Expecting 'scala.Option[scala.collection.mutable.Map[java.lang.String, scala.collection.mutable.ListBuffer[java.lang.String]]]' but found '" + x.getClass.toString + "'")
        }
      case None =>
        tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
          "Input error for '" + this.title + "': No value found for 'APIpermOpt'")       
    }
    var _shouldIncludeFlowFunction : scala.Option[AnyRef] = None
    var _shouldIncludeFlowFunctionKey : scala.Option[String] = None

    val keylistshouldIncludeFlowFunction = List(AndroidInterIntraProceduralModule.globalShouldIncludeFlowFunctionKey)
    keylistshouldIncludeFlowFunction.foreach(key => 
      if(job ? key) { 
        if(_shouldIncludeFlowFunction.isEmpty) {
          _shouldIncludeFlowFunction = Some(job(key))
          _shouldIncludeFlowFunctionKey = Some(key)
        }
        if(!(job(key).asInstanceOf[AnyRef] eq _shouldIncludeFlowFunction.get)) {
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': 'shouldIncludeFlowFunction' keys '" + _shouldIncludeFlowFunctionKey.get + " and '" + key + "' point to different objects.")
        }
      }
    )

    _shouldIncludeFlowFunction match{
      case Some(x) =>
        if(!x.isInstanceOf[scala.Function2[org.sireum.pilar.ast.LocationDecl, scala.collection.Iterable[org.sireum.pilar.ast.CatchClause], scala.Tuple2[scala.collection.Iterable[org.sireum.pilar.ast.CatchClause], scala.Boolean]]]){
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': Wrong type found for 'shouldIncludeFlowFunction'.  Expecting 'scala.Function2[org.sireum.pilar.ast.LocationDecl, scala.collection.Iterable[org.sireum.pilar.ast.CatchClause], scala.Tuple2[scala.collection.Iterable[org.sireum.pilar.ast.CatchClause], scala.Boolean]]' but found '" + x.getClass.toString + "'")
        }
      case None =>
        tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
          "Input error for '" + this.title + "': No value found for 'shouldIncludeFlowFunction'")       
    }
    return tags
  }

  def outputDefined (job : PipelineJob) : MBuffer[Tag] = {
    val tags = marrayEmpty[Tag]
    if(!(job ? AndroidInterIntraProceduralModule.globalIntraResultKey)) {
      tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
        "Output error for '" + this.title + "': No entry found for 'intraResult'. Expecting (AndroidInterIntraProceduralModule.globalIntraResultKey)") 
    }

    if(job ? AndroidInterIntraProceduralModule.globalIntraResultKey && !job(AndroidInterIntraProceduralModule.globalIntraResultKey).isInstanceOf[scala.collection.mutable.Map[java.lang.String, org.sireum.amandroid.scfg.CompressedControlFlowGraph[java.lang.String]]]) {
      tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker, 
        "Output error for '" + this.title + "': Wrong type found for AndroidInterIntraProceduralModule.globalIntraResultKey.  Expecting 'scala.collection.mutable.Map[java.lang.String, org.sireum.amandroid.scfg.CompressedControlFlowGraph[java.lang.String]]' but found '" + 
        job(AndroidInterIntraProceduralModule.globalIntraResultKey).getClass.toString + "'")
    } 

    if(!(job ? AndroidInterIntraProceduralModule.globalInterResultKey)) {
      tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
        "Output error for '" + this.title + "': No entry found for 'interResult'. Expecting (AndroidInterIntraProceduralModule.globalInterResultKey)") 
    }

    if(job ? AndroidInterIntraProceduralModule.globalInterResultKey && !job(AndroidInterIntraProceduralModule.globalInterResultKey).isInstanceOf[scala.Option[org.sireum.amandroid.scfg.SystemControlFlowGraph[java.lang.String]]]) {
      tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker, 
        "Output error for '" + this.title + "': Wrong type found for AndroidInterIntraProceduralModule.globalInterResultKey.  Expecting 'scala.Option[org.sireum.amandroid.scfg.SystemControlFlowGraph[java.lang.String]]' but found '" + 
        job(AndroidInterIntraProceduralModule.globalInterResultKey).getClass.toString + "'")
    } 
    return tags
  }

  def modGetParallel (options : scala.collection.Map[Property.Key, Any]) : scala.Boolean = {
    if (options.contains(AndroidInterIntraProceduralModule.globalParallelKey)) {
       return options(AndroidInterIntraProceduralModule.globalParallelKey).asInstanceOf[scala.Boolean]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def setParallel (options : MMap[Property.Key, Any], parallel : scala.Boolean) : MMap[Property.Key, Any] = {

    options(AndroidInterIntraProceduralModule.globalParallelKey) = parallel

    return options
  }

  def modGetSymbolTable (options : scala.collection.Map[Property.Key, Any]) : org.sireum.pilar.symbol.SymbolTable = {
    if (options.contains(AndroidInterIntraProceduralModule.globalSymbolTableKey)) {
       return options(AndroidInterIntraProceduralModule.globalSymbolTableKey).asInstanceOf[org.sireum.pilar.symbol.SymbolTable]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def setSymbolTable (options : MMap[Property.Key, Any], symbolTable : org.sireum.pilar.symbol.SymbolTable) : MMap[Property.Key, Any] = {

    options(AndroidInterIntraProceduralModule.globalSymbolTableKey) = symbolTable

    return options
  }

  def modGetAndroidVirtualMethodTables (options : scala.collection.Map[Property.Key, Any]) : org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables = {
    if (options.contains(AndroidInterIntraProceduralModule.globalAndroidVirtualMethodTablesKey)) {
       return options(AndroidInterIntraProceduralModule.globalAndroidVirtualMethodTablesKey).asInstanceOf[org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def setAndroidVirtualMethodTables (options : MMap[Property.Key, Any], androidVirtualMethodTables : org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables) : MMap[Property.Key, Any] = {

    options(AndroidInterIntraProceduralModule.globalAndroidVirtualMethodTablesKey) = androidVirtualMethodTables

    return options
  }

  def modGetAndroidCache (options : scala.collection.Map[Property.Key, Any]) : org.sireum.amandroid.cache.AndroidCacheFile[java.lang.String] = {
    if (options.contains(AndroidInterIntraProceduralModule.globalAndroidCacheKey)) {
       return options(AndroidInterIntraProceduralModule.globalAndroidCacheKey).asInstanceOf[org.sireum.amandroid.cache.AndroidCacheFile[java.lang.String]]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def setAndroidCache (options : MMap[Property.Key, Any], androidCache : org.sireum.amandroid.cache.AndroidCacheFile[java.lang.String]) : MMap[Property.Key, Any] = {

    options(AndroidInterIntraProceduralModule.globalAndroidCacheKey) = androidCache

    return options
  }

  def modGetShouldBuildCCfg (options : scala.collection.Map[Property.Key, Any]) : scala.Boolean = {
    if (options.contains(AndroidInterIntraProceduralModule.globalShouldBuildCCfgKey)) {
       return options(AndroidInterIntraProceduralModule.globalShouldBuildCCfgKey).asInstanceOf[scala.Boolean]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def setShouldBuildCCfg (options : MMap[Property.Key, Any], shouldBuildCCfg : scala.Boolean) : MMap[Property.Key, Any] = {

    options(AndroidInterIntraProceduralModule.globalShouldBuildCCfgKey) = shouldBuildCCfg

    return options
  }

  def modGetShouldBuildSCfg (options : scala.collection.Map[Property.Key, Any]) : scala.Boolean = {
    if (options.contains(AndroidInterIntraProceduralModule.globalShouldBuildSCfgKey)) {
       return options(AndroidInterIntraProceduralModule.globalShouldBuildSCfgKey).asInstanceOf[scala.Boolean]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def setShouldBuildSCfg (options : MMap[Property.Key, Any], shouldBuildSCfg : scala.Boolean) : MMap[Property.Key, Any] = {

    options(AndroidInterIntraProceduralModule.globalShouldBuildSCfgKey) = shouldBuildSCfg

    return options
  }

  def modGetShouldBuildCSCfg (options : scala.collection.Map[Property.Key, Any]) : scala.Boolean = {
    if (options.contains(AndroidInterIntraProceduralModule.globalShouldBuildCSCfgKey)) {
       return options(AndroidInterIntraProceduralModule.globalShouldBuildCSCfgKey).asInstanceOf[scala.Boolean]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def setShouldBuildCSCfg (options : MMap[Property.Key, Any], shouldBuildCSCfg : scala.Boolean) : MMap[Property.Key, Any] = {

    options(AndroidInterIntraProceduralModule.globalShouldBuildCSCfgKey) = shouldBuildCSCfg

    return options
  }

  def modGetAPIpermOpt (options : scala.collection.Map[Property.Key, Any]) : scala.Option[scala.collection.mutable.Map[java.lang.String, scala.collection.mutable.ListBuffer[java.lang.String]]] = {
    if (options.contains(AndroidInterIntraProceduralModule.globalAPIpermOptKey)) {
       return options(AndroidInterIntraProceduralModule.globalAPIpermOptKey).asInstanceOf[scala.Option[scala.collection.mutable.Map[java.lang.String, scala.collection.mutable.ListBuffer[java.lang.String]]]]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def setAPIpermOpt (options : MMap[Property.Key, Any], APIpermOpt : scala.Option[scala.collection.mutable.Map[java.lang.String, scala.collection.mutable.ListBuffer[java.lang.String]]]) : MMap[Property.Key, Any] = {

    options(AndroidInterIntraProceduralModule.globalAPIpermOptKey) = APIpermOpt

    return options
  }

  def modGetShouldIncludeFlowFunction (options : scala.collection.Map[Property.Key, Any]) : scala.Function2[org.sireum.pilar.ast.LocationDecl, scala.collection.Iterable[org.sireum.pilar.ast.CatchClause], scala.Tuple2[scala.collection.Iterable[org.sireum.pilar.ast.CatchClause], scala.Boolean]] = {
    if (options.contains(AndroidInterIntraProceduralModule.globalShouldIncludeFlowFunctionKey)) {
       return options(AndroidInterIntraProceduralModule.globalShouldIncludeFlowFunctionKey).asInstanceOf[scala.Function2[org.sireum.pilar.ast.LocationDecl, scala.collection.Iterable[org.sireum.pilar.ast.CatchClause], scala.Tuple2[scala.collection.Iterable[org.sireum.pilar.ast.CatchClause], scala.Boolean]]]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def setShouldIncludeFlowFunction (options : MMap[Property.Key, Any], shouldIncludeFlowFunction : scala.Function2[org.sireum.pilar.ast.LocationDecl, scala.collection.Iterable[org.sireum.pilar.ast.CatchClause], scala.Tuple2[scala.collection.Iterable[org.sireum.pilar.ast.CatchClause], scala.Boolean]]) : MMap[Property.Key, Any] = {

    options(AndroidInterIntraProceduralModule.globalShouldIncludeFlowFunctionKey) = shouldIncludeFlowFunction

    return options
  }

  def getIntraResult (options : scala.collection.Map[Property.Key, Any]) : scala.collection.mutable.Map[java.lang.String, org.sireum.amandroid.scfg.CompressedControlFlowGraph[java.lang.String]] = {
    if (options.contains(AndroidInterIntraProceduralModule.globalIntraResultKey)) {
       return options(AndroidInterIntraProceduralModule.globalIntraResultKey).asInstanceOf[scala.collection.mutable.Map[java.lang.String, org.sireum.amandroid.scfg.CompressedControlFlowGraph[java.lang.String]]]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def modSetIntraResult (options : MMap[Property.Key, Any], intraResult : scala.collection.mutable.Map[java.lang.String, org.sireum.amandroid.scfg.CompressedControlFlowGraph[java.lang.String]]) : MMap[Property.Key, Any] = {

    options(AndroidInterIntraProceduralModule.globalIntraResultKey) = intraResult

    return options
  }

  def getInterResult (options : scala.collection.Map[Property.Key, Any]) : scala.Option[org.sireum.amandroid.scfg.SystemControlFlowGraph[java.lang.String]] = {
    if (options.contains(AndroidInterIntraProceduralModule.globalInterResultKey)) {
       return options(AndroidInterIntraProceduralModule.globalInterResultKey).asInstanceOf[scala.Option[org.sireum.amandroid.scfg.SystemControlFlowGraph[java.lang.String]]]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def modSetInterResult (options : MMap[Property.Key, Any], interResult : scala.Option[org.sireum.amandroid.scfg.SystemControlFlowGraph[java.lang.String]]) : MMap[Property.Key, Any] = {

    options(AndroidInterIntraProceduralModule.globalInterResultKey) = interResult

    return options
  }
}

trait AndroidInterIntraProceduralModule {
  def job : PipelineJob

  def parallel : scala.Boolean = AndroidInterIntraProceduralModule.modGetParallel(job.properties)

  def symbolTable : org.sireum.pilar.symbol.SymbolTable = AndroidInterIntraProceduralModule.modGetSymbolTable(job.properties)

  def androidVirtualMethodTables : org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables = AndroidInterIntraProceduralModule.modGetAndroidVirtualMethodTables(job.properties)

  def androidCache : org.sireum.amandroid.cache.AndroidCacheFile[java.lang.String] = AndroidInterIntraProceduralModule.modGetAndroidCache(job.properties)

  def shouldBuildCCfg : scala.Boolean = AndroidInterIntraProceduralModule.modGetShouldBuildCCfg(job.properties)

  def shouldBuildSCfg : scala.Boolean = AndroidInterIntraProceduralModule.modGetShouldBuildSCfg(job.properties)

  def shouldBuildCSCfg : scala.Boolean = AndroidInterIntraProceduralModule.modGetShouldBuildCSCfg(job.properties)

  def APIpermOpt : scala.Option[scala.collection.mutable.Map[java.lang.String, scala.collection.mutable.ListBuffer[java.lang.String]]] = AndroidInterIntraProceduralModule.modGetAPIpermOpt(job.properties)

  def shouldIncludeFlowFunction : scala.Function2[org.sireum.pilar.ast.LocationDecl, scala.collection.Iterable[org.sireum.pilar.ast.CatchClause], scala.Tuple2[scala.collection.Iterable[org.sireum.pilar.ast.CatchClause], scala.Boolean]] = AndroidInterIntraProceduralModule.modGetShouldIncludeFlowFunction(job.properties)


  def intraResult_=(intraResult : scala.collection.mutable.Map[java.lang.String, org.sireum.amandroid.scfg.CompressedControlFlowGraph[java.lang.String]]) { AndroidInterIntraProceduralModule.modSetIntraResult(job.properties, intraResult) }


  def interResult_=(interResult : scala.Option[org.sireum.amandroid.scfg.SystemControlFlowGraph[java.lang.String]]) { AndroidInterIntraProceduralModule.modSetInterResult(job.properties, interResult) }
}

object CfgModule extends PipelineModule {
  def title = "Control Flow Graph Builder"
  def origin = classOf[Cfg]

  val poolKey = "Cfg.pool"
  val globalProcedureSymbolTableKey = "Global.procedureSymbolTable"
  val globalPoolKey = "Global.pool"
  val cfgKey = "Cfg.cfg"
  val globalShouldIncludeFlowFunctionKey = "Global.shouldIncludeFlowFunction"
  val globalCfgKey = "Global.cfg"

  def compute(job : PipelineJob, info : PipelineJobModuleInfo) : MBuffer[Tag] = {
    val tags = marrayEmpty[Tag]
    try {
      val module = Class.forName("org.sireum.amandroid.module.CfgDef")
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
    var _procedureSymbolTable : scala.Option[AnyRef] = None
    var _procedureSymbolTableKey : scala.Option[String] = None

    val keylistprocedureSymbolTable = List(CfgModule.globalProcedureSymbolTableKey)
    keylistprocedureSymbolTable.foreach(key => 
      if(job ? key) { 
        if(_procedureSymbolTable.isEmpty) {
          _procedureSymbolTable = Some(job(key))
          _procedureSymbolTableKey = Some(key)
        }
        if(!(job(key).asInstanceOf[AnyRef] eq _procedureSymbolTable.get)) {
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': 'procedureSymbolTable' keys '" + _procedureSymbolTableKey.get + " and '" + key + "' point to different objects.")
        }
      }
    )

    _procedureSymbolTable match{
      case Some(x) =>
        if(!x.isInstanceOf[org.sireum.pilar.symbol.ProcedureSymbolTable]){
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': Wrong type found for 'procedureSymbolTable'.  Expecting 'org.sireum.pilar.symbol.ProcedureSymbolTable' but found '" + x.getClass.toString + "'")
        }
      case None =>
        tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
          "Input error for '" + this.title + "': No value found for 'procedureSymbolTable'")       
    }
    var _shouldIncludeFlowFunction : scala.Option[AnyRef] = None
    var _shouldIncludeFlowFunctionKey : scala.Option[String] = None

    val keylistshouldIncludeFlowFunction = List(CfgModule.globalShouldIncludeFlowFunctionKey)
    keylistshouldIncludeFlowFunction.foreach(key => 
      if(job ? key) { 
        if(_shouldIncludeFlowFunction.isEmpty) {
          _shouldIncludeFlowFunction = Some(job(key))
          _shouldIncludeFlowFunctionKey = Some(key)
        }
        if(!(job(key).asInstanceOf[AnyRef] eq _shouldIncludeFlowFunction.get)) {
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': 'shouldIncludeFlowFunction' keys '" + _shouldIncludeFlowFunctionKey.get + " and '" + key + "' point to different objects.")
        }
      }
    )

    _shouldIncludeFlowFunction match{
      case Some(x) =>
        if(!x.isInstanceOf[scala.Function2[org.sireum.pilar.ast.LocationDecl, scala.collection.Iterable[org.sireum.pilar.ast.CatchClause], scala.Tuple2[scala.collection.Iterable[org.sireum.pilar.ast.CatchClause], scala.Boolean]]]){
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': Wrong type found for 'shouldIncludeFlowFunction'.  Expecting 'scala.Function2[org.sireum.pilar.ast.LocationDecl, scala.collection.Iterable[org.sireum.pilar.ast.CatchClause], scala.Tuple2[scala.collection.Iterable[org.sireum.pilar.ast.CatchClause], scala.Boolean]]' but found '" + x.getClass.toString + "'")
        }
      case None =>
        tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
          "Input error for '" + this.title + "': No value found for 'shouldIncludeFlowFunction'")       
    }
    return tags
  }

  def outputDefined (job : PipelineJob) : MBuffer[Tag] = {
    val tags = marrayEmpty[Tag]
    if(!(job ? CfgModule.poolKey) && !(job ? CfgModule.globalPoolKey)) {
      tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
        "Output error for '" + this.title + "': No entry found for 'pool'. Expecting (CfgModule.poolKey or CfgModule.globalPoolKey)") 
    }

    if(job ? CfgModule.poolKey && !job(CfgModule.poolKey).isInstanceOf[scala.collection.mutable.Map[org.sireum.alir.AlirIntraProceduralNode, org.sireum.alir.AlirIntraProceduralNode]]) {
      tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker, 
        "Output error for '" + this.title + "': Wrong type found for CfgModule.poolKey.  Expecting 'scala.collection.mutable.Map[org.sireum.alir.AlirIntraProceduralNode, org.sireum.alir.AlirIntraProceduralNode]' but found '" + 
        job(CfgModule.poolKey).getClass.toString + "'")
    } 

    if(job ? CfgModule.globalPoolKey && !job(CfgModule.globalPoolKey).isInstanceOf[scala.collection.mutable.Map[org.sireum.alir.AlirIntraProceduralNode, org.sireum.alir.AlirIntraProceduralNode]]) {
      tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker, 
        "Output error for '" + this.title + "': Wrong type found for CfgModule.globalPoolKey.  Expecting 'scala.collection.mutable.Map[org.sireum.alir.AlirIntraProceduralNode, org.sireum.alir.AlirIntraProceduralNode]' but found '" + 
        job(CfgModule.globalPoolKey).getClass.toString + "'")
    } 

    if(!(job ? CfgModule.cfgKey) && !(job ? CfgModule.globalCfgKey)) {
      tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
        "Output error for '" + this.title + "': No entry found for 'cfg'. Expecting (CfgModule.cfgKey or CfgModule.globalCfgKey)") 
    }

    if(job ? CfgModule.cfgKey && !job(CfgModule.cfgKey).isInstanceOf[org.sireum.alir.ControlFlowGraph[java.lang.String]]) {
      tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker, 
        "Output error for '" + this.title + "': Wrong type found for CfgModule.cfgKey.  Expecting 'org.sireum.alir.ControlFlowGraph[java.lang.String]' but found '" + 
        job(CfgModule.cfgKey).getClass.toString + "'")
    } 

    if(job ? CfgModule.globalCfgKey && !job(CfgModule.globalCfgKey).isInstanceOf[org.sireum.alir.ControlFlowGraph[java.lang.String]]) {
      tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker, 
        "Output error for '" + this.title + "': Wrong type found for CfgModule.globalCfgKey.  Expecting 'org.sireum.alir.ControlFlowGraph[java.lang.String]' but found '" + 
        job(CfgModule.globalCfgKey).getClass.toString + "'")
    } 
    return tags
  }

  def getPool (options : scala.collection.Map[Property.Key, Any]) : scala.collection.mutable.Map[org.sireum.alir.AlirIntraProceduralNode, org.sireum.alir.AlirIntraProceduralNode] = {
    if (options.contains(CfgModule.globalPoolKey)) {
       return options(CfgModule.globalPoolKey).asInstanceOf[scala.collection.mutable.Map[org.sireum.alir.AlirIntraProceduralNode, org.sireum.alir.AlirIntraProceduralNode]]
    }
    if (options.contains(CfgModule.poolKey)) {
       return options(CfgModule.poolKey).asInstanceOf[scala.collection.mutable.Map[org.sireum.alir.AlirIntraProceduralNode, org.sireum.alir.AlirIntraProceduralNode]]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def modSetPool (options : MMap[Property.Key, Any], pool : scala.collection.mutable.Map[org.sireum.alir.AlirIntraProceduralNode, org.sireum.alir.AlirIntraProceduralNode]) : MMap[Property.Key, Any] = {

    options(CfgModule.globalPoolKey) = pool
    options(CfgModule.poolKey) = pool

    return options
  }

  def modGetProcedureSymbolTable (options : scala.collection.Map[Property.Key, Any]) : org.sireum.pilar.symbol.ProcedureSymbolTable = {
    if (options.contains(CfgModule.globalProcedureSymbolTableKey)) {
       return options(CfgModule.globalProcedureSymbolTableKey).asInstanceOf[org.sireum.pilar.symbol.ProcedureSymbolTable]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def setProcedureSymbolTable (options : MMap[Property.Key, Any], procedureSymbolTable : org.sireum.pilar.symbol.ProcedureSymbolTable) : MMap[Property.Key, Any] = {

    options(CfgModule.globalProcedureSymbolTableKey) = procedureSymbolTable

    return options
  }

  def modGetShouldIncludeFlowFunction (options : scala.collection.Map[Property.Key, Any]) : scala.Function2[org.sireum.pilar.ast.LocationDecl, scala.collection.Iterable[org.sireum.pilar.ast.CatchClause], scala.Tuple2[scala.collection.Iterable[org.sireum.pilar.ast.CatchClause], scala.Boolean]] = {
    if (options.contains(CfgModule.globalShouldIncludeFlowFunctionKey)) {
       return options(CfgModule.globalShouldIncludeFlowFunctionKey).asInstanceOf[scala.Function2[org.sireum.pilar.ast.LocationDecl, scala.collection.Iterable[org.sireum.pilar.ast.CatchClause], scala.Tuple2[scala.collection.Iterable[org.sireum.pilar.ast.CatchClause], scala.Boolean]]]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def setShouldIncludeFlowFunction (options : MMap[Property.Key, Any], shouldIncludeFlowFunction : scala.Function2[org.sireum.pilar.ast.LocationDecl, scala.collection.Iterable[org.sireum.pilar.ast.CatchClause], scala.Tuple2[scala.collection.Iterable[org.sireum.pilar.ast.CatchClause], scala.Boolean]]) : MMap[Property.Key, Any] = {

    options(CfgModule.globalShouldIncludeFlowFunctionKey) = shouldIncludeFlowFunction

    return options
  }

  def getCfg (options : scala.collection.Map[Property.Key, Any]) : org.sireum.alir.ControlFlowGraph[java.lang.String] = {
    if (options.contains(CfgModule.globalCfgKey)) {
       return options(CfgModule.globalCfgKey).asInstanceOf[org.sireum.alir.ControlFlowGraph[java.lang.String]]
    }
    if (options.contains(CfgModule.cfgKey)) {
       return options(CfgModule.cfgKey).asInstanceOf[org.sireum.alir.ControlFlowGraph[java.lang.String]]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def modSetCfg (options : MMap[Property.Key, Any], cfg : org.sireum.alir.ControlFlowGraph[java.lang.String]) : MMap[Property.Key, Any] = {

    options(CfgModule.globalCfgKey) = cfg
    options(CfgModule.cfgKey) = cfg

    return options
  }
}

trait CfgModule {
  def job : PipelineJob


  def pool_=(pool : scala.collection.mutable.Map[org.sireum.alir.AlirIntraProceduralNode, org.sireum.alir.AlirIntraProceduralNode]) { CfgModule.modSetPool(job.properties, pool) }

  def procedureSymbolTable : org.sireum.pilar.symbol.ProcedureSymbolTable = CfgModule.modGetProcedureSymbolTable(job.properties)

  def shouldIncludeFlowFunction : scala.Function2[org.sireum.pilar.ast.LocationDecl, scala.collection.Iterable[org.sireum.pilar.ast.CatchClause], scala.Tuple2[scala.collection.Iterable[org.sireum.pilar.ast.CatchClause], scala.Boolean]] = CfgModule.modGetShouldIncludeFlowFunction(job.properties)


  def cfg_=(cfg : org.sireum.alir.ControlFlowGraph[java.lang.String]) { CfgModule.modSetCfg(job.properties, cfg) }
}

object cCfgModule extends PipelineModule {
  def title = "Compressed Control Flow Graph Builder"
  def origin = classOf[cCfg]

  val globalProcedureSymbolTableKey = "Global.procedureSymbolTable"
  val cCfgKey = "cCfg.cCfg"
  val globalPoolKey = "Global.pool"
  val globalCfgKey = "Global.cfg"
  val globalCCfgKey = "Global.cCfg"

  def compute(job : PipelineJob, info : PipelineJobModuleInfo) : MBuffer[Tag] = {
    val tags = marrayEmpty[Tag]
    try {
      val module = Class.forName("org.sireum.amandroid.module.cCfgDef")
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
    val deps = ilist[PipelineModule](CfgModule, CfgModule)
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
    var _pool : scala.Option[AnyRef] = None
    var _poolKey : scala.Option[String] = None

    val keylistpool = List(cCfgModule.globalPoolKey, CfgModule.poolKey)
    keylistpool.foreach(key => 
      if(job ? key) { 
        if(_pool.isEmpty) {
          _pool = Some(job(key))
          _poolKey = Some(key)
        }
        if(!(job(key).asInstanceOf[AnyRef] eq _pool.get)) {
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': 'pool' keys '" + _poolKey.get + " and '" + key + "' point to different objects.")
        }
      }
    )

    _pool match{
      case Some(x) =>
        if(!x.isInstanceOf[scala.collection.mutable.Map[org.sireum.alir.AlirIntraProceduralNode, org.sireum.alir.AlirIntraProceduralNode]]){
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': Wrong type found for 'pool'.  Expecting 'scala.collection.mutable.Map[org.sireum.alir.AlirIntraProceduralNode, org.sireum.alir.AlirIntraProceduralNode]' but found '" + x.getClass.toString + "'")
        }
      case None =>
        tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
          "Input error for '" + this.title + "': No value found for 'pool'")       
    }
    var _procedureSymbolTable : scala.Option[AnyRef] = None
    var _procedureSymbolTableKey : scala.Option[String] = None

    val keylistprocedureSymbolTable = List(cCfgModule.globalProcedureSymbolTableKey)
    keylistprocedureSymbolTable.foreach(key => 
      if(job ? key) { 
        if(_procedureSymbolTable.isEmpty) {
          _procedureSymbolTable = Some(job(key))
          _procedureSymbolTableKey = Some(key)
        }
        if(!(job(key).asInstanceOf[AnyRef] eq _procedureSymbolTable.get)) {
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': 'procedureSymbolTable' keys '" + _procedureSymbolTableKey.get + " and '" + key + "' point to different objects.")
        }
      }
    )

    _procedureSymbolTable match{
      case Some(x) =>
        if(!x.isInstanceOf[org.sireum.pilar.symbol.ProcedureSymbolTable]){
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': Wrong type found for 'procedureSymbolTable'.  Expecting 'org.sireum.pilar.symbol.ProcedureSymbolTable' but found '" + x.getClass.toString + "'")
        }
      case None =>
        tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
          "Input error for '" + this.title + "': No value found for 'procedureSymbolTable'")       
    }
    var _cfg : scala.Option[AnyRef] = None
    var _cfgKey : scala.Option[String] = None

    val keylistcfg = List(cCfgModule.globalCfgKey, CfgModule.cfgKey)
    keylistcfg.foreach(key => 
      if(job ? key) { 
        if(_cfg.isEmpty) {
          _cfg = Some(job(key))
          _cfgKey = Some(key)
        }
        if(!(job(key).asInstanceOf[AnyRef] eq _cfg.get)) {
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': 'cfg' keys '" + _cfgKey.get + " and '" + key + "' point to different objects.")
        }
      }
    )

    _cfg match{
      case Some(x) =>
        if(!x.isInstanceOf[org.sireum.alir.ControlFlowGraph[java.lang.String]]){
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': Wrong type found for 'cfg'.  Expecting 'org.sireum.alir.ControlFlowGraph[java.lang.String]' but found '" + x.getClass.toString + "'")
        }
      case None =>
        tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
          "Input error for '" + this.title + "': No value found for 'cfg'")       
    }
    return tags
  }

  def outputDefined (job : PipelineJob) : MBuffer[Tag] = {
    val tags = marrayEmpty[Tag]
    if(!(job ? cCfgModule.cCfgKey) && !(job ? cCfgModule.globalCCfgKey)) {
      tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
        "Output error for '" + this.title + "': No entry found for 'cCfg'. Expecting (cCfgModule.cCfgKey or cCfgModule.globalCCfgKey)") 
    }

    if(job ? cCfgModule.cCfgKey && !job(cCfgModule.cCfgKey).isInstanceOf[org.sireum.amandroid.scfg.CompressedControlFlowGraph[java.lang.String]]) {
      tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker, 
        "Output error for '" + this.title + "': Wrong type found for cCfgModule.cCfgKey.  Expecting 'org.sireum.amandroid.scfg.CompressedControlFlowGraph[java.lang.String]' but found '" + 
        job(cCfgModule.cCfgKey).getClass.toString + "'")
    } 

    if(job ? cCfgModule.globalCCfgKey && !job(cCfgModule.globalCCfgKey).isInstanceOf[org.sireum.amandroid.scfg.CompressedControlFlowGraph[java.lang.String]]) {
      tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker, 
        "Output error for '" + this.title + "': Wrong type found for cCfgModule.globalCCfgKey.  Expecting 'org.sireum.amandroid.scfg.CompressedControlFlowGraph[java.lang.String]' but found '" + 
        job(cCfgModule.globalCCfgKey).getClass.toString + "'")
    } 
    return tags
  }

  def modGetPool (options : scala.collection.Map[Property.Key, Any]) : scala.collection.mutable.Map[org.sireum.alir.AlirIntraProceduralNode, org.sireum.alir.AlirIntraProceduralNode] = {
    if (options.contains(cCfgModule.globalPoolKey)) {
       return options(cCfgModule.globalPoolKey).asInstanceOf[scala.collection.mutable.Map[org.sireum.alir.AlirIntraProceduralNode, org.sireum.alir.AlirIntraProceduralNode]]
    }
    if (options.contains(CfgModule.poolKey)) {
       return options(CfgModule.poolKey).asInstanceOf[scala.collection.mutable.Map[org.sireum.alir.AlirIntraProceduralNode, org.sireum.alir.AlirIntraProceduralNode]]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def setPool (options : MMap[Property.Key, Any], pool : scala.collection.mutable.Map[org.sireum.alir.AlirIntraProceduralNode, org.sireum.alir.AlirIntraProceduralNode]) : MMap[Property.Key, Any] = {

    options(cCfgModule.globalPoolKey) = pool
    options(CfgModule.poolKey) = pool

    return options
  }

  def modGetProcedureSymbolTable (options : scala.collection.Map[Property.Key, Any]) : org.sireum.pilar.symbol.ProcedureSymbolTable = {
    if (options.contains(cCfgModule.globalProcedureSymbolTableKey)) {
       return options(cCfgModule.globalProcedureSymbolTableKey).asInstanceOf[org.sireum.pilar.symbol.ProcedureSymbolTable]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def setProcedureSymbolTable (options : MMap[Property.Key, Any], procedureSymbolTable : org.sireum.pilar.symbol.ProcedureSymbolTable) : MMap[Property.Key, Any] = {

    options(cCfgModule.globalProcedureSymbolTableKey) = procedureSymbolTable

    return options
  }

  def modGetCfg (options : scala.collection.Map[Property.Key, Any]) : org.sireum.alir.ControlFlowGraph[java.lang.String] = {
    if (options.contains(cCfgModule.globalCfgKey)) {
       return options(cCfgModule.globalCfgKey).asInstanceOf[org.sireum.alir.ControlFlowGraph[java.lang.String]]
    }
    if (options.contains(CfgModule.cfgKey)) {
       return options(CfgModule.cfgKey).asInstanceOf[org.sireum.alir.ControlFlowGraph[java.lang.String]]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def setCfg (options : MMap[Property.Key, Any], cfg : org.sireum.alir.ControlFlowGraph[java.lang.String]) : MMap[Property.Key, Any] = {

    options(cCfgModule.globalCfgKey) = cfg
    options(CfgModule.cfgKey) = cfg

    return options
  }

  def getCCfg (options : scala.collection.Map[Property.Key, Any]) : org.sireum.amandroid.scfg.CompressedControlFlowGraph[java.lang.String] = {
    if (options.contains(cCfgModule.globalCCfgKey)) {
       return options(cCfgModule.globalCCfgKey).asInstanceOf[org.sireum.amandroid.scfg.CompressedControlFlowGraph[java.lang.String]]
    }
    if (options.contains(cCfgModule.cCfgKey)) {
       return options(cCfgModule.cCfgKey).asInstanceOf[org.sireum.amandroid.scfg.CompressedControlFlowGraph[java.lang.String]]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def modSetCCfg (options : MMap[Property.Key, Any], cCfg : org.sireum.amandroid.scfg.CompressedControlFlowGraph[java.lang.String]) : MMap[Property.Key, Any] = {

    options(cCfgModule.globalCCfgKey) = cCfg
    options(cCfgModule.cCfgKey) = cCfg

    return options
  }
}

trait cCfgModule {
  def job : PipelineJob

  def pool : scala.collection.mutable.Map[org.sireum.alir.AlirIntraProceduralNode, org.sireum.alir.AlirIntraProceduralNode] = cCfgModule.modGetPool(job.properties)

  def procedureSymbolTable : org.sireum.pilar.symbol.ProcedureSymbolTable = cCfgModule.modGetProcedureSymbolTable(job.properties)

  def cfg : org.sireum.alir.ControlFlowGraph[java.lang.String] = cCfgModule.modGetCfg(job.properties)


  def cCfg_=(cCfg : org.sireum.amandroid.scfg.CompressedControlFlowGraph[java.lang.String]) { cCfgModule.modSetCCfg(job.properties, cCfg) }
}

object sCfgModule extends PipelineModule {
  def title = "System Control Flow Graph Builder"
  def origin = classOf[sCfg]

  val globalAndroidVirtualMethodTablesKey = "Global.androidVirtualMethodTables"
  val globalAndroidCacheKey = "Global.androidCache"
  val globalSCfgKey = "Global.sCfg"
  val sCfgKey = "sCfg.sCfg"
  val globalCCfgsKey = "Global.cCfgs"

  def compute(job : PipelineJob, info : PipelineJobModuleInfo) : MBuffer[Tag] = {
    val tags = marrayEmpty[Tag]
    try {
      val module = Class.forName("org.sireum.amandroid.module.sCfgDef")
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
    var _androidVirtualMethodTables : scala.Option[AnyRef] = None
    var _androidVirtualMethodTablesKey : scala.Option[String] = None

    val keylistandroidVirtualMethodTables = List(sCfgModule.globalAndroidVirtualMethodTablesKey)
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
    var _androidCache : scala.Option[AnyRef] = None
    var _androidCacheKey : scala.Option[String] = None

    val keylistandroidCache = List(sCfgModule.globalAndroidCacheKey)
    keylistandroidCache.foreach(key => 
      if(job ? key) { 
        if(_androidCache.isEmpty) {
          _androidCache = Some(job(key))
          _androidCacheKey = Some(key)
        }
        if(!(job(key).asInstanceOf[AnyRef] eq _androidCache.get)) {
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': 'androidCache' keys '" + _androidCacheKey.get + " and '" + key + "' point to different objects.")
        }
      }
    )

    _androidCache match{
      case Some(x) =>
        if(!x.isInstanceOf[org.sireum.amandroid.cache.AndroidCacheFile[java.lang.String]]){
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': Wrong type found for 'androidCache'.  Expecting 'org.sireum.amandroid.cache.AndroidCacheFile[java.lang.String]' but found '" + x.getClass.toString + "'")
        }
      case None =>
        tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
          "Input error for '" + this.title + "': No value found for 'androidCache'")       
    }
    var _cCfgs : scala.Option[AnyRef] = None
    var _cCfgsKey : scala.Option[String] = None

    val keylistcCfgs = List(sCfgModule.globalCCfgsKey)
    keylistcCfgs.foreach(key => 
      if(job ? key) { 
        if(_cCfgs.isEmpty) {
          _cCfgs = Some(job(key))
          _cCfgsKey = Some(key)
        }
        if(!(job(key).asInstanceOf[AnyRef] eq _cCfgs.get)) {
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': 'cCfgs' keys '" + _cCfgsKey.get + " and '" + key + "' point to different objects.")
        }
      }
    )

    _cCfgs match{
      case Some(x) =>
        if(!x.isInstanceOf[scala.collection.mutable.Map[java.lang.String, org.sireum.amandroid.scfg.CompressedControlFlowGraph[java.lang.String]]]){
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': Wrong type found for 'cCfgs'.  Expecting 'scala.collection.mutable.Map[java.lang.String, org.sireum.amandroid.scfg.CompressedControlFlowGraph[java.lang.String]]' but found '" + x.getClass.toString + "'")
        }
      case None =>
        tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
          "Input error for '" + this.title + "': No value found for 'cCfgs'")       
    }
    return tags
  }

  def outputDefined (job : PipelineJob) : MBuffer[Tag] = {
    val tags = marrayEmpty[Tag]
    if(!(job ? sCfgModule.sCfgKey) && !(job ? sCfgModule.globalSCfgKey)) {
      tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
        "Output error for '" + this.title + "': No entry found for 'sCfg'. Expecting (sCfgModule.sCfgKey or sCfgModule.globalSCfgKey)") 
    }

    if(job ? sCfgModule.sCfgKey && !job(sCfgModule.sCfgKey).isInstanceOf[org.sireum.amandroid.scfg.SystemControlFlowGraph[java.lang.String]]) {
      tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker, 
        "Output error for '" + this.title + "': Wrong type found for sCfgModule.sCfgKey.  Expecting 'org.sireum.amandroid.scfg.SystemControlFlowGraph[java.lang.String]' but found '" + 
        job(sCfgModule.sCfgKey).getClass.toString + "'")
    } 

    if(job ? sCfgModule.globalSCfgKey && !job(sCfgModule.globalSCfgKey).isInstanceOf[org.sireum.amandroid.scfg.SystemControlFlowGraph[java.lang.String]]) {
      tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker, 
        "Output error for '" + this.title + "': Wrong type found for sCfgModule.globalSCfgKey.  Expecting 'org.sireum.amandroid.scfg.SystemControlFlowGraph[java.lang.String]' but found '" + 
        job(sCfgModule.globalSCfgKey).getClass.toString + "'")
    } 
    return tags
  }

  def modGetAndroidVirtualMethodTables (options : scala.collection.Map[Property.Key, Any]) : org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables = {
    if (options.contains(sCfgModule.globalAndroidVirtualMethodTablesKey)) {
       return options(sCfgModule.globalAndroidVirtualMethodTablesKey).asInstanceOf[org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def setAndroidVirtualMethodTables (options : MMap[Property.Key, Any], androidVirtualMethodTables : org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables) : MMap[Property.Key, Any] = {

    options(sCfgModule.globalAndroidVirtualMethodTablesKey) = androidVirtualMethodTables

    return options
  }

  def modGetAndroidCache (options : scala.collection.Map[Property.Key, Any]) : org.sireum.amandroid.cache.AndroidCacheFile[java.lang.String] = {
    if (options.contains(sCfgModule.globalAndroidCacheKey)) {
       return options(sCfgModule.globalAndroidCacheKey).asInstanceOf[org.sireum.amandroid.cache.AndroidCacheFile[java.lang.String]]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def setAndroidCache (options : MMap[Property.Key, Any], androidCache : org.sireum.amandroid.cache.AndroidCacheFile[java.lang.String]) : MMap[Property.Key, Any] = {

    options(sCfgModule.globalAndroidCacheKey) = androidCache

    return options
  }

  def getSCfg (options : scala.collection.Map[Property.Key, Any]) : org.sireum.amandroid.scfg.SystemControlFlowGraph[java.lang.String] = {
    if (options.contains(sCfgModule.globalSCfgKey)) {
       return options(sCfgModule.globalSCfgKey).asInstanceOf[org.sireum.amandroid.scfg.SystemControlFlowGraph[java.lang.String]]
    }
    if (options.contains(sCfgModule.sCfgKey)) {
       return options(sCfgModule.sCfgKey).asInstanceOf[org.sireum.amandroid.scfg.SystemControlFlowGraph[java.lang.String]]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def modSetSCfg (options : MMap[Property.Key, Any], sCfg : org.sireum.amandroid.scfg.SystemControlFlowGraph[java.lang.String]) : MMap[Property.Key, Any] = {

    options(sCfgModule.globalSCfgKey) = sCfg
    options(sCfgModule.sCfgKey) = sCfg

    return options
  }

  def modGetCCfgs (options : scala.collection.Map[Property.Key, Any]) : scala.collection.mutable.Map[java.lang.String, org.sireum.amandroid.scfg.CompressedControlFlowGraph[java.lang.String]] = {
    if (options.contains(sCfgModule.globalCCfgsKey)) {
       return options(sCfgModule.globalCCfgsKey).asInstanceOf[scala.collection.mutable.Map[java.lang.String, org.sireum.amandroid.scfg.CompressedControlFlowGraph[java.lang.String]]]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def setCCfgs (options : MMap[Property.Key, Any], cCfgs : scala.collection.mutable.Map[java.lang.String, org.sireum.amandroid.scfg.CompressedControlFlowGraph[java.lang.String]]) : MMap[Property.Key, Any] = {

    options(sCfgModule.globalCCfgsKey) = cCfgs

    return options
  }
}

trait sCfgModule {
  def job : PipelineJob

  def androidVirtualMethodTables : org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables = sCfgModule.modGetAndroidVirtualMethodTables(job.properties)

  def androidCache : org.sireum.amandroid.cache.AndroidCacheFile[java.lang.String] = sCfgModule.modGetAndroidCache(job.properties)


  def sCfg_=(sCfg : org.sireum.amandroid.scfg.SystemControlFlowGraph[java.lang.String]) { sCfgModule.modSetSCfg(job.properties, sCfg) }

  def cCfgs : scala.collection.mutable.Map[java.lang.String, org.sireum.amandroid.scfg.CompressedControlFlowGraph[java.lang.String]] = sCfgModule.modGetCCfgs(job.properties)
}

object csCfgModule extends PipelineModule {
  def title = "Compressed System Control Flow Graph Builder"
  def origin = classOf[csCfg]

  val globalAndroidVirtualMethodTablesKey = "Global.androidVirtualMethodTables"
  val globalCsCfgKey = "Global.csCfg"
  val globalAndroidCacheKey = "Global.androidCache"
  val globalAPIpermKey = "Global.APIperm"
  val csCfgKey = "csCfg.csCfg"
  val globalSCfgKey = "Global.sCfg"
  val globalCCfgsKey = "Global.cCfgs"

  def compute(job : PipelineJob, info : PipelineJobModuleInfo) : MBuffer[Tag] = {
    val tags = marrayEmpty[Tag]
    try {
      val module = Class.forName("org.sireum.amandroid.module.csCfgDef")
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
    var _androidVirtualMethodTables : scala.Option[AnyRef] = None
    var _androidVirtualMethodTablesKey : scala.Option[String] = None

    val keylistandroidVirtualMethodTables = List(csCfgModule.globalAndroidVirtualMethodTablesKey)
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
    var _androidCache : scala.Option[AnyRef] = None
    var _androidCacheKey : scala.Option[String] = None

    val keylistandroidCache = List(csCfgModule.globalAndroidCacheKey)
    keylistandroidCache.foreach(key => 
      if(job ? key) { 
        if(_androidCache.isEmpty) {
          _androidCache = Some(job(key))
          _androidCacheKey = Some(key)
        }
        if(!(job(key).asInstanceOf[AnyRef] eq _androidCache.get)) {
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': 'androidCache' keys '" + _androidCacheKey.get + " and '" + key + "' point to different objects.")
        }
      }
    )

    _androidCache match{
      case Some(x) =>
        if(!x.isInstanceOf[org.sireum.amandroid.cache.AndroidCacheFile[java.lang.String]]){
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': Wrong type found for 'androidCache'.  Expecting 'org.sireum.amandroid.cache.AndroidCacheFile[java.lang.String]' but found '" + x.getClass.toString + "'")
        }
      case None =>
        tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
          "Input error for '" + this.title + "': No value found for 'androidCache'")       
    }
    var _sCfg : scala.Option[AnyRef] = None
    var _sCfgKey : scala.Option[String] = None

    val keylistsCfg = List(csCfgModule.globalSCfgKey)
    keylistsCfg.foreach(key => 
      if(job ? key) { 
        if(_sCfg.isEmpty) {
          _sCfg = Some(job(key))
          _sCfgKey = Some(key)
        }
        if(!(job(key).asInstanceOf[AnyRef] eq _sCfg.get)) {
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': 'sCfg' keys '" + _sCfgKey.get + " and '" + key + "' point to different objects.")
        }
      }
    )

    _sCfg match{
      case Some(x) =>
        if(!x.isInstanceOf[org.sireum.amandroid.scfg.SystemControlFlowGraph[java.lang.String]]){
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': Wrong type found for 'sCfg'.  Expecting 'org.sireum.amandroid.scfg.SystemControlFlowGraph[java.lang.String]' but found '" + x.getClass.toString + "'")
        }
      case None =>
        tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
          "Input error for '" + this.title + "': No value found for 'sCfg'")       
    }
    var _cCfgs : scala.Option[AnyRef] = None
    var _cCfgsKey : scala.Option[String] = None

    val keylistcCfgs = List(csCfgModule.globalCCfgsKey)
    keylistcCfgs.foreach(key => 
      if(job ? key) { 
        if(_cCfgs.isEmpty) {
          _cCfgs = Some(job(key))
          _cCfgsKey = Some(key)
        }
        if(!(job(key).asInstanceOf[AnyRef] eq _cCfgs.get)) {
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': 'cCfgs' keys '" + _cCfgsKey.get + " and '" + key + "' point to different objects.")
        }
      }
    )

    _cCfgs match{
      case Some(x) =>
        if(!x.isInstanceOf[scala.collection.mutable.Map[java.lang.String, org.sireum.amandroid.scfg.CompressedControlFlowGraph[java.lang.String]]]){
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': Wrong type found for 'cCfgs'.  Expecting 'scala.collection.mutable.Map[java.lang.String, org.sireum.amandroid.scfg.CompressedControlFlowGraph[java.lang.String]]' but found '" + x.getClass.toString + "'")
        }
      case None =>
        tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
          "Input error for '" + this.title + "': No value found for 'cCfgs'")       
    }
    var _APIperm : scala.Option[AnyRef] = None
    var _APIpermKey : scala.Option[String] = None

    val keylistAPIperm = List(csCfgModule.globalAPIpermKey)
    keylistAPIperm.foreach(key => 
      if(job ? key) { 
        if(_APIperm.isEmpty) {
          _APIperm = Some(job(key))
          _APIpermKey = Some(key)
        }
        if(!(job(key).asInstanceOf[AnyRef] eq _APIperm.get)) {
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': 'APIperm' keys '" + _APIpermKey.get + " and '" + key + "' point to different objects.")
        }
      }
    )

    _APIperm match{
      case Some(x) =>
        if(!x.isInstanceOf[scala.collection.mutable.Map[java.lang.String, scala.collection.mutable.ListBuffer[java.lang.String]]]){
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': Wrong type found for 'APIperm'.  Expecting 'scala.collection.mutable.Map[java.lang.String, scala.collection.mutable.ListBuffer[java.lang.String]]' but found '" + x.getClass.toString + "'")
        }
      case None =>
        tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
          "Input error for '" + this.title + "': No value found for 'APIperm'")       
    }
    return tags
  }

  def outputDefined (job : PipelineJob) : MBuffer[Tag] = {
    val tags = marrayEmpty[Tag]
    if(!(job ? csCfgModule.csCfgKey) && !(job ? csCfgModule.globalCsCfgKey)) {
      tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
        "Output error for '" + this.title + "': No entry found for 'csCfg'. Expecting (csCfgModule.csCfgKey or csCfgModule.globalCsCfgKey)") 
    }

    if(job ? csCfgModule.csCfgKey && !job(csCfgModule.csCfgKey).isInstanceOf[org.sireum.amandroid.scfg.SystemControlFlowGraph[java.lang.String]]) {
      tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker, 
        "Output error for '" + this.title + "': Wrong type found for csCfgModule.csCfgKey.  Expecting 'org.sireum.amandroid.scfg.SystemControlFlowGraph[java.lang.String]' but found '" + 
        job(csCfgModule.csCfgKey).getClass.toString + "'")
    } 

    if(job ? csCfgModule.globalCsCfgKey && !job(csCfgModule.globalCsCfgKey).isInstanceOf[org.sireum.amandroid.scfg.SystemControlFlowGraph[java.lang.String]]) {
      tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker, 
        "Output error for '" + this.title + "': Wrong type found for csCfgModule.globalCsCfgKey.  Expecting 'org.sireum.amandroid.scfg.SystemControlFlowGraph[java.lang.String]' but found '" + 
        job(csCfgModule.globalCsCfgKey).getClass.toString + "'")
    } 
    return tags
  }

  def modGetAndroidVirtualMethodTables (options : scala.collection.Map[Property.Key, Any]) : org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables = {
    if (options.contains(csCfgModule.globalAndroidVirtualMethodTablesKey)) {
       return options(csCfgModule.globalAndroidVirtualMethodTablesKey).asInstanceOf[org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def setAndroidVirtualMethodTables (options : MMap[Property.Key, Any], androidVirtualMethodTables : org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables) : MMap[Property.Key, Any] = {

    options(csCfgModule.globalAndroidVirtualMethodTablesKey) = androidVirtualMethodTables

    return options
  }

  def modGetAndroidCache (options : scala.collection.Map[Property.Key, Any]) : org.sireum.amandroid.cache.AndroidCacheFile[java.lang.String] = {
    if (options.contains(csCfgModule.globalAndroidCacheKey)) {
       return options(csCfgModule.globalAndroidCacheKey).asInstanceOf[org.sireum.amandroid.cache.AndroidCacheFile[java.lang.String]]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def setAndroidCache (options : MMap[Property.Key, Any], androidCache : org.sireum.amandroid.cache.AndroidCacheFile[java.lang.String]) : MMap[Property.Key, Any] = {

    options(csCfgModule.globalAndroidCacheKey) = androidCache

    return options
  }

  def modGetSCfg (options : scala.collection.Map[Property.Key, Any]) : org.sireum.amandroid.scfg.SystemControlFlowGraph[java.lang.String] = {
    if (options.contains(csCfgModule.globalSCfgKey)) {
       return options(csCfgModule.globalSCfgKey).asInstanceOf[org.sireum.amandroid.scfg.SystemControlFlowGraph[java.lang.String]]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def setSCfg (options : MMap[Property.Key, Any], sCfg : org.sireum.amandroid.scfg.SystemControlFlowGraph[java.lang.String]) : MMap[Property.Key, Any] = {

    options(csCfgModule.globalSCfgKey) = sCfg

    return options
  }

  def modGetCCfgs (options : scala.collection.Map[Property.Key, Any]) : scala.collection.mutable.Map[java.lang.String, org.sireum.amandroid.scfg.CompressedControlFlowGraph[java.lang.String]] = {
    if (options.contains(csCfgModule.globalCCfgsKey)) {
       return options(csCfgModule.globalCCfgsKey).asInstanceOf[scala.collection.mutable.Map[java.lang.String, org.sireum.amandroid.scfg.CompressedControlFlowGraph[java.lang.String]]]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def setCCfgs (options : MMap[Property.Key, Any], cCfgs : scala.collection.mutable.Map[java.lang.String, org.sireum.amandroid.scfg.CompressedControlFlowGraph[java.lang.String]]) : MMap[Property.Key, Any] = {

    options(csCfgModule.globalCCfgsKey) = cCfgs

    return options
  }

  def getCsCfg (options : scala.collection.Map[Property.Key, Any]) : org.sireum.amandroid.scfg.SystemControlFlowGraph[java.lang.String] = {
    if (options.contains(csCfgModule.globalCsCfgKey)) {
       return options(csCfgModule.globalCsCfgKey).asInstanceOf[org.sireum.amandroid.scfg.SystemControlFlowGraph[java.lang.String]]
    }
    if (options.contains(csCfgModule.csCfgKey)) {
       return options(csCfgModule.csCfgKey).asInstanceOf[org.sireum.amandroid.scfg.SystemControlFlowGraph[java.lang.String]]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def modSetCsCfg (options : MMap[Property.Key, Any], csCfg : org.sireum.amandroid.scfg.SystemControlFlowGraph[java.lang.String]) : MMap[Property.Key, Any] = {

    options(csCfgModule.globalCsCfgKey) = csCfg
    options(csCfgModule.csCfgKey) = csCfg

    return options
  }

  def modGetAPIperm (options : scala.collection.Map[Property.Key, Any]) : scala.collection.mutable.Map[java.lang.String, scala.collection.mutable.ListBuffer[java.lang.String]] = {
    if (options.contains(csCfgModule.globalAPIpermKey)) {
       return options(csCfgModule.globalAPIpermKey).asInstanceOf[scala.collection.mutable.Map[java.lang.String, scala.collection.mutable.ListBuffer[java.lang.String]]]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def setAPIperm (options : MMap[Property.Key, Any], APIperm : scala.collection.mutable.Map[java.lang.String, scala.collection.mutable.ListBuffer[java.lang.String]]) : MMap[Property.Key, Any] = {

    options(csCfgModule.globalAPIpermKey) = APIperm

    return options
  }
}

trait csCfgModule {
  def job : PipelineJob

  def androidVirtualMethodTables : org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables = csCfgModule.modGetAndroidVirtualMethodTables(job.properties)

  def androidCache : org.sireum.amandroid.cache.AndroidCacheFile[java.lang.String] = csCfgModule.modGetAndroidCache(job.properties)

  def sCfg : org.sireum.amandroid.scfg.SystemControlFlowGraph[java.lang.String] = csCfgModule.modGetSCfg(job.properties)

  def cCfgs : scala.collection.mutable.Map[java.lang.String, org.sireum.amandroid.scfg.CompressedControlFlowGraph[java.lang.String]] = csCfgModule.modGetCCfgs(job.properties)


  def csCfg_=(csCfg : org.sireum.amandroid.scfg.SystemControlFlowGraph[java.lang.String]) { csCfgModule.modSetCsCfg(job.properties, csCfg) }

  def APIperm : scala.collection.mutable.Map[java.lang.String, scala.collection.mutable.ListBuffer[java.lang.String]] = csCfgModule.modGetAPIperm(job.properties)
}