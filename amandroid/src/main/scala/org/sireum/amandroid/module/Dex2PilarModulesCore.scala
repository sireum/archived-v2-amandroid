package org.sireum.amandroid.module

import org.sireum.util._
import org.sireum.pipeline._
import java.lang.String
import scala.Option
import scala.collection.Seq
import scala.collection.immutable.Seq

object Dex2PilarWrapperModule extends PipelineModule {
  def title = "Dex2Pilar Wrapper Module"
  def origin = classOf[Dex2PilarWrapper]

  val globalDestDirKey = "Global.destDir"
  val globalSrcFilesKey = "Global.srcFiles"
  val sourcesKey = "Dex2PilarWrapper.sources"
  val globalSourcesKey = "Global.sources"

  def compute(job : PipelineJob, info : PipelineJobModuleInfo) : MBuffer[Tag] = {
    val tags = marrayEmpty[Tag]
    try {
      val module = Class.forName("org.sireum.amandroid.module.Dex2PilarWrapperDef")
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
    if(!(job ? Dex2PilarWrapperModule.globalDestDirKey)) {
      val destDir = Class.forName("org.sireum.amandroid.module.Dex2PilarWrapper").getDeclaredMethod("$lessinit$greater$default$3").invoke(null).asInstanceOf[scala.Option[java.lang.String]]
      setDestDir(job.properties, destDir)
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
    var _srcFiles : scala.Option[AnyRef] = None
    var _srcFilesKey : scala.Option[String] = None

    val keylistsrcFiles = List(Dex2PilarWrapperModule.globalSrcFilesKey)
    keylistsrcFiles.foreach(key => 
      if(job ? key) { 
        if(_srcFiles.isEmpty) {
          _srcFiles = Some(job(key))
          _srcFilesKey = Some(key)
        }
        if(!(job(key).asInstanceOf[AnyRef] eq _srcFiles.get)) {
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': 'srcFiles' keys '" + _srcFilesKey.get + " and '" + key + "' point to different objects.")
        }
      }
    )

    _srcFiles match{
      case Some(x) =>
        if(!x.isInstanceOf[scala.collection.Seq[java.lang.String]]){
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': Wrong type found for 'srcFiles'.  Expecting 'scala.collection.Seq[java.lang.String]' but found '" + x.getClass.toString + "'")
        }
      case None =>
        tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
          "Input error for '" + this.title + "': No value found for 'srcFiles'")       
    }
    var _destDir : scala.Option[AnyRef] = None
    var _destDirKey : scala.Option[String] = None

    val keylistdestDir = List(Dex2PilarWrapperModule.globalDestDirKey)
    keylistdestDir.foreach(key => 
      if(job ? key) { 
        if(_destDir.isEmpty) {
          _destDir = Some(job(key))
          _destDirKey = Some(key)
        }
        if(!(job(key).asInstanceOf[AnyRef] eq _destDir.get)) {
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': 'destDir' keys '" + _destDirKey.get + " and '" + key + "' point to different objects.")
        }
      }
    )

    _destDir match{
      case Some(x) =>
        if(!x.isInstanceOf[scala.Option[java.lang.String]]){
          tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
            "Input error for '" + this.title + "': Wrong type found for 'destDir'.  Expecting 'scala.Option[java.lang.String]' but found '" + x.getClass.toString + "'")
        }
      case None =>
        tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
          "Input error for '" + this.title + "': No value found for 'destDir'")       
    }
    return tags
  }

  def outputDefined (job : PipelineJob) : MBuffer[Tag] = {
    val tags = marrayEmpty[Tag]
    if(!(job ? Dex2PilarWrapperModule.sourcesKey) && !(job ? Dex2PilarWrapperModule.globalSourcesKey)) {
      tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker,
        "Output error for '" + this.title + "': No entry found for 'sources'. Expecting (Dex2PilarWrapperModule.sourcesKey or Dex2PilarWrapperModule.globalSourcesKey)") 
    }

    if(job ? Dex2PilarWrapperModule.sourcesKey && !job(Dex2PilarWrapperModule.sourcesKey).isInstanceOf[scala.collection.immutable.Seq[scala.util.Either[java.lang.String, java.lang.String]]]) {
      tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker, 
        "Output error for '" + this.title + "': Wrong type found for Dex2PilarWrapperModule.sourcesKey.  Expecting 'scala.collection.immutable.Seq[scala.util.Either[java.lang.String, java.lang.String]]' but found '" + 
        job(Dex2PilarWrapperModule.sourcesKey).getClass.toString + "'")
    } 

    if(job ? Dex2PilarWrapperModule.globalSourcesKey && !job(Dex2PilarWrapperModule.globalSourcesKey).isInstanceOf[scala.collection.immutable.Seq[scala.util.Either[java.lang.String, java.lang.String]]]) {
      tags += PipelineUtil.genTag(PipelineUtil.ErrorMarker, 
        "Output error for '" + this.title + "': Wrong type found for Dex2PilarWrapperModule.globalSourcesKey.  Expecting 'scala.collection.immutable.Seq[scala.util.Either[java.lang.String, java.lang.String]]' but found '" + 
        job(Dex2PilarWrapperModule.globalSourcesKey).getClass.toString + "'")
    } 
    return tags
  }

  def modGetSrcFiles (options : scala.collection.Map[Property.Key, Any]) : scala.collection.Seq[java.lang.String] = {
    if (options.contains(Dex2PilarWrapperModule.globalSrcFilesKey)) {
       return options(Dex2PilarWrapperModule.globalSrcFilesKey).asInstanceOf[scala.collection.Seq[java.lang.String]]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def setSrcFiles (options : MMap[Property.Key, Any], srcFiles : scala.collection.Seq[java.lang.String]) : MMap[Property.Key, Any] = {

    options(Dex2PilarWrapperModule.globalSrcFilesKey) = srcFiles

    return options
  }

  def modGetDestDir (options : scala.collection.Map[Property.Key, Any]) : scala.Option[java.lang.String] = {
    if (options.contains(Dex2PilarWrapperModule.globalDestDirKey)) {
       return options(Dex2PilarWrapperModule.globalDestDirKey).asInstanceOf[scala.Option[java.lang.String]]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def setDestDir (options : MMap[Property.Key, Any], destDir : scala.Option[java.lang.String]) : MMap[Property.Key, Any] = {

    options(Dex2PilarWrapperModule.globalDestDirKey) = destDir

    return options
  }

  def getSources (options : scala.collection.Map[Property.Key, Any]) : scala.collection.immutable.Seq[scala.util.Either[java.lang.String, java.lang.String]] = {
    if (options.contains(Dex2PilarWrapperModule.globalSourcesKey)) {
       return options(Dex2PilarWrapperModule.globalSourcesKey).asInstanceOf[scala.collection.immutable.Seq[scala.util.Either[java.lang.String, java.lang.String]]]
    }
    if (options.contains(Dex2PilarWrapperModule.sourcesKey)) {
       return options(Dex2PilarWrapperModule.sourcesKey).asInstanceOf[scala.collection.immutable.Seq[scala.util.Either[java.lang.String, java.lang.String]]]
    }

    throw new Exception("Pipeline checker should guarantee we never reach here")
  }

  def modSetSources (options : MMap[Property.Key, Any], sources : scala.collection.immutable.Seq[scala.util.Either[java.lang.String, java.lang.String]]) : MMap[Property.Key, Any] = {

    options(Dex2PilarWrapperModule.globalSourcesKey) = sources
    options(Dex2PilarWrapperModule.sourcesKey) = sources

    return options
  }
}

trait Dex2PilarWrapperModule {
  def job : PipelineJob

  def srcFiles : scala.collection.Seq[java.lang.String] = Dex2PilarWrapperModule.modGetSrcFiles(job.properties)

  def destDir : scala.Option[java.lang.String] = Dex2PilarWrapperModule.modGetDestDir(job.properties)


  def sources_=(sources : scala.collection.immutable.Seq[scala.util.Either[java.lang.String, java.lang.String]]) { Dex2PilarWrapperModule.modSetSources(job.properties, sources) }
}