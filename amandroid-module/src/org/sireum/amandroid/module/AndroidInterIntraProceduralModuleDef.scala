package org.sireum.amandroid.module

import org.sireum.pipeline.PipelineJob
import org.sireum.pipeline.PipelineJobModuleInfo
import org.sireum.alir.ControlFlowGraph
import org.sireum.util._
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.alir.AlirIntraProceduralGraph
import org.sireum.amandroid.scfg.CompressedControlFlowGraph
import org.sireum.amandroid.scfg.SystemControlFlowGraph
import java.io._
import org.sireum.amandroid.xml.AndroidXStream
import org.sireum.pipeline.ErrorneousModulesThrowable
import org.sireum.pipeline.PipelineStage
import org.sireum.pipeline.PipelineConfiguration
import org.sireum.alir.ReachingDefinitionAnalysis
import org.sireum.amandroid.objectflowanalysis.ObjectFlowGraphPreprocessor
import org.sireum.amandroid.objectflowanalysis.ObjectFlowGraph
import org.sireum.amandroid.objectflowanalysis.OfaNode
import org.sireum.amandroid.scfg.ObjectFlowGraphAndSystemControlFlowGraphBuilder


/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */

class AndroidInterIntraProceduralModuleDef (val job : PipelineJob, info : PipelineJobModuleInfo) 
  extends AndroidInterIntraProceduralModule with ImplicitLogging{
  // add implementation here
  type VirtualLabel = String
  val par = this.parallel
  val aCache = this.androidCache
  val alitOpt = this.androidLibInfoTablesOpt
  val alit = alitOpt match{case Some(n) => n; case None => null}
  val st = this.symbolTable
  val psts = st.procedureSymbolTables.toSeq
  val siff = this.shouldIncludeFlowFunction
  //for building rda
  val dr = this.defRef
  val iopp = this.isInputOutputParamPredicate
  val saom = this.switchAsOrderedMatch
  //for prepare application
  val apkFLOpt = this.apkFileLocationOpt 
  val apkFL = apkFLOpt match{case Some(n) => n; case None => null}
  //////end here
  //for inter procedure analysis building
  val cfgs : MMap[ResourceUri, ControlFlowGraph[String]] = mmapEmpty
  val cCfgs : MMap[ResourceUri, CompressedControlFlowGraph[String]] = mmapEmpty
  val rdas : MMap[ResourceUri, ReachingDefinitionAnalysis.Result] = mmapEmpty
  
  val intraPipeline = buildIntraPipeline(job)
  val interPipeline = buildInterPipeline(job)
  
  def compute(pst : ProcedureSymbolTable) = {
    val j = PipelineJob()
    val options = j.properties
    CfgModule.setProcedureSymbolTable(options, pst)
    CfgModule.setShouldIncludeFlowFunction(options, siff)
    AndroidInterIntraProceduralModule.setDefRef(options, dr)
    AndroidInterIntraProceduralModule.setIsInputOutputParamPredicate(options, iopp)
    AndroidInterIntraProceduralModule.setSwitchAsOrderedMatch(options, saom)
    if(this.shouldBuildRda) RdaModule.setAndroidLibInfoTables(options, alit)
    if(this.shouldPreprocessOfg) OFAPreprocessModule.setAndroidLibInfoTables(options, alit)
    intraPipeline.compute(j)
    info.hasError = j.hasError
    if (info.hasError) {
      info.exception = Some(ErrorneousModulesThrowable(
        j.lastStageInfo.info.filter { i => i.hasError }))
    }
//    println(Tag.collateAsString(j.lastStageInfo.tags))
    val pool = CfgModule.getPool(options)
    val cfg = CfgModule.getCfg(options)
    val rdaOpt = if (this.shouldBuildRda) Some(RdaModule.getRda(options)) else None
    val ofgOpt = if (this.shouldPreprocessOfg) Some(OFAPreprocessModule.getOFG(options)) else None
    val cCfgOpt = if (this.shouldBuildCCfg) Some(cCfgModule.getCCfg(options)) else None

    Map(pst.procedureUri ->
      AndroidInterIntraProcedural.AndroidIntraAnalysisResult(
        pool, cfg, rdaOpt, ofgOpt, cCfgOpt
      ))
  }
  
  val intraResults : MMap[ResourceUri, AndroidInterIntraProcedural.AndroidIntraAnalysisResult] = mmapEmpty
  println("par=" + par)
  val col : GenSeq[ProcedureSymbolTable] = if (par) psts.par else psts
  println("intra analysis start! procudure number = " + col.size)
  val starttime = System.currentTimeMillis();
  
  def combine(tp1 : IMap[ResourceUri, AndroidInterIntraProcedural.AndroidIntraAnalysisResult],
      tp2 : IMap[ResourceUri, AndroidInterIntraProcedural.AndroidIntraAnalysisResult])
      : IMap[ResourceUri, AndroidInterIntraProcedural.AndroidIntraAnalysisResult] = {
    tp1 ++ tp2
  }
  
  intraResults ++= 
    (col.map { pst =>
      compute(pst)
    }).reduce(combine)

  this.intraResult_=(intraResults)
  println("intra building complete!")
  
  val endtime = System.currentTimeMillis();
  logger.info(s"End computing: Time: ${(endtime - starttime) / 1000d} s")
  
  
  if(this.shouldBuildOFAsCfg) {
    intraResults.foreach(
      item => {
        val pUri = item._1
        cfgs(pUri) = item._2.cfg
        item._2.rdaOpt match {
          case Some(rda) => rdas(pUri) = rda
          case None =>
        }
	    item._2.cCfgOpt match {
	      case Some(cCfg) => cCfgs(pUri) = cCfg
	      case None =>
	    }
      }  
    )
  }
  
  /**
   * below is the inter procedure analysis
   */
  def compute() = {
    val j = PipelineJob()
    val options = j.properties
    if(this.shouldBuildOFAsCfg) {
    	OFAsCfgModule.setApkFileLocation(options, apkFL)
      OFAsCfgModule.setAndroidCache(options, aCache)
      OFAsCfgModule.setAndroidLibInfoTables(options, alit)
      OFAsCfgModule.setCfgs(options, cfgs)
      OFAsCfgModule.setCCfgs(options, cCfgs)
      OFAsCfgModule.setProcedureSymbolTables(options, psts)
      OFAsCfgModule.setRdas(options, rdas)
      
      interPipeline.compute(j)
      info.hasError = j.hasError
      if (info.hasError) {
        info.exception = Some(ErrorneousModulesThrowable(
          j.lastStageInfo.info.filter { i => i.hasError }))
      }
//      println(Tag.collateAsString(j.lastStageInfo.tags))
      val ofaScfg = if (this.shouldBuildOFAsCfg) Some(OFAsCfgModule.getOFAsCfg(options)) else None
      this.interResult_=(AndroidInterIntraProcedural.AndroidInterAnalysisResult(
          ofaScfg
        ))
    }  else {
      this.interResult_=(AndroidInterIntraProcedural.AndroidInterAnalysisResult(
          None
        ))
    }
  }
  
  println("inter analysis start! procudure number = " + col.size)
  compute()
  println("inter building complete!")
  
  def buildIntraPipeline(job : PipelineJob) = {
    val stages = marrayEmpty[PipelineStage]

    if (this.shouldBuildCfg || this.shouldBuildRda || this.shouldPreprocessOfg || this.shouldBuildCCfg || this.shouldBuildOFAsCfg){
      stages += PipelineStage("CFG Building", false, CfgModule)
    }
    
    if (this.shouldBuildRda || this.shouldPreprocessOfg || this.shouldBuildOFAsCfg){
      stages += PipelineStage("RDA Building", false, RdaModule)
    }
    
    if (this.shouldPreprocessOfg){
      stages += PipelineStage("OFA Preprocess", false, OFAPreprocessModule)
    }
    
    if (this.shouldBuildCCfg || this.shouldBuildOFAsCfg){
      stages += PipelineStage("CCFG Building", false, cCfgModule)
    }

    PipelineConfiguration("Android Intraprocedural Analysis Pipeline",
      false, stages : _*)
  }
  
  def buildInterPipeline(job : PipelineJob) = {
    val stages = marrayEmpty[PipelineStage]
    
    if (this.shouldBuildOFAsCfg) 
      stages += PipelineStage("OFASCFG Building", false, OFAsCfgModule)
      
    PipelineConfiguration("Android Interprocedural Analysis Pipeline",
      false, stages : _*)
  }
  
}


class CfgModuleDef (val job : PipelineJob, info : PipelineJobModuleInfo) extends CfgModule {
  val ENTRY_NODE_LABEL = "Entry"
  val EXIT_NODE_LABEL = "Exit"
  val pl : AlirIntraProceduralGraph.NodePool = mmapEmpty
  val pst = this.procedureSymbolTable
  val siff = this.shouldIncludeFlowFunction
  this.pool_=(pl)
  val result = ControlFlowGraph[String](pst, ENTRY_NODE_LABEL, EXIT_NODE_LABEL, pl, siff)
  this.cfg_=(result)
}

class RdaModuleDef (val job : PipelineJob, info : PipelineJobModuleInfo) extends RdaModule {
  val pst = this.procedureSymbolTable
  val iiopp = this.isInputOutputParamPredicate(pst)
  val alit = this.androidLibInfoTables
  this.rda_=(ReachingDefinitionAnalysis[String](pst,
    this.cfg,
    this.defRef(pst.symbolTable, alit),
    first2(iiopp),
    this.switchAsOrderedMatch))
}

class OFAPreprocessModuleDef (val job : PipelineJob, info : PipelineJobModuleInfo) extends OFAPreprocessModule {
  this.OFG_=(ObjectFlowGraphPreprocessor(this.procedureSymbolTable, this.cfg, this.rda, this.androidLibInfoTables))
}

class cCfgModuleDef (val job : PipelineJob, info : PipelineJobModuleInfo) extends cCfgModule {
  this.cCfg_=(CompressedControlFlowGraph[String](this.procedureSymbolTable, this.pool, this.cfg))
}

class OFAsCfgModuleDef (val job : PipelineJob, info : PipelineJobModuleInfo) extends OFAsCfgModule {
  val results : MMap[ResourceUri, (ObjectFlowGraph[OfaNode], SystemControlFlowGraph[String])] = mmapEmpty
  this.androidCache match{
    case Some(ac) =>
      results ++= new ObjectFlowGraphAndSystemControlFlowGraphBuilder[OfaNode, String].apply(this.procedureSymbolTables, this.cfgs, this.rdas, this.cCfgs, this.androidLibInfoTables, ac, this.apkFileLocation)
    case None =>
  }
  this.OFAsCfg_=(results)
}
