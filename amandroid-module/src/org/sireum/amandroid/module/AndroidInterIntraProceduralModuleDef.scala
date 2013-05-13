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
import org.sireum.amandroid.objectflowanalysis.ObjectFlowGraphBuilder
import org.sireum.alir.ReachingDefinitionAnalysis
import org.sireum.amandroid.objectflowanalysis.ObjectFlowGraphPreprocessor


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
  val avmt = this.androidLibInfoTables
  val st = this.symbolTable
  val psts = st.procedureSymbolTables.toSeq
  val siff = this.shouldIncludeFlowFunction
  //for building rda
  val dr = this.defRef
  val iopp = this.isInputOutputParamPredicate
  val saom = this.switchAsOrderedMatch
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
    if(this.shouldPreprocessOfg) OFAPreprocessModule.setAndroidLibInfoTables(options, avmt)
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
//    cfgs(pst.procedureUri) = cfg
//    if(this.shouldBuildOFAsCfg) rdas(pst.procedureUri) = RdaModule.getRda(options)
//    cCfgOpt match {
//      case Some(item) => cCfgs(pst.procedureUri) = item
//      case None =>
//    }
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
//      println(pst.procedureUri)
      compute(pst)
    }).reduce(combine)

  this.intraResult_=(intraResults)
  println("intra building complete!")
  
  val endtime = System.currentTimeMillis();
  logger.info(s"End computing: Time: ${(endtime - starttime) / 1000d} s")
  
  
  
  /**
   * below is the inter procedure analysis
   */
  def compute() = {
    val j = PipelineJob()
    val options = j.properties
    if(this.shouldBuildSCfg || this.shouldBuildCSCfg){  // SCfg=system(wide)-control-flow-graph, CSCfg=compressedSCfg
      sCfgModule.setAndroidCache(options, aCache)
      sCfgModule.setAndroidLibInfoTables(options, avmt)
      sCfgModule.setCCfgs(options, cCfgs)
      
      if(this.shouldBuildCSCfg){
        
        val APIperm = this.APIpermOpt match {
          case Some(x) => x
          case None => null
        }
        
       // println("APIperm obtained = " + APIperm)
        csCfgModule.setAPIperm(options, APIperm)
          
      }
      
      interPipeline.compute(j)
      info.hasError = j.hasError
      if (info.hasError) {
        info.exception = Some(ErrorneousModulesThrowable(
          j.lastStageInfo.info.filter { i => i.hasError }))
      }
//      println(Tag.collateAsString(j.lastStageInfo.tags))
      val sCfg = if (this.shouldBuildSCfg) Some(sCfgModule.getSCfg(options)) else None
      this.interResult_=(AndroidInterIntraProcedural.AndroidInterAnalysisResult(
          sCfg
        ))
    } else if(this.shouldBuildOFAsCfg) {
      OFAsCfgModule.setAndroidCache(options, aCache)
      OFAsCfgModule.setAndroidLibInfoTables(options, avmt)
      OFAsCfgModule.setCfgs(options, cfgs)
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

    if (this.shouldBuildCfg || this.shouldBuildRda || this.shouldPreprocessOfg || this.shouldBuildCCfg || this.shouldBuildOFAsCfg || this.shouldBuildSCfg || this.shouldBuildCSCfg){
      stages += PipelineStage("CFG Building", false, CfgModule)
    }
    
    if (this.shouldBuildRda || this.shouldPreprocessOfg || this.shouldBuildOFAsCfg){
      stages += PipelineStage("RDA Building", false, RdaModule)
    }
    
    if (this.shouldPreprocessOfg){
      stages += PipelineStage("OFA Preprocess", false, OFAPreprocessModule)
    }
    
    if (this.shouldBuildCCfg || this.shouldBuildOFAsCfg || this.shouldBuildSCfg || this.shouldBuildCSCfg){
      stages += PipelineStage("CCFG Building", false, cCfgModule)
    }

    PipelineConfiguration("Android Intraprocedural Analysis Pipeline",
      false, stages : _*)
  }
  
  def buildInterPipeline(job : PipelineJob) = {
    val stages = marrayEmpty[PipelineStage]
    
    if (this.shouldBuildOFAsCfg) 
      stages += PipelineStage("OFASCFG Building", false, OFAsCfgModule)
    
    if (this.shouldBuildSCfg || this.shouldBuildCSCfg)
      stages += PipelineStage("SCFG Building", false, sCfgModule)
      
    if (this.shouldBuildCSCfg)
      stages += PipelineStage("CSCFG Building", false, csCfgModule)
      
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

class cCfgModuleDef (val job : PipelineJob, info : PipelineJobModuleInfo) extends cCfgModule {
  this.cCfg_=(CompressedControlFlowGraph[String](this.procedureSymbolTable, this.pool, this.cfg))
  
}

class sCfgModuleDef (val job : PipelineJob, info : PipelineJobModuleInfo) extends sCfgModule {
  this.androidCache match{
    case Some(ac) =>
      this.sCfg_=(SystemControlFlowGraph[String](this.androidLibInfoTables, this.cCfgs, ac))
      println("sCfg is built")
    case None =>
      this.sCfg_=(SystemControlFlowGraph[String]())
      println("Please setup AndroidCache!")
  }
}


class csCfgModuleDef (val job : PipelineJob, info : PipelineJobModuleInfo) extends csCfgModule {
 
  this.csCfg_=(this.sCfg.NOcompress(this.cCfgs, this.APIperm))
  println("csCfg is built, which is same as the sCfg")
}

class OFAPreprocessModuleDef (val job : PipelineJob, info : PipelineJobModuleInfo) extends OFAPreprocessModule {
  this.OFG_=(ObjectFlowGraphPreprocessor(this.procedureSymbolTable, this.cfg, this.rda, this.androidLibInfoTables))
}

class OFAsCfgModuleDef (val job : PipelineJob, info : PipelineJobModuleInfo) extends OFAsCfgModule {
  this.androidCache match{
    case Some(ac) =>
      val ofg = ObjectFlowGraphBuilder(this.procedureSymbolTables, this.cfgs, this.rdas, this.androidLibInfoTables, ac)
    case None =>
  }
  this.OFAsCfg_=(SystemControlFlowGraph[String]())
}
