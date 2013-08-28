package org.sireum.amandroid.module

import org.sireum.pipeline.PipelineJob
import org.sireum.pipeline.PipelineJobModuleInfo
import org.sireum.alir.ControlFlowGraph
import org.sireum.util._
import org.sireum.alir.AlirIntraProceduralGraph
import java.io._
import org.sireum.amandroid.xml.AndroidXStream
import org.sireum.pipeline.ErrorneousModulesThrowable
import org.sireum.pipeline.PipelineStage
import org.sireum.pipeline.PipelineConfiguration
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.amandroid.android.intraProcedural.reachingDefinitionAnalysis.AndroidReachingDefinitionAnalysis
import org.sireum.amandroid.intraProcedural.pointsToAnalysis.IntraPointsToAnalysis
import org.sireum.amandroid.android.interProcedural.objectFlowAnalysis.AndroidOfgPreprocessor
import org.sireum.amandroid.intraProcedural.compressedControlFlowGraph.CompressedControlFlowGraph
import org.sireum.pilar.ast.NameExp

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */

class AndroidIntraProceduralModuleDef (val job : PipelineJob, info : PipelineJobModuleInfo) 
  extends AndroidIntraProceduralModule with ImplicitLogging{
  // add implementation here
  type VirtualLabel = String
  val par = this.parallel
  val st = this.symbolTable
  val psts = st.procedureSymbolTables.toSeq
  val siff = this.shouldIncludeFlowFunction
  //for building rda
  val dr = this.defRef
  val iopp = this.isInputOutputParamPredicate
  val saom = this.switchAsOrderedMatch
  //////end here
  
  val intraPipeline = buildIntraPipeline(job)
  
  def compute(pst : ProcedureSymbolTable) = {
    val j = PipelineJob()
    val options = j.properties
    CfgModule.setProcedureSymbolTable(options, pst)
    CfgModule.setShouldIncludeFlowFunction(options, siff)
    AndroidIntraProceduralModule.setDefRef(options, dr)
    AndroidIntraProceduralModule.setIsInputOutputParamPredicate(options, iopp)
    AndroidIntraProceduralModule.setSwitchAsOrderedMatch(options, saom)
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
    val pagOpt = if (this.shouldBuildPag) Some(PagModule.getPag(options)) else None
    val ofgOpt = if (this.shouldPreprocessOfg) Some(OFAPreprocessModule.getOFG(options)) else None
    val cCfgOpt = if (this.shouldBuildCCfg) Some(cCfgModule.getCCfg(options)) else None
    val procSig = 
      pst.procedure.getValueAnnotation("signature") match {
	      case Some(exp : NameExp) =>
	        exp.name.name
	      case _ => throw new RuntimeException("Can not find signature")
	    }
    Map(procSig ->
      AndroidIntraProcedural.AndroidIntraAnalysisResult(
        pool, cfg, rdaOpt, pagOpt, ofgOpt, cCfgOpt
      ))
  }
  
  val intraResults : MMap[ResourceUri, AndroidIntraProcedural.AndroidIntraAnalysisResult] = mmapEmpty
  println("par=" + par)
  val col : GenSeq[ProcedureSymbolTable] = if (par) psts.par else psts
  println("intra analysis start! procudure number = " + col.size)
  val starttime = System.currentTimeMillis();
  
  def combine(tp1 : IMap[ResourceUri, AndroidIntraProcedural.AndroidIntraAnalysisResult],
      tp2 : IMap[ResourceUri, AndroidIntraProcedural.AndroidIntraAnalysisResult])
      : IMap[ResourceUri, AndroidIntraProcedural.AndroidIntraAnalysisResult] = {
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
  
  
  
  
  def buildIntraPipeline(job : PipelineJob) = {
    val stages = marrayEmpty[PipelineStage]

    if (this.shouldBuildCfg || this.shouldBuildRda || this.shouldPreprocessOfg || this.shouldBuildCCfg || this.shouldBuildPag){
      stages += PipelineStage("CFG Building", false, CfgModule)
    }
    
    if (this.shouldBuildRda || this.shouldPreprocessOfg || this.shouldBuildPag){
      stages += PipelineStage("RDA Building", false, RdaModule)
    }
    
    if (this.shouldBuildPag){
      stages += PipelineStage("PAG Preprocess", false, PagModule)
    }
    
    if (this.shouldPreprocessOfg){
      stages += PipelineStage("OFA Preprocess", false, OFAPreprocessModule)
    }
    
    if (this.shouldBuildCCfg){
      stages += PipelineStage("CCFG Building", false, cCfgModule)
    }

    PipelineConfiguration("Android Intraprocedural Analysis Pipeline",
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
  this.rda_=(AndroidReachingDefinitionAnalysis[String](pst,
    this.cfg,
    this.defRef(pst.symbolTable),
    first2(iiopp),
    this.switchAsOrderedMatch))
}

class PagModuleDef (val job : PipelineJob, info : PipelineJobModuleInfo) extends PagModule {
  /*TODO: change later*/
//  this.pag_=(new IntraPointsToAnalysis().build())
}

class OFAPreprocessModuleDef (val job : PipelineJob, info : PipelineJobModuleInfo) extends OFAPreprocessModule {
  this.OFG_=(AndroidOfgPreprocessor(this.procedureSymbolTable, this.cfg, this.rda))
}

class cCfgModuleDef (val job : PipelineJob, info : PipelineJobModuleInfo) extends cCfgModule {
  this.cCfg_=(CompressedControlFlowGraph[String](this.procedureSymbolTable, this.pool, this.cfg))
}