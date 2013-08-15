package org.sireum.amandroid.module

import org.sireum.pipeline.PipelineJob
import org.sireum.pipeline.PipelineJobModuleInfo
import org.sireum.pipeline.PipelineStage
import org.sireum.pipeline.PipelineConfiguration
import org.sireum.util._
import org.sireum.amandroid.scfg.SuperControlFlowGraph
import org.sireum.pipeline.ErrorneousModulesThrowable
import org.sireum.alir.ControlFlowGraph
import org.sireum.amandroid.scfg.CompressedControlFlowGraph
import org.sireum.alir.ReachingDefinitionAnalysis
import org.sireum.amandroid.androidObjectFlowAnalysis.AndroidObjectFlowGraph
import org.sireum.amandroid.objectFlowAnalysis.OfaNode
import org.sireum.amandroid.androidObjectFlowAnalysis.AndroidOfgAndScfgBuilder
import org.sireum.amandroid.androidObjectFlowAnalysis.AndroidValueSet
import org.sireum.amandroid.callGraph.CallGraph
import org.sireum.amandroid.callGraph.CallGraphBuilder

class AndroidInterProceduralModuleDef (val job : PipelineJob, info : PipelineJobModuleInfo) 
extends AndroidInterProceduralModule with ImplicitLogging {
  val cfgs : MMap[ResourceUri, ControlFlowGraph[String]] = mmapEmpty
  val cCfgs : MMap[ResourceUri, CompressedControlFlowGraph[String]] = mmapEmpty
  val rdas : MMap[ResourceUri, ReachingDefinitionAnalysis.Result] = mmapEmpty
  val aCache = this.androidCache
  val alit = this.androidLibInfoTablesOpt.getOrElse(null)
  val st = this.symbolTable
  val psts = st.procedureSymbolTables.toSeq
    
  this.intraResult.foreach(
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
  
  val interPipeline = buildInterPipeline(job)
  
  /**
   * below is the inter procedure analysis
   */
  def compute() = {
    val j = PipelineJob()
    val options = j.properties
    if(this.shouldBuildOFAsCfg) {
      CGModule.setAndroidCache(options, aCache)
      CGModule.setAndroidLibInfoTables(options, alit)
      CGModule.setCfgs(options, cfgs)
      CGModule.setProcedureSymbolTables(options, psts)
      CGModule.setRdas(options, rdas)
      CGModule.setAppInfoOpt(options, this.appInfoOpt)
      
      interPipeline.compute(j)
      info.hasError = j.hasError
      if (info.hasError) {
        info.exception = Some(ErrorneousModulesThrowable(
          j.lastStageInfo.info.filter { i => i.hasError }))
      }
//      println(Tag.collateAsString(j.lastStageInfo.tags))
      val cg = CGModule.getCallGraph(options)
      this.interResult_=(AndroidInterProcedural.AndroidInterAnalysisResult(
        cg
      ))
    }
  }
  
  println("inter analysis start! procudure number")
  compute()
  println("inter building complete!")
  
  def buildInterPipeline(job : PipelineJob) = {
    val stages = marrayEmpty[PipelineStage]
    
    if (this.shouldBuildOFAsCfg) 
      stages += PipelineStage("CallGraph Building", false, CGModule)
      
    PipelineConfiguration("Android Interprocedural Analysis Pipeline",
      false, stages : _*)
  }
}

class CGModuleDef (val job : PipelineJob, info : PipelineJobModuleInfo) extends CGModule {
  var result : CallGraph[String] = null
  this.androidCache match{
    case Some(ac) =>
      result = new CallGraphBuilder().apply(this.procedureSymbolTables, this.cfgs, this.rdas, this.androidLibInfoTables, this.appInfoOpt, ac)
    case None =>
  }
  this.callGraph_=(result)
}