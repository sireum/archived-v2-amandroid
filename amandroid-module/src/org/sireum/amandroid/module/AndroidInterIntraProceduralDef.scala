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


/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */

class AndroidInterIntraProceduralDef (val job : PipelineJob, info : PipelineJobModuleInfo) 
  extends AndroidInterIntraProceduralModule with ImplicitLogging{
  // add implementation here
  type VirtualLabel = String
  val par = this.parallel
  val cCfgs = mmapEmpty[ResourceUri, (AlirIntraProceduralGraph.NodePool, CompressedControlFlowGraph[VirtualLabel])]
  val aCache = this.androidCache
  val st = this.symbolTable
  val psts = st.procedureSymbolTables.toSeq
  val siff = this.shouldIncludeFlowFunction
  val intraPipeline = buildIntraPipeline(job)
  val interPipeline = buildInterPipeline(job)
  
  def compute(pst : ProcedureSymbolTable) = {
    val j = PipelineJob()
    val options = j.properties
    CfgModule.setProcedureSymbolTable(options, pst)
    CfgModule.setShouldIncludeFlowFunction(options, siff)
    if(this.shouldBuildCCfg){
      cCfgModule.setProcedureSymbolTable(options, pst)
    }
    intraPipeline.compute(j)
    info.hasError = j.hasError
    if (info.hasError) {
      info.exception = Some(ErrorneousModulesThrowable(
        j.lastStageInfo.info.filter { i => i.hasError }))
    }
    val pool = CfgModule.getPool(options)
    val cfg = CfgModule.getCfg(options)
    val cCfg = cCfgModule.getCCfg(options)
    (pst.procedureUri, cCfg)
  }
  
  val intraResult : MMap[ResourceUri, CompressedControlFlowGraph[VirtualLabel]] = mmapEmpty

  val col : GenSeq[ProcedureSymbolTable] = if (par) psts.par else psts
  println("ccfg building start! ccfg number = " + col.size)
  (col.map { pst =>
    compute(pst)
  }).foreach { p =>
    intraResult(p._1) = p._2
  }
  println("ccfg building complete!")
  this.intraResult_=(intraResult)
  
  if(this.shouldBuildSCfg || this.shouldBuildCSCfg){  // SCfg=system(wide)-control-flow-graph, CSCfg=compressedSCfg
    val j = PipelineJob()
    val options = j.properties
    val avmt = this.androidVirtualMethodTables
    val cCfgs : MMap[ResourceUri, CompressedControlFlowGraph[VirtualLabel]] = mmapEmpty
    
    sCfgModule.setAndroidCache(options, aCache)
    sCfgModule.setAndroidVirtualMethodTables(options, avmt)
    sCfgModule.setCCfgs(options, intraResult)
    
    if(this.shouldBuildCSCfg){
      
      val APIperm = this.APIpermOpt match {
        case Some(x) => x
        case None => null
      }
      
     // println("APIperm obtained = " + APIperm)
      
      csCfgModule.setAndroidCache(options, aCache)
      csCfgModule.setAndroidVirtualMethodTables(options, avmt)
      csCfgModule.setCCfgs(options, intraResult)
      csCfgModule.setAPIperm(options, APIperm)
        
    }
    
    interPipeline.compute(j)
    info.hasError = j.hasError
    if (info.hasError) {
      info.exception = Some(ErrorneousModulesThrowable(
        j.lastStageInfo.info.filter { i => i.hasError }))
    }
    this.interResult_=(Option(csCfgModule.getCsCfg(options)))
    
  } else {
    this.interResult_=(None)
  }
  
  def buildIntraPipeline(job : PipelineJob) = {
    val stages = marrayEmpty[PipelineStage]

    if (this.shouldBuildCCfg || this.shouldBuildSCfg || this.shouldBuildCSCfg){
      stages += PipelineStage("CFG Building", false, CfgModule)
      stages += PipelineStage("CCFG Building", false, cCfgModule)
    }

    PipelineConfiguration("Android Intraprocedural Analysis Pipeline",
      false, stages : _*)
  }
  
  def buildInterPipeline(job : PipelineJob) = {
    val stages = marrayEmpty[PipelineStage]
    
    if (this.shouldBuildSCfg || this.shouldBuildCSCfg)
      stages += PipelineStage("SCFG Building", false, sCfgModule)
      
    if (this.shouldBuildCSCfg)
      stages += PipelineStage("CSCFG Building", false, csCfgModule)
      
    PipelineConfiguration("Android Interprocedural Analysis Pipeline",
      false, stages : _*)
  }
  
}


class CfgDef (val job : PipelineJob, info : PipelineJobModuleInfo) extends CfgModule {
  val ENTRY_NODE_LABEL = "Entry"
  val EXIT_NODE_LABEL = "Exit"
  val pl : AlirIntraProceduralGraph.NodePool = mmapEmpty
  val pst = this.procedureSymbolTable
  val siff = this.shouldIncludeFlowFunction
  this.pool_=(pl)
  val result = ControlFlowGraph[String](pst, ENTRY_NODE_LABEL, EXIT_NODE_LABEL, pl, siff)
  this.cfg_=(result)
}

class cCfgDef (val job : PipelineJob, info : PipelineJobModuleInfo) extends cCfgModule {
  this.cCfg_=(CompressedControlFlowGraph[String](this.procedureSymbolTable, this.pool, this.cfg))
  
}

class sCfgDef (val job : PipelineJob, info : PipelineJobModuleInfo) extends sCfgModule {
  this.sCfg_=(SystemControlFlowGraph[String](this.androidVirtualMethodTables, this.cCfgs, this.androidCache))
  println("sCfg is built")
}


class csCfgDef (val job : PipelineJob, info : PipelineJobModuleInfo) extends csCfgModule {
 
  this.csCfg_=(this.sCfg.compress(this.cCfgs, this.APIperm))
  println("csCfg is built")
}