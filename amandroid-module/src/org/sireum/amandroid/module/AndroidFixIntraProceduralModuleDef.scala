package org.sireum.amandroid.module

import org.sireum.pipeline.PipelineJob
import org.sireum.pipeline.PipelineJobModuleInfo
import org.sireum.util._
import org.sireum.core.module.PilarParserModuleDef
import org.sireum.amandroid.AndroidSymbolResolver.AndroidSymbolTable
import org.sireum.pilar.symbol._
import org.sireum.pilar.ast._
import org.sireum.pilar.parser.ChunkingPilarParser
import org.sireum.alir.ControlFlowGraph
import org.sireum.alir.AlirIntraProceduralGraph
import org.sireum.alir.ReachingDefinitionAnalysis
import org.sireum.amandroid.scfg.CompressedControlFlowGraph
import org.sireum.amandroid.androidObjectFlowAnalysis.AndroidOfgPreprocessor
import org.sireum.amandroid.androidObjectFlowAnalysis.AndroidObjectFlowGraph
import org.sireum.amandroid.objectFlowAnalysis.OfaNode

class AndroidFixIntraProceduralModuleDef (val job : PipelineJob, info : PipelineJobModuleInfo) extends AndroidFixIntraProceduralModule {
  val newProcedures = this.appInfo.getDummyMainCodeMap
  val newModels = newProcedures.map{case (k, v) => ChunkingPilarParser(Left(v), reporter(info)) match{case Some(m) => m; case None => null}}.toList
  
  val ms = this.models
  val par = this.parallel
  val stp = this.symbolTable.asInstanceOf[SymbolTableProducer]
  val fst = { _ : Unit => new ST }
  val libInfoTable = this.androidLibInfoTablesOpt match{case Some(a) => a; case None => {info.hasError = true; null}}
  val result = AndroidSymbolTable(stp, ms, Set(), newModels, fst, libInfoTable, par, shouldBuildLibInfoTables)

  val st = result._1.asInstanceOf[ST]
  info.tags ++= st.tags

  if (st.hasErrors)
    info.hasError = true
    
  this.symbolTable_=(result._1)
  
  this.androidLibInfoTablesOpt=(result._2)
  
  type VirtualLabel = String
  val siff = this.shouldIncludeFlowFunction
  //for building rda
  val dr = this.defRef
  val iopp = this.isInputOutputParamPredicate
  val saom = this.switchAsOrderedMatch
  val newPUris = result._3
  var tempResult = 
    this.intraResult ++
    newPUris.map{
	    pUri=>
	      val pst = result._1.procedureSymbolTable(pUri)
	      val (pool, cfg) = buildCfg(pst)
		    var rdaOpt : Option[ReachingDefinitionAnalysis.Result] = None
		    var ofgOpt : Option[AndroidObjectFlowGraph[OfaNode]] = None
		    var cCfgOpt : Option[CompressedControlFlowGraph[VirtualLabel]] = None
		
		    if (this.shouldBuildRda){
		      val rda = buildRda(pst, cfg)
		      rdaOpt = Some(rda)
		      if(this.shouldPreprocessOfg){
		        val ofg = preprocessOFA(pst, cfg, rda)
		        ofgOpt = Some(ofg)
		      }
		      if (this.shouldBuildCCfg){
		        val cCfg = buildCCfg(pst, cfg, pool)
		        cCfgOpt = Some(cCfg)
		      }
		    }
		    
		    Map(pst.procedureUri ->
		      AndroidIntraProcedural.AndroidIntraAnalysisResult(
		        pool, cfg, rdaOpt, ofgOpt, cCfgOpt
		      ))
	  }.reduce(combine)
  
	this.intraResult_=(tempResult)
	  
  def combine(tp1 : IMap[ResourceUri, AndroidIntraProcedural.AndroidIntraAnalysisResult],
      tp2 : IMap[ResourceUri, AndroidIntraProcedural.AndroidIntraAnalysisResult])
      : IMap[ResourceUri, AndroidIntraProcedural.AndroidIntraAnalysisResult] = {
    tp1 ++ tp2
  }

  def buildCfg(pst : ProcedureSymbolTable) = {
	  val ENTRY_NODE_LABEL = "Entry"
	  val EXIT_NODE_LABEL = "Exit"
	  val pool : AlirIntraProceduralGraph.NodePool = mmapEmpty
	  val result = ControlFlowGraph[VirtualLabel](pst, ENTRY_NODE_LABEL, EXIT_NODE_LABEL, pool, siff)
	  (pool, result)
	}
	
	def buildRda (pst : ProcedureSymbolTable, cfg : ControlFlowGraph[VirtualLabel]) = {
	  val iiopp = iopp(pst)
	  ReachingDefinitionAnalysis[VirtualLabel](pst,
	    cfg,
	    defRef(pst.symbolTable, libInfoTable),
	    first2(iiopp),
	    saom)
	}
	
	def preprocessOFA (pst : ProcedureSymbolTable, cfg : ControlFlowGraph[VirtualLabel], rda : ReachingDefinitionAnalysis.Result) = {
	  AndroidOfgPreprocessor.setAndroidLibInfoTables(libInfoTable)
	  AndroidOfgPreprocessor(pst, cfg, rda)
	}
	
	def buildCCfg (pst : ProcedureSymbolTable, cfg : ControlFlowGraph[VirtualLabel], pool : AlirIntraProceduralGraph.NodePool) = {
	  CompressedControlFlowGraph[String](pst, pool, cfg)
	}
  
  def reporter(info : PipelineJobModuleInfo) =
    new org.sireum.pilar.parser.PilarParser.ErrorReporter {
      def report(source : Option[FileResourceUri], line : Int,
                 column : Int, message : String) =
        info.tags += Tag.toTag(source, line, column, message, PilarParserModuleDef.ERROR_TAG_TYPE)
    }
}