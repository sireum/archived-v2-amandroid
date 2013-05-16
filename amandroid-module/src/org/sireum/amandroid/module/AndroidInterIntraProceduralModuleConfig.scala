package org.sireum.amandroid.module

import org.sireum.pipeline._
import org.sireum.option.PipelineMode
import org.sireum.pipeline.gen.ModuleGenerator
import org.sireum.util._
import org.sireum.core.module.AlirIntraProcedural
import org.sireum.amandroid.AndroidSymbolResolver.AndroidLibInfoTables
import org.sireum.amandroid.scfg.{CompressedControlFlowGraph, SystemControlFlowGraph}
import org.sireum.pilar.ast.{LocationDecl, CatchClause}
import org.sireum.amandroid.cache.AndroidCacheFile
import org.sireum.amandroid.objectflowanalysis.ObjectFlowGraph
import org.sireum.amandroid.objectflowanalysis.OfaNode
import org.sireum.alir._
import org.sireum.pilar.symbol._


/*
Copyright (c) 2012-2013 Sankardas Roy & Fengguo Wei, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/

object AndroidInterIntraProcedural {
  type VirtualLabel = String
  type Node = OfaNode
  type CFG = ControlFlowGraph[VirtualLabel]
  type RDA = ReachingDefinitionAnalysis.Result  //adding for rda building
  
  type CCFG = CompressedControlFlowGraph[VirtualLabel]
  type OFG = ObjectFlowGraph[OfaNode]
  type OFAsCfg = MMap[ResourceUri, (ObjectFlowGraph[OfaNode], SystemControlFlowGraph[String])]
  
  final case class AndroidIntraAnalysisResult(
    pool : AlirIntraProceduralGraph.NodePool,
    cfg : AndroidInterIntraProcedural.CFG,
    rdaOpt : Option[AndroidInterIntraProcedural.RDA],
    ofgOpt : Option[AndroidInterIntraProcedural.OFG],
    cCfgOpt : Option[AndroidInterIntraProcedural.CCFG])
    
  final case class AndroidInterAnalysisResult(
    ofaScfgOpt : Option[AndroidInterIntraProcedural.OFAsCfg])
  
  type ShouldIncludeFlowFunction = (LocationDecl, Iterable[CatchClause]) => 
      (Iterable[CatchClause], java.lang.Boolean)
        
}


/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankar Roy</a>
 * @author <a href="mailto:robby@k-state.edu">Robby</a>
 */

case class AndroidInterIntraProcedural(
  title : String = "Android InterIntraProcedural Module",
  
  @Input
  parallel : Boolean,
  
  @Input 
  symbolTable : SymbolTable,
  
  @Input
  androidLibInfoTablesOpt : Option[AndroidLibInfoTables],
  
  @Input
  androidCache : scala.Option[AndroidCacheFile[ResourceUri]] = None,
  
  @Input
  shouldBuildCfg : Boolean = true,

  @Input 
  shouldBuildRda : Boolean = false,
  
  @Input
  shouldPreprocessOfg : Boolean = false,
  
  @Input
  shouldBuildCCfg : Boolean = false,
  
  @Input
  shouldBuildOFAsCfg : Boolean = false,
  
  @Input
  APIpermOpt : scala.Option[MMap[ResourceUri, MList[String]]] = None,
  
  @Input 
  shouldIncludeFlowFunction : AndroidInterIntraProcedural.ShouldIncludeFlowFunction =
    // ControlFlowGraph.defaultSiff,
    { (_, _) => (Array.empty[CatchClause], false) },
    
//modified for building RDA       
  @Input defRef : (SymbolTable, AndroidLibInfoTables) => DefRef = { (st, alit) => new org.sireum.amandroid.objectflowanalysis.AndroidDefRef(st, new org.sireum.amandroid.objectflowanalysis.AndroidVarAccesses(st), alit) },

  @Input isInputOutputParamPredicate : ProcedureSymbolTable => (ResourceUri => java.lang.Boolean, ResourceUri => java.lang.Boolean) = { pst =>
    val params = pst.params.toSet[ResourceUri]
    ({ localUri => params.contains(localUri) },
      { s => falsePredicate1[ResourceUri](s) })
  },

  @Input switchAsOrderedMatch : Boolean = true,

  @Input procedureAbsUriIterator : scala.Option[Iterator[ResourceUri]] = None,
//////////////////end here
  
  @Output intraResult : MMap[ResourceUri, AndroidInterIntraProcedural.AndroidIntraAnalysisResult],
  
  @Output interResult : AndroidInterIntraProcedural.AndroidInterAnalysisResult
  
)
 
case class Cfg(
  title : String = "Control Flow Graph Builder",

  @Input 
  shouldIncludeFlowFunction : AlirIntraProcedural.ShouldIncludeFlowFunction,

  @Input
  procedureSymbolTable : ProcedureSymbolTable,
  
  @Output
  @Produce
  pool : AlirIntraProceduralGraph.NodePool,
  
  @Output
  @Produce
  cfg : ControlFlowGraph[String]) 

/**
 * @author <a href="mailto:belt@k-state.edu">Jason Belt</a>
 * @author <a href="mailto:robby@k-state.edu">Robby</a>
 */
case class Rda(
  title : String = "Reaching Definition Analysis Builder",

  @Input procedureSymbolTable : ProcedureSymbolTable,

  @Input @Consume(Array(classOf[Cfg])) cfg : ControlFlowGraph[String],

  @Input
  androidLibInfoTables : AndroidLibInfoTables,
  
  @Input defRef : (SymbolTable, AndroidLibInfoTables) => DefRef = { (st, alit) => new org.sireum.amandroid.objectflowanalysis.AndroidDefRef(st, new org.sireum.amandroid.objectflowanalysis.AndroidVarAccesses(st), alit) },

  @Input isInputOutputParamPredicate : ProcedureSymbolTable => (ResourceUri => java.lang.Boolean, ResourceUri => java.lang.Boolean),

  @Input switchAsOrderedMatch : Boolean = false,

  @Output @Produce rda : ReachingDefinitionAnalysis.Result)

case class cCfg(
  title : String = "Compressed Control Flow Graph Builder",
  
  @Input
  procedureSymbolTable : ProcedureSymbolTable,
    
  @Input
  @Consume(Array(classOf[Cfg]))
  pool : AlirIntraProceduralGraph.NodePool,
  
  @Input
  @Consume(Array(classOf[Cfg]))  
  cfg : ControlFlowGraph[String],

  @Output
  @Produce
  cCfg : CompressedControlFlowGraph[String])

case class OFAPreprocess(
  title : String = "intra object flow graph Build",
  
  @Input 
  @Consume(Array(classOf[Cfg]))  
  cfg : ControlFlowGraph[String],
  
  @Input
  @Consume(Array(classOf[Rda]))
  rda : ReachingDefinitionAnalysis.Result,
  
  @Input
  procedureSymbolTable : ProcedureSymbolTable,

  @Input
  androidLibInfoTables : AndroidLibInfoTables,
  
  // for test now. Later will change it.
  @Output
  @Produce
  OFG : AndroidInterIntraProcedural.OFG)
  
case class OFAsCfg(
  title : String = "System Control Flow Graph with OFA Builder",

  @Input
  androidCache : scala.Option[AndroidCacheFile[ResourceUri]],
  
  @Input 
  cfgs : MMap[ResourceUri, ControlFlowGraph[String]],
  
  @Input 
  rdas : MMap[ResourceUri, ReachingDefinitionAnalysis.Result],
  
  @Input 
  cCfgs : MMap[ResourceUri, CompressedControlFlowGraph[String]],
  
  @Input
  procedureSymbolTables : Seq[ProcedureSymbolTable],

  @Input
  androidLibInfoTables : AndroidLibInfoTables,
  
  // for test now. Later will change it.
  @Output
  @Produce
  OFAsCfg : AndroidInterIntraProcedural.OFAsCfg)
  
//case class sCfg(
//  title : String = "System Control Flow Graph Builder",
//
//  @Input
//  androidCache : scala.Option[AndroidCacheFile[ResourceUri]] = None,
//  
//  @Input 
//  cCfgs : MMap[ResourceUri, CompressedControlFlowGraph[String]],
//
//  @Input
//  androidLibInfoTables : AndroidLibInfoTables,
//  
//  @Output
//  @Produce
//  sCfg : SystemControlFlowGraph[String])
//  
//  
//  case class csCfg(
//  title : String = "Compressed System Control Flow Graph Builder",
//
//  @Input
//  androidCache : scala.Option[AndroidCacheFile[ResourceUri]] = None,
//  
//  @Input 
//  cCfgs : MMap[ResourceUri, CompressedControlFlowGraph[String]],
//
//  @Input
//  androidLibInfoTables : AndroidLibInfoTables,
//  
//  @Input
//  sCfg : SystemControlFlowGraph[String],
//  
//  @Input
//  APIperm : MMap[ResourceUri, MList[String]],  // MList will contain the list of permission strings for one API
//  
//  @Output
//  @Produce
//  csCfg : SystemControlFlowGraph[String])
  
  
/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */
  
object AndroidInterIntraProceduralModuleBuild {
  def main(args : Array[String]) {
    val opt = PipelineMode()
    opt.classNames = Array(      
        AndroidInterIntraProcedural.getClass().getName().dropRight(1),
        Cfg.getClass().getName().dropRight(1),
        Rda.getClass().getName().dropRight(1),
        OFAPreprocess.getClass().getName().dropRight(1),
        cCfg.getClass().getName().dropRight(1),
        OFAsCfg.getClass().getName().dropRight(1)
        )
    opt.dir = "./src/org/sireum/amandroid/module"
    opt.genClassName = "AndroidInterIntraProceduralModuleCore"

    ModuleGenerator.run(opt)
  }
}
