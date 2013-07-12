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
import org.sireum.alir._
import org.sireum.pilar.symbol._
import org.sireum.amandroid.objectFlowAnalysis.OfaNode
import org.sireum.amandroid.androidObjectFlowAnalysis.AndroidObjectFlowGraph
import org.sireum.amandroid.androidObjectFlowAnalysis.AndroidValueSet
import org.sireum.amandroid.reachingDefinitionAnalysis.AndroidReachingDefinitionAnalysis


/*
Copyright (c) 2012-2013 Sankardas Roy & Fengguo Wei, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/

object AndroidIntraProcedural {
  type VirtualLabel = String
  type Node = OfaNode
  type CFG = ControlFlowGraph[VirtualLabel]
  type RDA = AndroidReachingDefinitionAnalysis.Result  //adding for rda building
  
  type CCFG = CompressedControlFlowGraph[VirtualLabel]
  type OFG = AndroidObjectFlowGraph[OfaNode, AndroidValueSet]
  
  final case class AndroidIntraAnalysisResult(
    pool : AlirIntraProceduralGraph.NodePool,
    cfg : AndroidIntraProcedural.CFG,
    rdaOpt : Option[AndroidIntraProcedural.RDA],
    ofgOpt : Option[AndroidIntraProcedural.OFG],
    cCfgOpt : Option[AndroidIntraProcedural.CCFG])

  type ShouldIncludeFlowFunction = (LocationDecl, Iterable[CatchClause]) => 
      (Iterable[CatchClause], java.lang.Boolean)
        
}


/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankar Roy</a>
 * @author <a href="mailto:robby@k-state.edu">Robby</a>
 */

case class AndroidIntraProcedural(
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
  APIpermOpt : scala.Option[MMap[ResourceUri, MList[String]]] = None,
  
  @Input 
  shouldIncludeFlowFunction : AndroidIntraProcedural.ShouldIncludeFlowFunction =
    // ControlFlowGraph.defaultSiff,
    { (_, _) => (Array.empty[CatchClause], false) },
    
//modified for building RDA       
  @Input defRef : (SymbolTable, AndroidLibInfoTables) => DefRef = { (st, alit) => new org.sireum.amandroid.androidObjectFlowAnalysis.AndroidDefRef(st, new org.sireum.amandroid.androidObjectFlowAnalysis.AndroidVarAccesses(st), alit) },

  @Input isInputOutputParamPredicate : ProcedureSymbolTable => (ResourceUri => java.lang.Boolean, ResourceUri => java.lang.Boolean) = { pst =>
    val params = pst.params.toSet[ResourceUri]
    ({ localUri => params.contains(localUri) },
      { s => falsePredicate1[ResourceUri](s) })
  },

  @Input switchAsOrderedMatch : Boolean = true,

  @Input procedureAbsUriIterator : scala.Option[Iterator[ResourceUri]] = None,
//////////////////end here
  
  @Output intraResult : MMap[ResourceUri, AndroidIntraProcedural.AndroidIntraAnalysisResult]
   
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
  

  @Input defRef : (SymbolTable, AndroidLibInfoTables) => DefRef = { (st, alit) => new org.sireum.amandroid.androidObjectFlowAnalysis.AndroidDefRef(st, new org.sireum.amandroid.androidObjectFlowAnalysis.AndroidVarAccesses(st), alit) },

  @Input isInputOutputParamPredicate : ProcedureSymbolTable => (ResourceUri => java.lang.Boolean, ResourceUri => java.lang.Boolean),

  @Input switchAsOrderedMatch : Boolean = false,

  @Output @Produce rda : AndroidIntraProcedural.RDA)

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
  rda : AndroidIntraProcedural.RDA,
  
  @Input
  procedureSymbolTable : ProcedureSymbolTable,

  @Input
  androidLibInfoTables : AndroidLibInfoTables,
  
  // for test now. Later will change it.
  @Output
  @Produce
  OFG : AndroidIntraProcedural.OFG)
  
  
/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */
  
object AndroidIntraProceduralModuleBuild {
  def main(args : Array[String]) {
    val opt = PipelineMode()
    opt.classNames = Array(      
        AndroidIntraProcedural.getClass().getName().dropRight(1),
        Cfg.getClass().getName().dropRight(1),
        Rda.getClass().getName().dropRight(1),
        OFAPreprocess.getClass().getName().dropRight(1),
        cCfg.getClass().getName().dropRight(1)
        )
    opt.dir = "./src/org/sireum/amandroid/module"
    opt.genClassName = "AndroidIntraProceduralModuleCore"

    ModuleGenerator.run(opt)
  }
}
