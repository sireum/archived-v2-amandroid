package org.sireum.amandroid.module

import org.sireum.pipeline._
import org.sireum.option.PipelineMode
import org.sireum.pipeline.gen.ModuleGenerator
import org.sireum.alir.ControlFlowGraph
import org.sireum.util.{MMap, MList}
import org.sireum.alir.AlirIntraProceduralGraph
import org.sireum.util.ResourceUri
import org.sireum.core.module.AlirIntraProcedural
import org.sireum.amandroid.AndroidSymbolResolver.AndroidVirtualMethodTables
import org.sireum.pilar.symbol.SymbolTable
import org.sireum.amandroid.scfg.{CompressedControlFlowGraph, SystemControlFlowGraph}
import org.sireum.pilar.ast.{LocationDecl, CatchClause}
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.amandroid.cache.AndroidCacheFile


/*
Copyright (c) 2012-2013 Sankardas Roy & Fengguo Wei, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/

object AndroidInterIntraProcedural {
  type VirtualLabel = String
  type CFG = ControlFlowGraph[VirtualLabel]
  type CCFG = CompressedControlFlowGraph[VirtualLabel]
  type OFAsCfg = SystemControlFlowGraph[VirtualLabel]
  type SCFG = SystemControlFlowGraph[VirtualLabel]
  // type CSCFG = SystemControlFlowGraph[VirtualLabel]
  
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
  androidVirtualMethodTables : AndroidVirtualMethodTables,
  
  @Input
  androidCache : AndroidCacheFile[ResourceUri],
  
  @Input
  shouldBuildCCfg : Boolean,
  
  @Input
  shouldBuildOFAsCfg : Boolean,
  
  @Input
  shouldBuildSCfg : Boolean,
  
  @Input
  shouldBuildCSCfg : Boolean,
  
  @Input
  APIpermOpt : Option[MMap[ResourceUri, MList[String]]],
  
  @Input 
  shouldIncludeFlowFunction : AndroidInterIntraProcedural.ShouldIncludeFlowFunction =
    // ControlFlowGraph.defaultSiff,
    { (_, _) => (Array.empty[CatchClause], false) },
       
  @Output 
  intraResult : MMap[ResourceUri, AndroidInterIntraProcedural.CFG],
    
  @Output
  interResult : Option[AndroidInterIntraProcedural.OFAsCfg]  // actually it can be the compressed sCFG
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
  
case class OFAsCfg(
  title : String = "System Control Flow Graph with OFA Builder",

  @Input
  androidCache : AndroidCacheFile[ResourceUri],
  
  @Input 
  cfgs : MMap[ResourceUri, ControlFlowGraph[String]],

  @Input
  androidVirtualMethodTables : AndroidVirtualMethodTables,
  
  @Output
  @Produce
  OFAsCfg : SystemControlFlowGraph[String])
  
case class sCfg(
  title : String = "System Control Flow Graph Builder",

  @Input
  androidCache : AndroidCacheFile[ResourceUri],
  
  @Input 
  cCfgs : MMap[ResourceUri, CompressedControlFlowGraph[String]],

  @Input
  androidVirtualMethodTables : AndroidVirtualMethodTables,
  
  @Output
  @Produce
  sCfg : SystemControlFlowGraph[String])
  
  
  case class csCfg(
  title : String = "Compressed System Control Flow Graph Builder",

  @Input
  androidCache : AndroidCacheFile[ResourceUri],
  
  @Input 
  cCfgs : MMap[ResourceUri, CompressedControlFlowGraph[String]],

  @Input
  androidVirtualMethodTables : AndroidVirtualMethodTables,
  
  @Input
  sCfg : SystemControlFlowGraph[String],
  
  @Input
  APIperm : MMap[ResourceUri, MList[String]],  // MList will contain the list of permission strings for one API
  
  @Output
  @Produce
  csCfg : SystemControlFlowGraph[String])
  
  
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
        cCfg.getClass().getName().dropRight(1),
        OFAsCfg.getClass().getName().dropRight(1),
        sCfg.getClass().getName().dropRight(1),
        csCfg.getClass().getName().dropRight(1)
        )
    opt.dir = "./src/org/sireum/amandroid/module"
    opt.genClassName = "AndroidInterIntraProceduralModuleCore"

    ModuleGenerator.run(opt)
  }
}
