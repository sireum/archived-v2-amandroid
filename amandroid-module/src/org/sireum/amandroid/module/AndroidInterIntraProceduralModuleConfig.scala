package org.sireum.amandroid.module

import org.sireum.pipeline._
import org.sireum.option.PipelineMode
import org.sireum.pipeline.gen.ModuleGenerator
import org.sireum.alir.ControlFlowGraph
import org.sireum.util.MMap
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
Copyright (c) 2011-2012 Sankardas Roy & Fengguo Wei, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/

object AndroidInterIntraProcedural {
  type VirtualLabel = String
  type CFG = ControlFlowGraph[VirtualLabel]
  type CCFG = CompressedControlFlowGraph[VirtualLabel]
  type SCFG = SystemControlFlowGraph[VirtualLabel]
  
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
  shouldBuildSCfg : Boolean,
  
  @Input 
  shouldIncludeFlowFunction : AndroidInterIntraProcedural.ShouldIncludeFlowFunction =
    // ControlFlowGraph.defaultSiff,
    { (_, _) => (Array.empty[CatchClause], false) },
       
  @Output 
  intraResult : MMap[ResourceUri, AndroidInterIntraProcedural.CCFG],
    
  @Output
  interResult : Option[SystemControlFlowGraph[String]]
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
  
/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
  
object AndroidInterIntraProceduralModuleBuild {
  def main(args : Array[String]) {
    val opt = PipelineMode()
    opt.classNames = Array(      
        AndroidInterIntraProcedural.getClass().getName().dropRight(1),
        Cfg.getClass().getName().dropRight(1),
        cCfg.getClass().getName().dropRight(1),
        sCfg.getClass().getName().dropRight(1)
        )
    opt.dir = "./src/org/sireum/amandroid/module"
    opt.genClassName = "AndroidInterIntraProceduralModuleCore"

    ModuleGenerator.run(opt)
  }
}
