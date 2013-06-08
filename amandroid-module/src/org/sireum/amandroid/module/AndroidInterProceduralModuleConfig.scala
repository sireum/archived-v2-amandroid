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
import org.sireum.amandroid.objectFlowAnalysis.ObjectFlowGraph
import org.sireum.amandroid.objectFlowAnalysis.OfaNode
import org.sireum.alir._
import org.sireum.pilar.symbol._
import org.sireum.amandroid.objectFlowAnalysis.PrepareApp

/*
Copyright (c) 2012-2013 Sankardas Roy & Fengguo Wei, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/

object AndroidInterProcedural {
  type VirtualLabel = String
  type OfaSCfg = (ObjectFlowGraph[OfaNode], SystemControlFlowGraph[String])
    
  final case class AndroidInterAnalysisResult(
    ofaScfg : AndroidInterProcedural.OfaSCfg)
        
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankar Roy</a>
 * @author <a href="mailto:robby@k-state.edu">Robby</a>
 */

case class AndroidInterProcedural(
  title : String = "Android InterProcedural Module",
  
  @Input
  parallel : Boolean,
  
  @Input 
  symbolTable : SymbolTable,
  
  @Input
  androidLibInfoTablesOpt : Option[AndroidLibInfoTables],
  
  @Input
  androidCache : scala.Option[AndroidCacheFile[ResourceUri]] = None,
  
  @Input
  shouldBuildOFAsCfg : Boolean = true,
  
  @Input
  APIpermOpt : scala.Option[MMap[ResourceUri, MList[String]]] = None,
  
  @Input
  appInfo : PrepareApp,
  
  @Input
  intraResult : MMap[ResourceUri, AndroidIntraProcedural.AndroidIntraAnalysisResult],
    
  @Output interResult : AndroidInterProcedural.AndroidInterAnalysisResult
  
)
 

case class OfaSCfg(
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
  
  @Input
  appInfo : PrepareApp,
  
  // for test now. Later will change it.
  @Output
  @Produce
  OFAsCfg : AndroidInterProcedural.OfaSCfg)
  
  
/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
  
object AndroidInterProceduralModuleBuild {
  def main(args : Array[String]) {
    val opt = PipelineMode()
    opt.classNames = Array(      
        AndroidInterProcedural.getClass().getName().dropRight(1),
        OfaSCfg.getClass().getName().dropRight(1)
        )
    opt.dir = "./src/org/sireum/amandroid/module"
    opt.genClassName = "AndroidInterProceduralModuleCore"

    ModuleGenerator.run(opt)
  }
}