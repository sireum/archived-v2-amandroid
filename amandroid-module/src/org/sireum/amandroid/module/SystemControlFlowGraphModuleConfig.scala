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
import org.sireum.amandroid.scfg.CompressedControlFlowGraph


/*
Copyright (c) 2011-2012 Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/

object AndroidInterProcedural {
  type VirtualLabel = String
  type SCFG = MMap[ResourceUri, (AlirIntraProceduralGraph.NodePool, ControlFlowGraph[VirtualLabel])]
  

  final case class AndroidInterProceduralAnalysisResult(
    // pool : AlirIntraProceduralGraph.NodePool, // later we will see how this works for interProceduralGraph
    scfg : AndroidInterProcedural.SCFG)
        
}



/**
 * @author <a href="mailto:sroy@k-state.edu">Sankar Roy</a>
 * @author <a href="mailto:robby@k-state.edu">Robby</a>
 */

case class SystemCFG(
  title : String = "compress CFGs, and build the SCFG for Android",
  
  @Input
  parallel : Boolean,
  
  @Input 
  symbolTable : SymbolTable,
  
  @Input
  androidVirtualMethodTables : AndroidVirtualMethodTables,
  
  @Input
  result : MMap[ResourceUri, AlirIntraProcedural.AlirIntraproceduralAnalysisResult], 
  
  // the above is a confusing name, but we have to use it as it is the output of alir stage
   
 @Output 
  androidresult : AndroidInterProcedural.AndroidInterProceduralAnalysisResult
 )
  

/**
 * @author <a href="mailto:robby@k-state.edu">Robby</a>
 */
  
object tempForRun {
  def main(args : Array[String]) {
    val opt = PipelineMode()
    opt.classNames = Array(      
        SystemCFG.getClass().getName().dropRight(1)
        )
    opt.dir = "./src/org/sireum/amandroid/module"
    opt.genClassName = "SystemControlFlowGraphModuleCore"

    ModuleGenerator.run(opt)
  }
}
