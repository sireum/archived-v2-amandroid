package org.sireum.amandroid.module
/*
Copyright (c) 2011-2012 Fengguo Wei Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/

import org.sireum.pipeline._
import org.sireum.core.module.PilarParser
import org.sireum.util._
import org.sireum.pilar.ast.Model
import org.sireum.pilar.symbol.SymbolTable
import org.sireum.option.PipelineMode
import org.sireum.pipeline.gen.ModuleGenerator
import org.sireum.amandroid.AndroidSymbolResolver.AndroidLibInfoTables
import org.sireum.amandroid.cache.AndroidCacheFile
import org.sireum.pilar.ast.CatchClause
import org.sireum.alir.DefRef
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.amandroid.appInfo.PrepareApp

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
case class AndroidFixIntraProcedural(
  title : String = "Fix Intra procedural for Android",
  
  @Input
  parallel : Boolean,
  
  @Input
  shouldBuildLibInfoTables : Boolean = true,
  
  @Input
  appInfoOpt : scala.Option[PrepareApp],
  
  @Input 
  models : ISeq[Model],
 
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
  
  @Output
  symbolTable : SymbolTable,
  
  @Output
  androidLibInfoTablesOpt : Option[AndroidLibInfoTables],
  
	@Output 
	intraResult : MMap[ResourceUri, AndroidIntraProcedural.AndroidIntraAnalysisResult])
  

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object fixResolver {
  def main(args : Array[String]) {
    val opt = PipelineMode()
    opt.classNames = Array(AndroidFixIntraProcedural.getClass().getName().dropRight(1))
    opt.dir = "./src/org/sireum/amandroid/module"
    opt.genClassName = "AndroidFixIntraProceduralModulesCore"

    ModuleGenerator.run(opt)
  }
}