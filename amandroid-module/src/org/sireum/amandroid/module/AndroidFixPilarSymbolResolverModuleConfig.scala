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
import org.sireum.util.ISeq
import org.sireum.pilar.ast.Model
import org.sireum.pilar.symbol.SymbolTable
import org.sireum.option.PipelineMode
import org.sireum.pipeline.gen.ModuleGenerator
import org.sireum.amandroid.AndroidSymbolResolver.AndroidLibInfoTables
import org.sireum.amandroid.objectflowanalysis.PrepareApp

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
case class AndroidFixPilarSymbolResolver(
  title : String = "Fix Pilar Symbol Resolver for Android",
  
  @Input
  parallel : Boolean,
  
  @Input
  shouldBuildLibInfoTables : Boolean = true,
  
  @Input
  appInfo : PrepareApp,
  
  @Input 
  models : ISeq[Model],
  
  @Output
  androidLibInfoTablesOpt : Option[AndroidLibInfoTables],
 
  
  @Output
  symbolTable : SymbolTable)
  

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object fixResolver {
  def main(args : Array[String]) {
    val opt = PipelineMode()
    opt.classNames = Array(AndroidFixPilarSymbolResolver.getClass().getName().dropRight(1))
    opt.dir = "./src/org/sireum/amandroid/module"
    opt.genClassName = "AndroidFixPilarSymbolResolverModulesCore"

    ModuleGenerator.run(opt)
  }
}