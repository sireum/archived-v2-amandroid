package org.sireum.amandroid.module

import org.sireum.pipeline._
import org.sireum.core.module.PilarParser
import org.sireum.util.ISeq
import org.sireum.pilar.ast.Model
import org.sireum.pilar.symbol.SymbolTable
import org.sireum.option.PipelineMode
import org.sireum.pipeline.gen.ModuleGenerator
import org.sireum.amandroid.AndroidSymbolResolver.AndroidLibInfoTables


/*
Copyright (c) 2011-2012 Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/




/**
 * @author <a href="mailto:sroy@k-state.edu">Sankar Roy</a>
 * @author <a href="mailto:robby@k-state.edu">Robby</a>
 */

case class PilarAndroidSymbolResolver(
  title : String = "Pilar Symbol Resolver for Android",
  
  @Input
  parallel : Boolean,
// Fengguo Wei put this three input as below.  
  
  @Input
  hasExistingAndroidLibInfoTables : scala.Option[AndroidLibInfoTables],
  
  @Input
  shouldBuildLibInfoTables : Boolean = true,
  
  @Input 
  @Consume(Array(classOf[PilarParser]))
  @Output 
  @Produce 
  models : ISeq[Model],
  
// Fengguo Wei put this output: androidRecordProcedureDependencyTables
  @Output
  androidLibInfoTablesOpt : Option[AndroidLibInfoTables],
 
  
  @Output
  symbolTable : SymbolTable)
  

/**
 * @author <a href="mailto:robby@k-state.edu">Robby</a>
 */
  
object hold {
  def main(args : Array[String]) {
    val opt = PipelineMode()
    opt.classNames = Array(      
        PilarAndroidSymbolResolver.getClass().getName().dropRight(1)
        )
    opt.dir = "./src/org/sireum/amandroid/module"
    opt.genClassName = "PilarAndroidSymbolResolverModulesCore"

    ModuleGenerator.run(opt)
  }
}
