package org.sireum.amandroid.module

import org.sireum.pipeline.PipelineJob
import org.sireum.pipeline.PipelineJobModuleInfo
import org.sireum.util._
import org.sireum.core.module.PilarParserModuleDef
import org.sireum.amandroid.AndroidSymbolResolver.AndroidSymbolTable
import org.sireum.pilar.symbol._
import org.sireum.pilar.ast._
import org.sireum.pilar.parser.ChunkingPilarParser

class AndroidFixPilarSymbolResolverModuleDef (val job : PipelineJob, info : PipelineJobModuleInfo) extends AndroidFixPilarSymbolResolverModule {
  val newProcedures = this.appInfo.getDummyMainMap
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

  def reporter(info : PipelineJobModuleInfo) =
    new org.sireum.pilar.parser.PilarParser.ErrorReporter {
      def report(source : Option[FileResourceUri], line : Int,
                 column : Int, message : String) =
        info.tags += Tag.toTag(source, line, column, message, PilarParserModuleDef.ERROR_TAG_TYPE)
    }
}