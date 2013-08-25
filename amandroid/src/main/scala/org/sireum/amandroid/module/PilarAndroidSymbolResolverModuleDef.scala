package org.sireum.amandroid.module

import org.sireum.pilar.ast._
import org.sireum.pilar.symbol._
import org.sireum.util._
import org.sireum.pipeline._
import org.sireum.amandroid.symbolResolver.AmandroidSymbolTableBuilder
import org.sireum.amandroid.symbolResolver.AndroidLibInfoTables
import org.sireum.amandroid.symbolResolver.AmandroidSymbolTable
import org.sireum.amandroid.Center


object PilarAndroidSymbolResolverModuleDefObject{
    val ERROR_TAG_TYPE = MarkerType(
    "org.sireum.pilar.tag.error.symtab",
    None,
    "Pilar Symbol Resolution Error",
    MarkerTagSeverity.Error,
    MarkerTagPriority.Normal,
    ilist(MarkerTagKind.Problem, MarkerTagKind.Text))
    
    val WARNING_TAG_TYPE = MarkerType(
    "org.sireum.pilar.tag.error.symtab",
    None,
    "Pilar Symbol Resolution Warning",
    MarkerTagSeverity.Warning,
    MarkerTagPriority.Normal,
    ilist(MarkerTagKind.Problem, MarkerTagKind.Text))
}


class PilarAndroidSymbolResolverModuleDef (val job : PipelineJob, info : PipelineJobModuleInfo) extends PilarAndroidSymbolResolverModule {
  val ms = this.models
  val par = this.parallel
  val fst = { _ : Unit => new AmandroidSymbolTable }
  val result = 
    if (this.buildLibInfoTablesOnly) {
      require(this.hasExistingAndroidLibInfoTables.isDefined)
      val eavmt = this.hasExistingAndroidLibInfoTables.get.asInstanceOf[AndroidLibInfoTables]
      AmandroidSymbolTableBuilder(ms, fst, eavmt, par)
    }
    else if (this.hasExistingAndroidLibInfoTables.isDefined) {
      require(this.hasExistingAndroidLibInfoTables.isDefined)
      val eavmt = this.hasExistingAndroidLibInfoTables.get.asInstanceOf[AndroidLibInfoTables]
      AmandroidSymbolTableBuilder(ms, fst, eavmt, par, shouldBuildLibInfoTables)
    }
    else AmandroidSymbolTableBuilder(ms, fst, par, shouldBuildLibInfoTables)
    
  Center.resolveRecordsRelationWholeProgram
    
  val st = result._1.asInstanceOf[AmandroidSymbolTable]
  info.tags ++= st.tags

  if (st.hasErrors){
    info.hasError = true
  }
    
  this.symbolTable_=(result._1)
  
  this.androidLibInfoTablesOpt=(result._2)
  
}