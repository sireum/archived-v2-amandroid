package org.sireum.amandroid.module

import org.sireum.pilar.ast._
import org.sireum.pilar.symbol._
import org.sireum.util._
import org.sireum.pipeline._
import org.sireum.amandroid.symbolResolver.AmandroidSymbolTableBuilder
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
  val result = AmandroidSymbolTableBuilder(ms, fst, par)
    
  Center.resolveRecordsRelationWholeProgram
    
  val st = result.asInstanceOf[AmandroidSymbolTable]
  info.tags ++= st.tags

  if (st.hasErrors){
    info.hasError = true
  }
    
  this.symbolTable_=(result)
}