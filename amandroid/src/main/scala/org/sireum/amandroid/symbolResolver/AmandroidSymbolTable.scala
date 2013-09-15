package org.sireum.amandroid.symbolResolver

import org.sireum.pilar.symbol._
import org.sireum.util._
import org.sireum.amandroid.ProcedureBody

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
class AmandroidSymbolTable extends SymbolTable with SymbolTableProducer {
  st =>
  val tables = SymbolTableData()
  val tags = marrayEmpty[LocationTag]
  var hasErrors = false
    
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
   
  def reportError(source : Option[FileResourceUri], line : Int,
                  column : Int, message : String) : Unit = {
    tags += Tag.toTag(source, line, column, message, ERROR_TAG_TYPE)
    hasErrors = true
  }

  def reportWarning(fileUri : Option[String], line : Int,
                    column : Int, message : String) : Unit =
    tags += Tag.toTag(fileUri, line, column, message, WARNING_TAG_TYPE)

  val pdMap = mmapEmpty[ResourceUri, ProcedureBody]

  def globalVars = tables.globalVarTable.keys
  def globalVar(globalUri : ResourceUri) = tables.globalVarTable(globalUri)

  def procedures = tables.procedureTable.keys

  def procedures(procedureUri : ResourceUri) = tables.procedureTable(procedureUri)

  def procedureSymbolTables = pdMap.values

  def procedureSymbolTable(procedureAbsUri : ResourceUri) : ProcedureSymbolTable =
    procedureSymbolTableProducer(procedureAbsUri)

  def procedureSymbolTableProducer(procedureAbsUri : ResourceUri) = {
    assert(tables.procedureAbsTable.contains(procedureAbsUri))
    pdMap.getOrElseUpdate(procedureAbsUri, new ProcedureBody(procedureAbsUri, st))
  }

  def toSymbolTable : SymbolTable = this
}