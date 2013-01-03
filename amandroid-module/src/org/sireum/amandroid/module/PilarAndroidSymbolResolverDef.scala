package org.sireum.amandroid.module

import org.sireum.pilar.ast._
import org.sireum.pilar.symbol._
import org.sireum.util._
import org.sireum.pipeline._



object PilarAndroidSymbolResolverDef{
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


class PilarAndroidSymbolResolverDef (val job : PipelineJob, info : PipelineJobModuleInfo) extends PilarAndroidSymbolResolverModule {
  
  val ms = this.models
  val par = this.parallel
  val fst = { _ : Unit => new ST }

  val result = AndroidSymbolTable(ms, fst, par)
    

  val st = result.asInstanceOf[ST]
  info.tags ++= st.tags

  if (st.hasErrors)
    info.hasError = true

  this.symbolTable_=(result)

  class ST extends SymbolTable with SymbolTableProducer {
    st =>

    import PilarAndroidSymbolResolverDef.ERROR_TAG_TYPE
    import PilarAndroidSymbolResolverDef.WARNING_TAG_TYPE
    
    val tables = SymbolTableData()
    val tags = marrayEmpty[LocationTag]
    var hasErrors = false
 
    // println("pilarAndroidSymbolResDef :: AndroidSymbolTable :: class ST :: OK1") // sankar testing
    
    def reportError(source : Option[FileResourceUri], line : Int,
                    column : Int, message : String) : Unit = {
      tags += Tag.toTag(source, line, column, message, ERROR_TAG_TYPE)
      hasErrors = true
    }

    def reportWarning(fileUri : Option[String], line : Int,
                      column : Int, message : String) : Unit =
      tags += Tag.toTag(fileUri, line, column, message, WARNING_TAG_TYPE)

    val pdMap = mmapEmpty[ResourceUri, PST]

    def globalVars = tables.globalVarTable.keys
    def globalVar(globalUri : ResourceUri) = tables.globalVarTable(globalUri)

    def procedures = tables.procedureTable.keys

    def procedures(procedureUri : ResourceUri) = tables.procedureTable(procedureUri)

    def procedureSymbolTables = pdMap.values

    def procedureSymbolTable(procedureAbsUri : ResourceUri) : ProcedureSymbolTable =
      procedureSymbolTableProducer(procedureAbsUri)

    def procedureSymbolTableProducer(procedureAbsUri : ResourceUri) = {
      assert(tables.procedureAbsTable.contains(procedureAbsUri))
     // println("pilarAndroidSymbolResDef :: AndroidSymbolTable :: class ST :: OK3.1" + procedureAbsUri) // sankar testing
      pdMap.getOrElseUpdate(procedureAbsUri, new PST(procedureAbsUri))
    }

    class PST(val procedureUri : ResourceUri)
        extends ProcedureSymbolTable with ProcedureSymbolTableProducer {
      
      val tables = ProcedureSymbolTableData()
      var nextLocTable : CMap[ResourceUri, ResourceUri] = null
      
      // println("pilarAndroidSymbolResDef :: AndroidSymbolTable :: class ST :: class PST :: OK3.2" + procedureUri) // sankar testing
      
      def symbolTable = st
      def symbolTableProducer = st
      def procedure = st.tables.procedureAbsTable(procedureUri)
      def typeVars : ISeq[ResourceUri] = tables.typeVarTable.keys.toList
      def params : ISeq[ResourceUri] = tables.params.toList
      def isParam(localUri : ResourceUri) = tables.params.contains(localUri)
      def locals : Iterable[ResourceUri] = tables.localVarTable.keys
      def nonParamLocals : Iterable[ResourceUri] = tables.localVarTable.keys.filterNot(isParam)
      def locations =
        tables.bodyTables match {
          case Some(bt) => procedure.body.asInstanceOf[ImplementedBody].locations
          case _        => ilistEmpty
        }
      def typeVar(typeVarUri : ResourceUri) : NameDefinition =
        tables.typeVarTable(typeVarUri)
      def param(paramUri : ResourceUri) : ParamDecl =
        tables.localVarTable(paramUri).asInstanceOf[ParamDecl]
      def local(localUri : ResourceUri) : LocalVarDecl =
        tables.localVarTable(localUri).asInstanceOf[LocalVarDecl]
      def location(locationIndex : Int) = locations(locationIndex)
      def location(locationUri : ResourceUri) =
        tables.bodyTables.get.locationTable(locationUri)
      def catchClauses(locationIndex : Int) : Iterable[CatchClause] =
        tables.bodyTables.get.catchTable.getOrElse(locationIndex,
          Array.empty[CatchClause] : Iterable[CatchClause])
    }

    def toSymbolTable : SymbolTable = this
  }
  
  
}