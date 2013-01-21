package org.sireum.amandroid.AndroidSymbolResolver



/*
Copyright (c) 2011-2012 Robby, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/


import scala.collection.GenSeq
import scala.collection.mutable.WrappedArray
import org.sireum.pilar.ast._
import org.sireum.pilar.symbol._
import org.sireum.util._

/**
 * @author <a href="mailto:sroy@k-state.edu">Sankar Roy</a>
 */


/**
 * @author <a href="mailto:robby@k-state.edu">Robby</a>
 */

object H1 {
  
  abstract class StpWrapper(stp : AndroidSymbolTableProducer) extends AndroidSymbolTableProducer {
    def tables : AndroidSymbolTableData = stp.tables

    def procedureSymbolTableProducer //
    (procedureUri : ResourceUri) : AndroidProcedureSymbolTableProducer =
      stp.procedureSymbolTableProducer(procedureUri)

    def toSymbolTable : SymbolTable = stp.toSymbolTable

    def reportError(fileUri : Option[String], line : Int,
                    column : Int, message : String) : Unit =
      stp.reportError(fileUri, line, column, message)

    def reportWarning(fileUri : Option[String], line : Int,
                      column : Int, message : String) : Unit =
      stp.reportWarning(fileUri, line, column, message)
  }
    

  class PackageElementMiner(stp : AndroidSymbolTableProducer)
      extends StpWrapper(stp)
      with AndroidProcedureMiner with AndroidRecordMiner {

    val packageElementMiner =
      Visitor.build(Visitor.map(
        ilist(procedureMiner, recordMiner), false))
  }

  class ProcedureMinerResolver(pstp : AndroidProcedureSymbolTableProducer)
      extends AndroidProcedureSymbolTableProducer with AndroidProcedureSymbolMiner
      with AndroidJumpResolver {
    override def dependency = H.EMPTY_DEPENDENCY

    def tables : ProcedureSymbolTableData = pstp.tables

    def androidSymbolTableProducer() : AndroidSymbolTableProducer = pstp.androidSymbolTableProducer

    val procMiner =
      Visitor.build(Visitor.map(ilist(procedureBodyMiner), false))

    val procResolver = Visitor.build(Visitor.map(ilist(jumpResolver), false))
  }

//  class PackageElementResolver(stp : SymbolTableProducer)
//      extends H.StpWrapper(stp)
//      with ConstResolver with EnumResolver with ExtensionResolver
//      with FunResolver with GlobalVarResolver with ProcedureResolver
//      with RecordResolver with TypeAliasResolver 
//      with FunParamResolver {
//
//    override def dependency = mmapEmpty[String, MSet[String]]
//
//    val packageElementResolver =
//      Visitor.buildEnd(
//        Visitor.map(ilist(constResolver, enumResolver, extensionResolver,
//          funResolver, globalVarResolver, procedureResolver,
//          recordResolver, typeAliasResolver,
//          funParamResolver), false),
//        Visitor.map(ilist(funParamResolverEnd), false))
//  }

  def tearDown(tables : SymbolTableData, m : Model) = {
    val fileUri = m.sourceURI.get
    val declaredSymbols = tables.declaredSymbols(fileUri)
    tables.declaredSymbols -= fileUri
    tables.constTable --= declaredSymbols
    tables.constElementTable --= declaredSymbols
    tables.enumTable --= declaredSymbols
    tables.enumElementTable --= declaredSymbols
    tables.extensionTable --= declaredSymbols
    tables.extensionElementTable --= declaredSymbols
    tables.funTable --= declaredSymbols
    tables.globalVarTable --= declaredSymbols
    tables.procedureTable --= declaredSymbols
    tables.procedureAbsTable --= declaredSymbols
    tables.recordTable --= declaredSymbols
    tables.attributeTable --= declaredSymbols
    tables.typeVarTable --= declaredSymbols
    tables.typeAliasTable --= declaredSymbols
    // tables.vsetTable --= declaredSymbols
    tables.dependency -= fileUri
    tables.dependency.values.foreach { s =>
      s -= fileUri
    }
  }

  
  
  def combine(stp1 : AndroidSymbolTableProducer,
              stp2 : AndroidSymbolTableProducer) : AndroidSymbolTableProducer = {
    val tables1 = stp1.tables
    val tables2 = stp2.tables
    val str = stp1

//    H.combineMap(tables1.declaredSymbols, tables2.declaredSymbols)
//
//    H.combineTable(tables1.constTable, tables2.constTable)
//
//    H.combineTable(str, tables1.constElementTable, tables2.constElementTable,
//      { x : ConstElement => x.name }, SymbolTableMessage.DUPLICATE_CONST_ELEMENT + SymbolTableMessage.OF_FILE)
//
//    H.combineTable(tables1.enumTable, tables2.enumTable)
//
//    H.combineTable(str, tables1.enumElementTable, tables2.enumElementTable,
//      { x : EnumElement => x.name }, SymbolTableMessage.DUPLICATE_ENUM_ELEMENT + SymbolTableMessage.OF_FILE)
//
//    H.combineTable(tables1.extensionTable, tables2.extensionTable)
//
//    H.combineTable(str, tables1.extensionElementTable, tables2.extensionElementTable,
//      { x : ExtElement => x.name }, SymbolTableMessage.DUPLICATE_EXTENSION_ELEMENT + SymbolTableMessage.OF_FILE)
//
//    H.combineTable(str, tables1.funTable, tables2.funTable,
//      { x : FunDecl => x.name }, SymbolTableMessage.DUPLICATE_FUN + SymbolTableMessage.OF_FILE)
//
//    H.combineTable(str, tables1.globalVarTable, tables2.globalVarTable,
//      { x : GlobalVarDecl => x.name }, SymbolTableMessage.DUPLICATE_GLOBAL_VAR + SymbolTableMessage.OF_FILE)

    H.combineTable(tables1.procedureTable, tables2.procedureTable)

    H.combineTable(str, tables1.procedureAbsTable, tables2.procedureAbsTable,
      { x : ProcedureDecl => x.name }, SymbolTableMessage.DUPLICATE_PROCEDURE + SymbolTableMessage.OF_FILE)

    H.combineTable(str, tables1.recordTable, tables2.recordTable,
      { x : RecordDecl => x.name }, SymbolTableMessage.DUPLICATE_RECORD + SymbolTableMessage.OF_FILE)

//    H.combineTable(str, tables1.attributeTable, tables2.attributeTable,
//      { x : AttributeDecl => x.name }, SymbolTableMessage.DUPLICATE_ATTRIBUTE + SymbolTableMessage.OF_FILE)
//
//    H.combineTable(str, tables1.typeVarTable, tables2.typeVarTable,
//      { x : NameDefinition => x }, SymbolTableMessage.DUPLICATE_TYPE_VAR + SymbolTableMessage.OF_FILE)
//
//    H.combineTable(str, tables1.typeAliasTable, tables2.typeAliasTable,
//      { x : TypeAliasDecl => x.name }, SymbolTableMessage.DUPLICATE_TYPE_ALIAS + SymbolTableMessage.OF_FILE)

    //combineTable(str, tables1.vsetTable, tables2.vsetTable,
      // { x : VSetDecl => x.name }, DUPLICATE_VSET + OF_FILE)

    stp1
  }

}
