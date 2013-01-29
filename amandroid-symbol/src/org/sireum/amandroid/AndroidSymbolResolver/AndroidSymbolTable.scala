package org.sireum.amandroid.AndroidSymbolResolver

import scala.collection.JavaConversions._
import org.sireum.pilar.ast._
import org.sireum.util._
import org.sireum.pilar.symbol._
import com.google.common.collect.HashMultimap
import org.sireum.amandroid.AndroidSymbolResolver._


/**
 * @author <a href="mailto:robby@k-state.edu">Robby</a>
 */

trait AndroidSymbolTable extends SymbolTable



/**
 * @author <a href="mailto:robby@k-state.edu">Robby</a>
 */

object AndroidSymbolTable {
  def apply(models : ISeq[Model],
            stpConstructor : Unit => AndroidSymbolTableProducer,
            parallel : Boolean) =
    buildSymbolTable(models, stpConstructor, parallel)
    
  def apply(models : ISeq[Model],
            stpConstructor : Unit => AndroidSymbolTableProducer,
            existingST : SymbolTable,
            parallel : Boolean) =
    buildSymbolTable(models, stpConstructor, existingST, parallel)


  def minePackageElements[P <: AndroidSymbolTableProducer] //
  (models : ISeq[Model], stpConstructor : Unit => P,
   parallel : Boolean) : AndroidSymbolTableProducer = {
    if (models.isEmpty) return stpConstructor()

    val ms : GenSeq[Model] = if (parallel) models.par else models
    ms.map { model =>
      val stp = stpConstructor()
      new H1.PackageElementMiner(stp).packageElementMiner(model)
      val tables = stp.tables
      model.sourceURI.foreach { fileUri =>
        val set = msetEmpty[ResourceUri]
//        set ++= tables.constTable.keys
//        set ++= tables.constElementTable.keys
//        set ++= tables.enumTable.keys
//        set ++= tables.enumElementTable.keys
//        set ++= tables.extensionTable.keys
//        set ++= tables.extensionElementTable.keys
//        set ++= tables.funTable.keys
        set ++= tables.globalVarTable.keys
        set ++= tables.procedureTable.keys
        set ++= tables.procedureAbsTable.keys
        set ++= tables.recordTable.keys
//        set ++= tables.attributeTable.keys
//        set ++= tables.typeVarTable.keys
//        set ++= tables.typeAliasTable.keys
        // set ++= tables.vsetTable.keys
        tables.declaredSymbols(fileUri) = set
      }
      // println("AndroidSymbolTable :: AndroidSymbolTable :: minePackageElements :: OK2.1") // sankar testing
      stp
    }.toIterable.reduce(H1.combine)
    
  }

  def resolveVirtualMethod(stp : AndroidSymbolTableProducer) : AndroidVirtualMethodTables = {
    val rht = HashMultimap.create[ResourceUri, ResourceUri]()
    val vmt = HashMultimap.create[ResourceUri, ResourceUri]()
    val rpt = HashMultimap.create[ResourceUri, ResourceUri]()
    val cfrt = HashMultimap.create[ResourceUri, ResourceUri]()
    val rut = mmapEmpty[String, ResourceUri]
    val put = mmapEmpty[String, ResourceUri]
    val ptt = mmapEmpty[String, String]
    val it = msetEmpty[ResourceUri]
    new Object with AndroidVirtualMethodResolver {
      override def recordHierarchyTable = rht
      override def virtualMethodTable = vmt
      override def recordProcedureTable = rpt
      override def cannotFindRecordTable = cfrt
      override def recordUriTable = rut
      override def procedureUriTable = put
      override def procedureTypeTable = ptt
      override def interfaceTable = it
    }.androidVirtualMethodResolver(stp)
    val favmt = {_ : Unit => new AVMT}
    AndroidVirtualMethodTables(rht, vmt, rpt, cfrt, rut, put, ptt, it, favmt)
  }
  
  class AVMT extends AndroidVirtualMethodTables with AndroidVirtualMethodTablesProducer {
    avmt =>
    
    val tables = AndroidVirtualMethodTablesData()
    
    def recordHierarchyTable : HashMultimap[ResourceUri, ResourceUri] = tables.recordHierarchyTable
    def virtualMethodTable : HashMultimap[ResourceUri, ResourceUri] = tables.virtualMethodTable
    def recordProcedureTable : HashMultimap[ResourceUri, ResourceUri] = tables.recordProcedureTable
    def cannotFindRecordTable : HashMultimap[ResourceUri, ResourceUri] = tables.cannotFindRecordTable
    def recordUriTable : MMap[String, ResourceUri] = tables.recordUriTable
    def procedureUriTable : MMap[String, ResourceUri] = tables.procedureUriTable
    def procedureTypeTable : MMap[ResourceUri, String] = tables.procedureTypeTable
    def interfaceTable : MSet[ResourceUri] = tables.interfaceTable
    def getRecordUri(recordName : String) : ResourceUri = {
      if(recordUriTable.contains(recordName)){
        recordUriTable(recordName)
      }
      else null
    }
    
    def getProcedureUriBySignature(sig : String) : ResourceUri = {
      if(procedureUriTable.contains(sig))
        procedureUriTable(sig)
      else null
    }
    
    def getCalleeOptionsByUri(procedureUri : ResourceUri) : java.util.Set[ResourceUri] = {
      if(procedureUri != null){
        if(isConstructor(procedureUri) || isStaticMethod(procedureUri)){
          val option : java.util.Set[ResourceUri] = null
          option +(procedureUri)
          option
        }
        else virtualMethodTable.get(procedureUri)
      }
      else null
    }
    
    def getCalleeOptionsBySignature(sig : String) : java.util.Set[ResourceUri] = {
      var procedureUri = getProcedureUriBySignature(sig)
      getCalleeOptionsByUri(procedureUri)
    }
    
    def isConstructor(procedureUri : ResourceUri) : Boolean = {
      if(procedureTypeTable.contains(procedureUri)){
        if(procedureTypeTable(procedureUri) != null && procedureTypeTable(procedureUri).contains("CONSTRUCTOR")) true
        else false
      }
      else {
        println("procedureTypeTable : cannot find " + procedureUri)
        false
      }
    }
    
    def isStaticMethod(procedureUri : ResourceUri) : Boolean = {
      if(procedureTypeTable.contains(procedureUri)){
        if(procedureTypeTable(procedureUri) != null && procedureTypeTable(procedureUri).contains("STATIC")) true
        else false
      }
      else {
        println("procedureTypeTable : cannot find " + procedureUri)
        false
      }
    }
    
    def isVirtualMethod(procedureUri : ResourceUri) : Boolean = {
      if(procedureTypeTable.contains(procedureUri)){
        if(procedureTypeTable(procedureUri) != null && !procedureTypeTable(procedureUri).contains("CONSTRUCTOR") && !procedureTypeTable(procedureUri).contains("STATIC")) true
        else false
      }
      else {
        println("procedureTypeTable : cannot find " + procedureUri)
        false
      }
    }
    
    def mergeWith(anotherVmTables : AndroidVirtualMethodTables) = {
    mergeRecordHierarchyTable(anotherVmTables.recordHierarchyTable,
                              anotherVmTables.cannotFindRecordTable,
                              anotherVmTables.recordUriTable)
    
  }
  
  def mergeRecordHierarchyTable(anotherRecordHierarchyTable : HashMultimap[ResourceUri, ResourceUri],
                                anotherCannotFindRecordTable : HashMultimap[ResourceUri, ResourceUri],
                                anotherRecordUriTables : MMap[String, ResourceUri]) = {
    var tempNotFoundRecordsTable : HashMultimap[ResourceUri, ResourceUri] = HashMultimap.create()
    var keys = cannotFindRecordTable.keySet
    keys.map(
      key=>
      {
        val anotherRecordSets = anotherRecordUriTables.values.toSeq
        val notFoundRecords = cannotFindRecordTable.get(key)
        notFoundRecords.map(
          notFoundRecord =>
            if(anotherRecordSets.contains(notFoundRecord)){
              println("find: " + notFoundRecord)
              recordHierarchyTable.put(key, notFoundRecord)
            } 
            else {
              tempNotFoundRecordsTable.put(key, notFoundRecord)
            }
        )
      }
    )
    
    cannotFindRecordTable.clear()
    cannotFindRecordTable.putAll(tempNotFoundRecordsTable)
    tempNotFoundRecordsTable.clear()
    val recordSets = recordUriTable.values.toSeq
    keys = anotherCannotFindRecordTable.keySet
    keys.map(
      key => 
      {
        val notFoundRecords = anotherCannotFindRecordTable.get(key)
        notFoundRecords.map(
          notFoundRecord =>
          {
            if(recordSets.contains(notFoundRecord)){
              println("find: " + notFoundRecord)
              recordHierarchyTable.put(key, notFoundRecord)
            } 
            else {
              tempNotFoundRecordsTable.put(key, notFoundRecord)
            }
          }
        )
      }
    )
        
    cannotFindRecordTable.putAll(tempNotFoundRecordsTable)
    recordHierarchyTable.putAll(anotherRecordHierarchyTable)
    recordUriTable ++= anotherRecordUriTables
  }
    
    def toAndroidVirtualMethodTables : AndroidVirtualMethodTables = this
  }

  def buildProcedureSymbolTables(stp : AndroidSymbolTableProducer, parallel : Boolean = true) : Unit = {
    val procedures = stp.tables.procedureAbsTable.keys.toSeq
    val col : GenSeq[ResourceUri] = if (parallel) procedures.par else procedures
    
    println("build pst start! pst number : " + col.size)
    
    col.foreach { procedureUri =>
      val pstp = stp.procedureSymbolTableProducer(procedureUri)
      val pd = stp.tables.procedureAbsTable(procedureUri)
      pd.body match {
        case body : ImplementedBody =>
          pstp.tables.bodyTables =
            Some(BodySymbolTableData())
        case body : EmptyBody =>
      }
      val pmr = new H1.ProcedureMinerResolver(pstp)
      pmr.procMiner(pd)
      pmr.procResolver(pd)
    }
  }
  
  def buildSymbolTable(models : ISeq[Model],
                       stpConstructor : Unit => AndroidSymbolTableProducer,
                       parallel : Boolean) = {
    val stp = minePackageElements(models, stpConstructor, parallel)
    val tables = resolveVirtualMethod(stp)
    //resolvePackageElements(models, stp, parallel)
    buildProcedureSymbolTables(stp, parallel)
    (stp.toSymbolTable, tables)
  }
  
  def buildSymbolTable(models : ISeq[Model],
                       stpConstructor : Unit => AndroidSymbolTableProducer,
                       existingST : SymbolTable,
                       parallel : Boolean) = {
    val stp = minePackageElements(models, stpConstructor, parallel)
    val stp2 = existingST.asInstanceOf[AndroidSymbolTableProducer]
    H1.combine(stp, stp2)
    val tables = resolveVirtualMethod(stp)
    //resolvePackageElements(models, stp, parallel)
    buildProcedureSymbolTables(stp)
    (stp.toSymbolTable, tables)
  }

  
}

trait AndroidSymbolTableProducer extends SymbolTableReporter {
  def tables : AndroidSymbolTableData

  def procedureSymbolTableProducer //
  (procedureUri : ResourceUri) : AndroidProcedureSymbolTableProducer

  def toSymbolTable : SymbolTable
}

trait AndroidProcedureSymbolTableProducer {
  def tables : ProcedureSymbolTableData

  def androidSymbolTableProducer : AndroidSymbolTableProducer
}

sealed case class AndroidSymbolTableData //
(declaredSymbols : MMap[FileResourceUri, MSet[ResourceUri]] = mmapEmpty,
// constTable : MMap[ResourceUri, MBuffer[ConstDecl]] = mmapEmpty,
// constElementTable : MMap[ResourceUri, ConstElement] = mmapEmpty,
// enumTable : MMap[ResourceUri, MBuffer[EnumDecl]] = mmapEmpty,
// enumElementTable : MMap[ResourceUri, EnumElement] = mmapEmpty,
// extensionTable : MMap[ResourceUri, MBuffer[ExtensionDecl]] = mmapEmpty,
// extensionElementTable : MMap[ResourceUri, ExtElement] = mmapEmpty,
// funTable : MMap[ResourceUri, FunDecl] = mmapEmpty,
 globalVarTable : MMap[ResourceUri, GlobalVarDecl] = mmapEmpty,
 procedureTable : MMap[ResourceUri, MBuffer[ResourceUri]] = mmapEmpty,
 procedureAbsTable : MMap[ResourceUri, ProcedureDecl] = mmapEmpty,
 recordTable : MMap[ResourceUri, RecordDecl] = mmapEmpty,
// attributeTable : MMap[ResourceUri, AttributeDecl] = mmapEmpty,
// typeVarTable : MMap[ResourceUri, NameDefinition] = mmapEmpty,
// typeAliasTable : MMap[ResourceUri, TypeAliasDecl] = mmapEmpty,
// vsetTable : MMap[ResourceUri, VSetDecl] = mmapEmpty,
 dependency : DependencyMap = mmapEmpty)
