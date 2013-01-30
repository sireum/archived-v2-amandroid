package org.sireum.amandroid.AndroidSymbolResolver

import scala.collection.JavaConversions._
import org.sireum.pilar.ast._
import org.sireum.util._
import org.sireum.pilar.symbol._
import com.google.common.collect.{HashMultimap, HashBiMap}
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
            existingAndroidVirtualMethodTables : AndroidVirtualMethodTables,
            parallel : Boolean) =
    buildSymbolTable(models, stpConstructor, existingAndroidVirtualMethodTables, parallel)


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
    val rut = HashBiMap.create[String, ResourceUri]()
    val put = HashBiMap.create[String, ResourceUri]()
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
    def recordUriTable : HashBiMap[String, ResourceUri] = tables.recordUriTable
    def procedureUriTable : HashBiMap[String, ResourceUri] = tables.procedureUriTable
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
        virtualMethodTable.get(procedureUri)
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
      combineTables(anotherVmTables)
      mergeRecordHierarchyTableAndVirtualMethodTable()
      
    }
    
    def combineTables(anotherVmTables : AndroidVirtualMethodTables) = {
      virtualMethodTable.putAll(anotherVmTables.virtualMethodTable)
      recordProcedureTable.putAll(anotherVmTables.recordProcedureTable)
      recordHierarchyTable.putAll(anotherVmTables.recordHierarchyTable)
      cannotFindRecordTable.putAll(anotherVmTables.cannotFindRecordTable)
      procedureTypeTable ++= anotherVmTables.procedureTypeTable
      procedureUriTable ++= anotherVmTables.procedureUriTable
      recordUriTable ++= anotherVmTables.recordUriTable
      interfaceTable ++= anotherVmTables.interfaceTable
    }
    
    def mergeRecordHierarchyTableAndVirtualMethodTable() = {
      var tempNotFoundRecordsTable : HashMultimap[ResourceUri, ResourceUri] = HashMultimap.create()
      val recordSets = recordUriTable.values.toSeq
      var keys = cannotFindRecordTable.keySet
      keys.map(
        key=>
        {
          val notFoundRecords = cannotFindRecordTable.get(key)
          notFoundRecords.map(
            notFoundRecord =>
              if(recordSets.contains(notFoundRecord)){
                println("find: " + notFoundRecord)
                recordHierarchyTable.put(key, notFoundRecord)
                resolveVM(key)
              } 
              else {
                tempNotFoundRecordsTable.put(key, notFoundRecord)
              }
          )
        }
      )
      cannotFindRecordTable.clear()
      cannotFindRecordTable.putAll(tempNotFoundRecordsTable)
    }
    
    def isInterface(recordUri : ResourceUri) : Boolean = {
      if(interfaceTable.contains(recordUri)) true
      else false
    }
    
    def isConsStaticFinalOrProtected(access : String) : Boolean = {
      if(access != null){
        if(access.contains("CONSTRUCTOR"))
          true
        else if(access.contains("STATIC"))
          true
        else if(access.contains("FINAL"))
          true
        else if(access.contains("PROTECTED"))
          true
        else
          false
      } else {
        false
      }
    }
    
    def getInside(name : String) : String = {
      val rName = name.substring(2, name.length()-2)
      rName
    }
    
    def getPartSig(sig : String) : String = {
      getInside(sig).split(";", 2)(1)
    }
    
    def sigEqual(procedureUri : ResourceUri, parentProcedureUri : ResourceUri) : Boolean = {
      val sig1 = procedureUriTable.inverse.get(procedureUri)
      val sig2 = procedureUriTable.inverse.get(parentProcedureUri)
      if(sig1 != null && sig2 !=null && getPartSig(sig1).equals(getPartSig(sig2))){
        true
      } else {
        false
      }
    }
    
    def buildVirtualMethodTable(from : ResourceUri, to : ResourceUri) : Unit = {
      if(!virtualMethodTable.get(from).contains(to))
        virtualMethodTable.put(from, to)
    }
    
    def addRelation(recordUri : ResourceUri, procedureUri : ResourceUri) : Boolean = {
      val parentsUri = recordHierarchyTable.get(recordUri)
      if(parentsUri.isEmpty()) true
      else{
        for(parentUri <- parentsUri){
          val parentProceduresUri = recordProcedureTable.get(parentUri)
          for (parentProcedureUri <- parentProceduresUri){
            if(sigEqual(procedureUri, parentProcedureUri)){
              val vmPreceduresUri = virtualMethodTable.get(procedureUri)
              println("procedures : " + vmPreceduresUri)
              vmPreceduresUri.map(
                pUri =>
                {
                  buildVirtualMethodTable(parentProcedureUri, pUri)
                }
              )
              
            }
          }
          addRelation(parentUri, procedureUri)
        }
        false
      }
    }
    
    def resolveVM(recordUri : ResourceUri) = {
      if(!isInterface(recordUri)){
        val proceduresUri = recordProcedureTable.get(recordUri)
        proceduresUri.foreach(
          procedureUri =>
          {
            if(!isConsStaticFinalOrProtected(procedureTypeTable(procedureUri))){
              addRelation(recordUri, procedureUri)
            }
          }
        )
      }
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
                       existingAndroidVirtualMethodTables : AndroidVirtualMethodTables,
                       parallel : Boolean) = {
    val stp = minePackageElements(models, stpConstructor, parallel)
    val tables = resolveVirtualMethod(stp)
    //resolvePackageElements(models, stp, parallel)
    buildProcedureSymbolTables(stp)
    println("before merge : " + tables.cannotFindRecordTable)
    tables.mergeWith(existingAndroidVirtualMethodTables)
    println("after merge : " + tables.cannotFindRecordTable)
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
