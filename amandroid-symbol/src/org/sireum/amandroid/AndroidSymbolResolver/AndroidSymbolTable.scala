package org.sireum.amandroid.AndroidSymbolResolver

import scala.collection.JavaConversions._
import org.sireum.pilar.ast._
import org.sireum.util._
import org.sireum.pilar.symbol._
import com.google.common.collect.{HashMultimap, HashBiMap}
import org.sireum.amandroid.AndroidSymbolResolver._
import scala.collection.parallel._


/**
 * @author <a href="mailto:robby@k-state.edu">Robby</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */

trait AndroidSymbolTable extends SymbolTable


/**
 * @author <a href="mailto:robby@k-state.edu">Robby</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */


object AndroidSymbolTable {
  def apply(models : ISeq[Model],
            stpConstructor : Unit => SymbolTableProducer,
            parallel : Boolean,
            shouldBuildLibInfoTables : Boolean) =
    buildSymbolTable(models, stpConstructor, parallel, shouldBuildLibInfoTables)
    
  def apply(models : ISeq[Model],
            stpConstructor : Unit => SymbolTableProducer,
            existingLibInfoTables : AndroidLibInfoTables,
            parallel : Boolean,
            shouldBuildLibInfoTables : Boolean) =
    buildSymbolTable(models, stpConstructor, existingLibInfoTables, parallel, shouldBuildLibInfoTables)
    
  def apply(models : ISeq[Model],
            stpConstructor : Unit => SymbolTableProducer,
            existingLibInfoTables : AndroidLibInfoTables,
            parallel : Boolean) = 
    buildLibInfoTables(models, stpConstructor, existingLibInfoTables, parallel)

  def apply[P <: SymbolTableProducer] //
  (stp : SymbolTableProducer, stModels : ISeq[Model],
   changedOrDeletedModelFiles : Set[FileResourceUri],
   changedOrAddedModels : ISeq[Model],
   stpConstructor : Unit => P,
   existingAndroidLibInfoTables : AndroidLibInfoTables,
   parallel : Boolean,
   shouldBuildLibInfoTables : Boolean) =
    fixSymbolTable(stp, stModels, changedOrDeletedModelFiles,
      changedOrAddedModels, stpConstructor, existingAndroidLibInfoTables, parallel, shouldBuildLibInfoTables)

  def minePackageElements[P <: SymbolTableProducer] //
  (models : ISeq[Model], stpConstructor : Unit => P,
   parallel : Boolean) : SymbolTableProducer = {
    if (models.isEmpty) return stpConstructor()

    val ms : GenSeq[Model] = if (parallel) models.par else models
    ms.map { model =>
      val stp = stpConstructor()
      new H.PackageElementMiner(stp).packageElementMiner(model)
      val tables = stp.tables
      model.sourceURI.foreach { fileUri =>
        val set = msetEmpty[ResourceUri]
        set ++= tables.constTable.keys
        set ++= tables.constElementTable.keys
        set ++= tables.enumTable.keys
        set ++= tables.enumElementTable.keys
        set ++= tables.extensionTable.keys
        set ++= tables.extensionElementTable.keys
        set ++= tables.funTable.keys
        set ++= tables.globalVarTable.keys
        set ++= tables.procedureTable.keys
        set ++= tables.procedureAbsTable.keys
        set ++= tables.recordTable.keys
        set ++= tables.attributeTable.keys
        set ++= tables.typeVarTable.keys
        set ++= tables.typeAliasTable.keys
         set ++= tables.vsetTable.keys
        tables.declaredSymbols(fileUri) = set
      }
      stp
    }.toIterable.reduce(H.combine)
  }

  def resolveVirtualMethod(stp : SymbolTableProducer) : AndroidLibInfoTables = {
    val avmr = new AndroidLibInfoResolver
    avmr.resolveAndroidLibInfo(stp)
    avmr.toAndroidLibInfoTables
  }
  
  def buildProcedureSymbolTables(stp : SymbolTableProducer, parallel : Boolean) : Unit = {
    val procedures = stp.tables.procedureAbsTable.keys.toSeq
    doBuildPST(stp, procedures, parallel)
  }
  
  def fixProcedureSymbolTables(stp : SymbolTableProducer, newProcedures : Seq[ResourceUri], parallel : Boolean) : Unit = {
    doBuildPST(stp, newProcedures, parallel)
  }
  
  private def doBuildPST(stp : SymbolTableProducer, procedures : Seq[ResourceUri], parallel : Boolean = true) : Unit = {
    println("parallel=" + parallel)
    val col : GenSeq[ResourceUri] = if (false) procedures.par else procedures
    println("col size=" + col.size)
    col.map { procedureUri =>
      val pstp = stp.procedureSymbolTableProducer(procedureUri)
      val pd = stp.tables.procedureAbsTable(procedureUri)
      pd.body match {
        case body : ImplementedBody =>
          pstp.tables.bodyTables =
            Some(BodySymbolTableData())
        case body : EmptyBody =>
      }
      val pmr = new H.ProcedureMinerResolver(pstp)
      pmr.procMiner(pd)
      pmr.procResolver(pd)
    }
  }
  
  def buildSymbolTable(models : ISeq[Model],
                       stpConstructor : Unit => SymbolTableProducer,
                       parallel : Boolean,
                       shouldBuildLibInfoTables : Boolean) = {
    val stp = minePackageElements(models, stpConstructor, parallel)
    val tablesOpt = if(shouldBuildLibInfoTables) Some(resolveVirtualMethod(stp)) else None
    resolvePackageElements(models, stp, parallel)
    buildProcedureSymbolTables(stp, parallel)
    (stp.toSymbolTable, tablesOpt)
  }
  
  def resolvePackageElements(models : ISeq[Model], stp : SymbolTableProducer,
                             parallel : Boolean) : Unit = {
    if (models.isEmpty) return

    val ms : GenSeq[Model] = if (parallel) models.par else models

    val dependencies = ms.map { model =>
      val per = new H.PackageElementResolver(stp)
      per.packageElementResolver(model)
      per.dependency
    }
    dependencies.foldLeft(stp.tables.dependency)(H.combineMap)
  }
  
  def buildSymbolTable(models : ISeq[Model],
                       stpConstructor : Unit => SymbolTableProducer,
                       existingAndroidLibInfoTables : AndroidLibInfoTables,
                       parallel : Boolean,
                       shouldBuildLibInfoTables : Boolean) = {
    val stp = minePackageElements(models, stpConstructor, parallel)
    val tablesOpt = if(shouldBuildLibInfoTables) Some(resolveVirtualMethod(stp)) else None
    resolvePackageElements(models, stp, parallel)
    buildProcedureSymbolTables(stp, parallel)
    tablesOpt match {
      case Some(tables) =>
        println("before merge : " + tables.asInstanceOf[AndroidLibInfoTablesProducer].tables.cannotFindRecordTable)
	    tables.mergeWith(existingAndroidLibInfoTables)
	    tables.asInstanceOf[AndroidLibInfoResolver].separateInterfaceImplementAndClassExtend
	    println("after merge : " + tables.asInstanceOf[AndroidLibInfoTablesProducer].tables.cannotFindRecordTable)
	    (stp.toSymbolTable, Some(tables))
      case None =>
        (stp.toSymbolTable, Some(existingAndroidLibInfoTables))
    }
  }
  
  def buildLibInfoTables(models : ISeq[Model],
            stpConstructor : Unit => SymbolTableProducer,
            existingLibInfoTables : AndroidLibInfoTables,
            parallel : Boolean) = {
    val stp = minePackageElements(models, stpConstructor, parallel)
    val tables = resolveVirtualMethod(stp)
    println("before merge : " + tables.asInstanceOf[AndroidLibInfoTablesProducer].tables.cannotFindRecordTable)
    tables.mergeWith(existingLibInfoTables)
    println("after merge : " + tables.asInstanceOf[AndroidLibInfoTablesProducer].tables.cannotFindRecordTable)
    (stp.toSymbolTable, Some(tables))
  }
  
  def fixSymbolTable[P <: SymbolTableProducer] //
  (stp : SymbolTableProducer, stModels : ISeq[Model],
   changedOrDeletedModelFiles : Set[FileResourceUri],
   changedOrAddedModels : ISeq[Model],
   stpConstructor : Unit => P,
   existingAndroidLibInfoTables : AndroidLibInfoTables,
   parallel : Boolean,
   shouldBuildLibInfoTables : Boolean) = {

    val models = mlistEmpty[Model]
    stModels.foreach { m =>
      m.sourceURI match {
        case Some(uri) =>
          if (changedOrDeletedModelFiles.contains(uri))
            H.tearDown(stp.tables, m)
          else
            models += m
        case _ =>
      }
    }
    val newStp = minePackageElements(changedOrAddedModels, stpConstructor, parallel)
    
    val tablesOpt = if(shouldBuildLibInfoTables) Some(resolveVirtualMethod(newStp)) else None
    H.combine(stp, newStp)
    models ++= changedOrAddedModels
    resolvePackageElements(models.toList, stp, parallel)
    fixProcedureSymbolTables(stp, newStp.tables.procedureAbsTable.keySet.toSeq, parallel)
    tablesOpt match {
      case Some(tables) =>
        println("before merge : " + tables.asInstanceOf[AndroidLibInfoTablesProducer].tables.cannotFindRecordTable)
		    tables.mergeWith(existingAndroidLibInfoTables)
		    println("after merge : " + tables.asInstanceOf[AndroidLibInfoTablesProducer].tables.cannotFindRecordTable)
		    (stp.toSymbolTable, Some(tables))
      case None =>
        (stp.toSymbolTable, Some(existingAndroidLibInfoTables))
    }
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
