package org.sireum.amandroid.module

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
            stpConstructor : Unit => SymbolTableProducer,
            parallel : Boolean) =
    buildSymbolTable(models, stpConstructor, parallel)


  def minePackageElements[P <: SymbolTableProducer] //
  (models : ISeq[Model], stpConstructor : Unit => P,
   parallel : Boolean) : SymbolTableProducer = {
    if (models.isEmpty) return stpConstructor()

    val ms : GenSeq[Model] = if (parallel) models.par else models
    ms.map { model =>
      val stp = stpConstructor()
      new H1.PackageElementMiner(stp).packageElementMiner(model)
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
        // set ++= tables.vsetTable.keys
        tables.declaredSymbols(fileUri) = set
      }
      // println("AndroidSymbolTable :: AndroidSymbolTable :: minePackageElements :: OK2.1") // sankar testing
      stp
    }.toIterable.reduce(H1.combine)
    
  }

  def resolveVirtualMethod(stp : SymbolTableProducer) : AndroidVirtualMethodTables = {
    val rht = HashMultimap.create[String, String]()
    val vmt = HashMultimap.create[String, String]()
    new Object with AndroidVirtualMethodResolver {
      override def recordHierarchyTable = rht
      override def virtualMethodTable = vmt
    }.androidVirtualMethodResolver(stp)
    val favmt = {_ : Unit => new AVMT}
    AndroidVirtualMethodTables(rht, vmt, favmt)
  }
  
  class AVMT extends AndroidVirtualMethodTables with AndroidVirtualMethodTablesProducer {
    avmt =>
    
    val tables = AndroidVirtualMethodTablesData()
    
    def recordHierarchyTable : HashMultimap[String, String] = tables.recordHierarchyTable
    def virtualMethodTable : HashMultimap[String, String] = tables.virtualMethodTable
    
    def toAndroidVirtualMethodTables : AndroidVirtualMethodTables = this
  }

  def resolvePackageElements(models : ISeq[Model], stp : SymbolTableProducer,
                             parallel : Boolean) : Unit = {
    if (models.isEmpty) return

    val ms : GenSeq[Model] = if (parallel) models.par else models

    val dependencies = ms.map { model =>
      val per = new H1.PackageElementResolver(stp)
      per.packageElementResolver(model)
      per.dependency
    }
    dependencies.foldLeft(stp.tables.dependency)(H.combineMap)
    
    // println("AndroidSymbolTable :: AndroidSymbolTable :: resolvePackageElements :: OK2.2") // sankar testing
  }

  
  def buildSymbolTable(models : ISeq[Model],
                       stpConstructor : Unit => SymbolTableProducer,
                       parallel : Boolean) = {
    val stp = minePackageElements(models, stpConstructor, parallel)
    val tables = resolveVirtualMethod(stp)
    resolvePackageElements(models, stp, parallel)
    SymbolTable.buildProcedureSymbolTables(stp)
    (stp.toSymbolTable, tables)
  }


  
}

