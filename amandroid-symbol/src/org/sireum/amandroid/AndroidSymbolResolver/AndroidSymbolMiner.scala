package org.sireum.amandroid.AndroidSymbolResolver

import org.sireum.pilar.symbol._
import org.sireum.util._
import org.sireum.pilar.ast._
import org.sireum.pilar.symbol.SymbolTableMessage._

trait AndroidProcedureMiner extends SymbolMiner {
  self : AndroidSymbolTableProducer =>
    
  val procedureMiner : VisitorFunction = {
    case pd : PackageDecl =>
      val packageSymDef = pd.name
      pd.elements.foreach {
        case pd : ProcedureDecl =>
          procedureSymbol(pd, packageSymDef)
          val procedureSymDef = pd.name
        case _ =>
      }
      false
    case pe : PackageElement =>
      false
    case a : Annotation =>
      false
  }

  def procedureSymbol(procedureDecl : ProcedureDecl,
                      packageSymDef : Option[SymbolDefinition]) = {
    val nameDef = procedureDecl.name
    H.symbolInit(nameDef, H.PROCEDURE_TYPE,
      H.packagePath(packageSymDef, nameDef.name))

    val key = nameDef.uri
    val pds = mmapGetOrElseUpdateT(tables.procedureTable, key,
      mlistEmpty[ResourceUri], stringInternFunction)

        import LineColumnLocation._
    H.symbolInit(nameDef, H.PROCEDURE_TYPE,
      H.packagePath(packageSymDef, nameDef.name + "/" + 
          nameDef.line + "/" + nameDef.column +
          "/" + Integer.toHexString(procedureDecl.withoutBody.toString.hashCode)))
          
    val absKey = nameDef.uri
    
    val pat = tables.procedureAbsTable
    pat.get(absKey) match {
      case Some(other) =>
        import FileLocation._
        reportError(nameDef.fileUriOpt, nameDef.line,
          nameDef.column,
          DUPLICATE_PROCEDURE.format(H.symbolSimpleName(nameDef),
            other.name.line, other.name.column))
      case _ =>
        pds += absKey.intern
        tables.procedureAbsTable(absKey.intern) = procedureDecl
    }
  }
}

/**
 * @author <a href="mailto:robby@k-state.edu">Robby</a>
 */
trait AndroidRecordMiner extends SymbolMiner {
  self : AndroidSymbolTableProducer =>
    
  val recordMiner : VisitorFunction = {
    case pd : PackageDecl =>
      val packageSymDef = pd.name
      pd.elements.foreach {
        case rd : RecordDecl =>
          recordSymbol(rd, packageSymDef)
//          val recordSymDef = rd.name
//          rd.typeVars.foreach { tv =>
//            H.typeVarSymbol(tables.typeVarTable, self, tv._1, recordSymDef)
//          }
//          rd.attributes.foreach {
//            recordAttributeSymbol(_, recordSymDef)
//          }
        case _ =>
      }
      false
    case pe : PackageElement =>
      false
    case a : Annotation =>
      false
  }

//  def recordAttributeSymbol(attribute : AttributeDecl,
//                            recordSymDef : SymbolDefinition) = {
//    val nameDef = attribute.name
//    H.symbolInit(nameDef, H.RECORD_TYPE,
//      H.paths(recordSymDef, nameDef.name))
//
//    val key = nameDef.uri
//    val at = tables.attributeTable
//    at.get(key) match {
//      case Some(other) =>
//        import LineColumnLocation._
//        import FileLocation._
//        reportError(nameDef.fileUriOpt,
//          nameDef.line, nameDef.column,
//          DUPLICATE_ATTRIBUTE.format(H.symbolSimpleName(nameDef),
//            other.name.line, other.name.column))
//      case _ =>
//        at(key.intern) = attribute
//    }
//  }

  def recordSymbol(recordDecl : RecordDecl,
                   packageSymDef : Option[SymbolDefinition]) = {
    val nameDef = recordDecl.name
    H.symbolInit(nameDef, H.RECORD_TYPE,
      H.packagePath(packageSymDef, nameDef.name))

    val key = nameDef.uri
    val rt = tables.recordTable
    rt.get(key) match {
      case Some(other) =>
        import LineColumnLocation._
        import FileLocation._
        reportError(nameDef.fileUriOpt,
          nameDef.line, nameDef.column,
          DUPLICATE_RECORD.format(H.symbolSimpleName(nameDef),
            other.name.line, other.name.column))
      case _ =>
        rt(key.intern) = recordDecl
    }
  }
}

trait AndroidProcedureSymbolMiner extends SymbolMiner {
  self : AndroidProcedureSymbolTableProducer =>
    
//  val procedureHeaderMiner : VisitorFunction = {
//    case pd : ProcedureDecl =>
//      val procedureSymDef = pd.name
//      pd.typeVars.foreach { tv =>
//        H.typeVarSymbol(tables.typeVarTable, self.symbolTableProducer,
//          tv._1, procedureSymDef)
//      }
//      val visitor = Visitor.build {
//        case p : ParamDecl =>
//          localVarSymbol(p, procedureSymDef, true)
//          false
//      }
//      pd.params.foreach { visitor(_) }
//      false
//  }

  val procedureBodyMiner : VisitorFunction = {
    case pd : ProcedureDecl =>
      val procedureSymDef = pd.name
      pd.body match {
        case ib : ImplementedBody =>
          var index = 0
          ib.locations.foreach { l => 
            locationSymbol(l, index) 
            index += 1 
          }
          ib.locals.foreach { localVarSymbol(_, procedureSymDef, false) }
        case _ =>
      }
      false
  }

  def localVarSymbol(localVar : LocalVar,
                     procedureSymDef : SymbolDefinition, isParam : Boolean) = {
    val nameDef = localVar.name
    H.symbolInit(nameDef, H.LOCAL_VAR_TYPE,
      H.paths(procedureSymDef, nameDef.name))

    val key = nameDef.uri
    val lvt = tables.localVarTable
    val params = tables.params
    lvt.get(key) match {
      case Some(other) =>
        val msg = if (isParam) DUPLICATE_PARAM else DUPLICATE_LOCAL_VAR
        import LineColumnLocation._
        import FileLocation._
        androidSymbolTableProducer.reportError(nameDef.fileUriOpt,
          nameDef.line, nameDef.column,
          msg.format(H.symbolSimpleName(nameDef),
            other.name.line, other.name.column))
      case _ =>
        lvt(key.intern) = localVar
        if (isParam)
          params += nameDef.uri
    }
  }

  def locationSymbol(loc : LocationDecl, index : Int) = {
    loc.name match {
      case Some(nameDef) =>
        H.symbolInit(nameDef, H.LOCATION_TYPE, ilist(nameDef.name), true)
        val key = nameDef.uri
        val lt = tables.bodyTables.get.locationTable
        lt.get(key) match {
          case Some(other) =>
            val otherSymDef = other.name.get
        import LineColumnLocation._
        import FileLocation._
            androidSymbolTableProducer.reportError(nameDef.fileUriOpt,
              nameDef.line, nameDef.column,
              DUPLICATE_LOCATION.format(nameDef.name,
                otherSymDef.line, otherSymDef.column))
          case _ =>
            lt(key.intern) = loc
            loc.index(index)
            H.computeLocationDescriptor(loc, Some(nameDef.uri), index)
        }
      case None =>
        loc.index(index)
        H.computeLocationDescriptor(loc, None, index)
    }
  }
}