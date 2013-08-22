package org.sireum.amandroid.symbolResolver

import org.sireum.pilar.ast._
import org.sireum.pilar.symbol._
import org.sireum.util._
import org.sireum.pilar.symbol.SymbolTableMessage._

trait AndroidJumpResolver extends SymbolResolver {
  self : AndroidProcedureSymbolTableProducer =>

  def hasImplicitNextJump(loc : LocationDecl) : Boolean = {
      def hasImplicit(j : Jump) : Boolean =
        j match {
          case j : IfJump if j.ifElse.isEmpty          => true
          case j : SwitchJump if j.defaultCase.isEmpty => true
          case j : CallJump =>
            if (j.jump.isEmpty) true
            else hasImplicit(j.jump.get)
          case _ => false
        }
    loc match {
      case loc : ActionLocation =>
        !loc.action.isInstanceOf[ThrowAction]
      case loc : EmptyLocation  => true
      case loc : JumpLocation   => hasImplicit(loc.jump)
      case loc : ComplexLocation =>
        loc.transformations.exists { t =>
          if (t.actions.exists(_.isInstanceOf[ThrowAction])) false
          else if (t.jump.isEmpty) true
          else hasImplicit(t.jump.get)
        }
      case _ => false
    }
  }

  val jumpResolver : VisitorFunction = {
    var source : Option[FileResourceUri] = null
    var procNameDef : NameDefinition = null
    var locations : ISeq[LocationDecl] = null;
    import LineColumnLocation._
    import FileLocation._
    {
      case pd : ProcedureDecl =>
        procNameDef = pd.name
        source = procNameDef.fileUriOpt
        true
      case ib : ImplementedBody =>
        locations = ib.locations
        val lastLoc = locations(locations.size - 1)
        if (hasImplicitNextJump(lastLoc))
          androidSymbolTableProducer.reportError(source,
            lastLoc.line, lastLoc.column,
            LAST_LOCATION_NEED_EXPLICIT_JUMP)
        true
      case gj : GotoJump =>
        val nameUser = gj.target
        location(source, nameUser, procNameDef)
        false
      case iftj : IfThenJump =>
        val nameUser = iftj.target
        location(source, nameUser, procNameDef)
        false
      case scj : SwitchCaseJump =>
        val nameUser = scj.target
        location(source, nameUser, procNameDef)
        false
      case cc : CatchClause =>
        val fromNameUser = cc.fromTarget
        location(source, fromNameUser, procNameDef)
        val toNameUser = cc.toTarget
        location(source, toNameUser, procNameDef)
        val start = tables.bodyTables.get.locationTable(fromNameUser.uri).index
        val end = tables.bodyTables.get.locationTable(toNameUser.uri).index
        if (end == -1) {
          import LineColumnLocation._
          import FileLocation._
          androidSymbolTableProducer.reportError(source, toNameUser.line,
            toNameUser.column, CATCH_TABLE_END_BEFORE_START.
              format(H.symbolSimpleName(toNameUser),
                H.symbolSimpleName(fromNameUser),
                fromNameUser.line, fromNameUser.column))
        } else {
          val ct = tables.bodyTables.get.catchTable
          for (i <- start to end)
            ct.getOrElseUpdate(locations(i).index, marrayEmpty) += cc
        }
        true
      case lvd : LocalVarDecl =>
        false
      case a : Annotation =>
        false
      case g : Guard =>
        false
      case g : Exp =>
        false
    }
  }

  def location(source : Option[FileResourceUri],
               nameUser : NameUser,
               procNameDef : NameDefinition) = {
    val key = H.symbolUri(H.LOCATION_TYPE, ilist(nameUser.name), true)
    tables.bodyTables.get.locationTable.get(key) match {
      case Some(ld) =>
        val symDef = ld.name.get
        H.symbolInit(nameUser, symDef.uriType,
          symDef.uriPaths, symDef.uri)
      case None =>
        import LineColumnLocation._
        import FileLocation._
        androidSymbolTableProducer.reportError(source,
          nameUser.line, nameUser.column,
          NOT_FOUND_LOCATION.format(nameUser.name, procNameDef.name))
    }
  }
}