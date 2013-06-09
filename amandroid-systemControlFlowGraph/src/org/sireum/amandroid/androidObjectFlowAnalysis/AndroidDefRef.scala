package org.sireum.amandroid.androidObjectFlowAnalysis

import org.sireum.pilar.symbol.SymbolTable
import org.sireum.alir.DefRef
import org.sireum.alir.VarAccesses
import org.sireum.pilar.ast._
import org.sireum.util._
import org.sireum.alir.Slot
import org.sireum.pilar.symbol.H
import org.sireum.alir.VarSlot
import org.sireum.amandroid.AndroidSymbolResolver.AndroidLibInfoTables
import org.sireum.pilar.symbol.Symbol.pp2r


final class AndroidVarAccesses(st: SymbolTable) extends VarAccesses {
  def localVarAccesses(procedureUri: ResourceUri): CSet[Slot] =
    procedureLocalAccessCache(procedureUri)

  def globalVarReads(procedureUri: ResourceUri): CSet[Slot] =
    procedureGlobalReadCache(procedureUri)

  def globalVarWrites(procedureUri: ResourceUri): CSet[Slot] =
    procedureGlobalWriteCache(procedureUri)

  def strongGlobalVarWrites(procedureUri: ResourceUri): CSet[Slot] =
    Set()

  private val (procedureLocalAccessCache, procedureGlobalReadCache, procedureGlobalWriteCache) = {
    val localAccesses = mmapEmpty[ResourceUri, MSet[Slot]]
    val globalReads = mmapEmpty[ResourceUri, MSet[Slot]]
    val globalWrites = mmapEmpty[ResourceUri, MSet[Slot]]

    def init() {
      var accessLocalVars = msetEmpty[Slot]
      var readGlobalVars = msetEmpty[Slot]
      var writtenGlobalVars = msetEmpty[Slot]

      def addLocalAccess(ne: NameExp) =
        if (H.isLocalVar(ne.name))
          accessLocalVars += VarSlot(ne.name.uri)

      val visitor = Visitor.build({
        case a: Assignment =>
          val lhss = PilarAstUtil.getLHSs(a)
          for (NameExp(name) <- lhss.keys)
            if (name.hasResourceInfo && H.isGlobalVar(name))
              writtenGlobalVars += VarSlot(name.uri)
          Visitor.build({
            case ne: NameExp =>
              if (ne.name.hasResourceInfo)
                if (H.isGlobalVar(ne.name) && !lhss.contains(ne))
                  readGlobalVars += VarSlot(ne.name.uri)
                else
                  addLocalAccess(ne)
              false
          })(a)
          false
        case ne: NameExp =>
          if (ne.name.hasResourceInfo)
            if (H.isGlobalVar(ne.name))
              readGlobalVars += VarSlot(ne.name.uri)
            else
              addLocalAccess(ne)
          false
      })

      st.procedureSymbolTables.map { pst =>
        val p = pst.procedure
        visitor(p)
        localAccesses(pst.procedureUri) = accessLocalVars
        globalReads(pst.procedureUri) = readGlobalVars
        globalWrites(pst.procedureUri) = writtenGlobalVars
        accessLocalVars = msetEmpty[Slot]
        readGlobalVars = msetEmpty[Slot]
        writtenGlobalVars = msetEmpty[Slot]
      }
    }
    init()
    (localAccesses, globalReads, globalWrites)
  }
}


final class AndroidDefRef(st: SymbolTable, val varAccesses: VarAccesses, alit : AndroidLibInfoTables)
  extends DefRef {

  def definitions(a: Assignment): ISet[Slot] = {
    import org.sireum.pilar.symbol.H

    strongDefinitions(a)
  }

  def strongDefinitions(a: Assignment): ISet[Slot] =
    defCache.getOrElseUpdate(a, {
      val lhss = PilarAstUtil.getLHSs(a)
      var result = isetEmpty[Slot]
      for (ne @ NameExp(_) <- lhss.keys) {
        var uri : ResourceUri = null
        if(!ne.name.hasResourceInfo){
          
        	uri = alit.getGlobalVarUriByName(ne.name.name)
        } else {
          uri = ne.name.uri
        }
        result = result + VarSlot(uri)
      }
      result
    })

  def references(a: Action): ISet[Slot] =
    refCache.getOrElseUpdate(a, getRefs(a))

  def references(j: Jump): ISet[Slot] =
    refCache.getOrElseUpdate(j, getRefs(j))

  def callReferences(j: CallJump): ISeq[ISet[Slot]] = {
    val arg = j.callExp.arg
    arg match {
      case e: TupleExp =>
        val result = e.exps.map { exp => refCache.getOrElseUpdate(exp, getRefs(exp)) }
        result
      case e =>
        ivector(refCache.getOrElseUpdate(j, getRefs(e)))
    }
  }

  def callDefinitions(j: CallJump): ISeq[ISet[Slot]] = {
    val arg = j.callExp.arg
    arg match {
      case e: TupleExp =>
        e.exps.map { exp => isetEmpty[Slot] }
      case e =>
        ivector(isetEmpty[Slot])
    }
  }

  private def getRefs(n: PilarAstNode): ISet[Slot] = {
    var result = isetEmpty[Slot]
    val lhss = PilarAstUtil.getLHSs(n)
    Visitor.build({
      case ne: NameExp =>
        if (!lhss.contains(ne))
          result = result + VarSlot(ne.name.uri)
        false
    })(n)
    result
  }

  private val defCache = idmapEmpty[Assignment, ISet[Slot]]
  private val refCache = idmapEmpty[PilarAstNode, ISet[Slot]]
}