package org.sireum.amandroid

import org.sireum.pilar.parser.ChunkingPilarParser
import org.sireum.util._
import org.sireum.pilar.symbol._
import org.sireum.pilar.ast._
import org.sireum.amandroid.symbolResolver.AndroidSymbolTable
import org.sireum.alir._
import org.sireum.amandroid.symbolResolver.AndroidLibInfoTables
import org.sireum.amandroid.android.intraProcedural.reachingDefinitionAnalysis._


object Transform {
  
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
  
  //for building symbol table
  var par : Boolean = true
  var shouldBuildLibInfoTables : Boolean = false
  val fst = { _ : Unit => new ST }
  
  //for building cfg
  type VirtualLabel = String
  
  //for building rda
  val siff : ControlFlowGraph.ShouldIncludeFlowFunction =
    // ControlFlowGraph.defaultSiff,
    { (_, _) => (Array.empty[CatchClause], false) }   
  val dr : (SymbolTable, AndroidLibInfoTables) => DefRef = { (st, alit) => new AndroidDefRef(st, new AndroidVarAccesses(st), alit) }
  val iopp : ProcedureSymbolTable => (ResourceUri => Boolean, ResourceUri => Boolean) = { pst =>
    val params = pst.params.toSet[ResourceUri]
    ({ localUri => params.contains(localUri) },
      { s => falsePredicate1[ResourceUri](s) })
  }
  val saom : Boolean = true
  
	def run(code : String, libInfoTable : AndroidLibInfoTables) : Map[ResourceUri, TransformResult] = {
	  val newModels = ChunkingPilarParser(Left(code), reporter) match{case Some(m) => List(m); case None => List()}
	  doRun(newModels, libInfoTable)
	}
	
	def run(codes : Set[String], libInfoTable : AndroidLibInfoTables) : Map[ResourceUri, TransformResult] = {
	  val newModels = codes.map{v => ChunkingPilarParser(Left(v), reporter) match{case Some(m) => m; case None => null}}.filter(v => v != null).toList
	  doRun(newModels, libInfoTable)
	}
	
	private def doRun(models : List[Model], libInfoTable : AndroidLibInfoTables) : Map[ResourceUri, TransformResult] = {
	  val result = AndroidSymbolTable(models, fst, par, shouldBuildLibInfoTables)
	  result._1.procedureSymbolTables.map{
	    pst=>
	      val (pool, cfg) = buildCfg(pst)
	      val rda = buildRda(pst, cfg, libInfoTable)
	      (pst.procedureUri, new TransformResult(pst, cfg, rda))
	  }.toMap
	}
	
	def buildCfg(pst : ProcedureSymbolTable) = {
	  val ENTRY_NODE_LABEL = "Entry"
	  val EXIT_NODE_LABEL = "Exit"
	  val pool : AlirIntraProceduralGraph.NodePool = mmapEmpty
	  val result = ControlFlowGraph[VirtualLabel](pst, ENTRY_NODE_LABEL, EXIT_NODE_LABEL, pool, siff)
	  (pool, result)
	}
	
	def buildRda (pst : ProcedureSymbolTable, cfg : ControlFlowGraph[VirtualLabel], libInfoTable : AndroidLibInfoTables) = {
	  val iiopp = iopp(pst)
	  AndroidReachingDefinitionAnalysis[VirtualLabel](pst,
	    cfg,
	    dr(pst.symbolTable, libInfoTable),
	    first2(iiopp),
	    saom)
	}
	
	def reporter = {
	  new org.sireum.pilar.parser.PilarParser.ErrorReporter {
      def report(source : Option[FileResourceUri], line : Int,
                 column : Int, message : String) =
        System.err.println("source:" + source + ".line:" + line + ".column:" + column + "message" + message)
    }
	}
}

case class TransformResult(pst : ProcedureSymbolTable, cfg : ControlFlowGraph[String], rda : AndroidReachingDefinitionAnalysis.Result)

class ST extends SymbolTable with SymbolTableProducer {
    st =>
      
    import Transform.ERROR_TAG_TYPE
    import Transform.WARNING_TAG_TYPE
    
    val tables = SymbolTableData()
    val tags = marrayEmpty[LocationTag]
    var hasErrors = false
     
    def reportError(source : Option[FileResourceUri], line : Int,
                    column : Int, message : String) : Unit = {
      tags += Tag.toTag(source, line, column, message, ERROR_TAG_TYPE)
      hasErrors = true
    }

    def reportWarning(fileUri : Option[String], line : Int,
                      column : Int, message : String) : Unit =
      tags += Tag.toTag(fileUri, line, column, message, WARNING_TAG_TYPE)

    val pdMap = mmapEmpty[ResourceUri, PST]

    def globalVars = null
    def globalVar(globalUri : ResourceUri) = null

    def procedures = tables.procedureTable.keys

    def procedures(procedureUri : ResourceUri) = tables.procedureTable(procedureUri)

    def procedureSymbolTables = pdMap.values

    def procedureSymbolTable(procedureAbsUri : ResourceUri) : ProcedureSymbolTable =
      procedureSymbolTableProducer(procedureAbsUri)

    def procedureSymbolTableProducer(procedureAbsUri : ResourceUri) = {
      assert(tables.procedureAbsTable.contains(procedureAbsUri))
      pdMap.getOrElseUpdate(procedureAbsUri, new PST(procedureAbsUri))
    }

class PST(val procedureUri : ResourceUri)
      extends ProcedureSymbolTable with ProcedureSymbolTableProducer {
    val tables = ProcedureSymbolTableData()
    var nextLocTable : CMap[ResourceUri, ResourceUri] = null
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
        case _        => ivectorEmpty
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