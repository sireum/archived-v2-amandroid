package org.sireum.amandroid

import org.sireum.util._
import org.sireum.amandroid.symbolResolver.AmandroidSymbolTable
import org.sireum.pilar.ast._
import org.sireum.alir._
import org.sireum.pilar.symbol.SymbolTable
import org.sireum.pilar.parser.ChunkingPilarParser
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.amandroid.android.intraProcedural.reachingDefinitionAnalysis._
import org.sireum.amandroid.symbolResolver.AmandroidSymbolTableBuilder
import org.sireum.amandroid.symbolResolver.AmandroidSymbolTableBuilder


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
  var par : Boolean = false
  val fst = { _ : Unit => new AmandroidSymbolTable }
  
  //for building cfg
  type VirtualLabel = String
  
  //for building rda
  val siff : ControlFlowGraph.ShouldIncludeFlowFunction =
    // ControlFlowGraph.defaultSiff,
    { (_, _) => (Array.empty[CatchClause], false) }   
  val dr : SymbolTable => DefRef = { st => new AndroidDefRef(st, new AndroidVarAccesses(st)) }
  val iopp : ProcedureSymbolTable => (ResourceUri => Boolean, ResourceUri => Boolean) = { pst =>
    val params = pst.params.toSet[ResourceUri]
    ({ localUri => params.contains(localUri) },
      { s => falsePredicate1[ResourceUri](s) })
  }
  val saom : Boolean = true
  
	def getIntraProcedureResult(code : String) : Map[ResourceUri, TransformIntraProcedureResult] = {
	  val newModels = parseCodes(Set(code))
	  doGetIntraProcedureResult(newModels)
	}
	
	def getIntraProcedureResult(codes : Set[String]) : Map[ResourceUri, TransformIntraProcedureResult] = {
	  val newModels = parseCodes(codes)
	  doGetIntraProcedureResult(newModels)
	}
	
	private def doGetIntraProcedureResult(models : List[Model]) : Map[ResourceUri, TransformIntraProcedureResult] = {
	  val result = AmandroidSymbolTableBuilder(models, fst, par)
	  result.procedureSymbolTables.map{
	    pst=>
	      val (pool, cfg) = buildCfg(pst)
	      val rda = buildRda(pst, cfg)
	      val procSig = 
	        pst.procedure.getValueAnnotation("signature") match {
			      case Some(exp : NameExp) =>
			        exp.name.name
			      case _ => throw new RuntimeException("Can not find signature")
			    }
	      (procSig, new TransformIntraProcedureResult(pst, cfg, rda))
	  }.toMap
	}
	
	def parseCodes(codes : Set[String]) : List[Model] = {
	  codes.map{v => ChunkingPilarParser(Left(v), reporter) match{case Some(m) => m; case None => null}}.filter(v => v != null).toList
	}
	
	def buildCfg(pst : ProcedureSymbolTable) = {
	  val ENTRY_NODE_LABEL = "Entry"
	  val EXIT_NODE_LABEL = "Exit"
	  val pool : AlirIntraProceduralGraph.NodePool = mmapEmpty
	  val result = ControlFlowGraph[VirtualLabel](pst, ENTRY_NODE_LABEL, EXIT_NODE_LABEL, pool, siff)
	  (pool, result)
	}
	
	def buildRda (pst : ProcedureSymbolTable, cfg : ControlFlowGraph[VirtualLabel]) = {
	  val iiopp = iopp(pst)
	  AndroidReachingDefinitionAnalysis[VirtualLabel](pst,
	    cfg,
	    dr(pst.symbolTable),
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
	
	def getSymbolResolveResult(codes : Set[String]) : SymbolTable = {
	  val newModels = parseCodes(codes)
	  AmandroidSymbolTableBuilder(newModels, fst, par)
	}
}

case class TransformIntraProcedureResult(pst : ProcedureSymbolTable, cfg : ControlFlowGraph[String], rda : AndroidReachingDefinitionAnalysis.Result)

