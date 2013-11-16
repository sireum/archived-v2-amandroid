package org.sireum.amandroid

import org.sireum.util._
import org.sireum.amandroid.symbolResolver.AmandroidSymbolTable
import org.sireum.pilar.ast._
import org.sireum.alir._
import org.sireum.pilar.symbol._
import org.sireum.pilar.parser.ChunkingPilarParser
import org.sireum.amandroid.symbolResolver.AmandroidSymbolTableBuilder
import org.sireum.amandroid.intraProcedural.reachingDefinitionAnalysis.AmandroidReachingDefinitionAnalysis


object Transform {
  //for building cfg
  type VirtualLabel = String
  
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
  
  var fst = { _ : Unit => new AmandroidSymbolTable }
  
  var dr : SymbolTable => DefRef = null
  
  val iopp : ProcedureSymbolTable => (ResourceUri => Boolean, ResourceUri => Boolean) = { pst =>
    val params = pst.params.toSet[ResourceUri]
    ({ localUri => params.contains(localUri) },
      { s => falsePredicate1[ResourceUri](s) })
  }
  
  val saom : Boolean = true
  
  def init(dr : SymbolTable => DefRef) = {
    this.dr = dr
  }
  
  def getExceptionName(cc : CatchClause) : String = {
    require(cc.typeSpec.isDefined)
    require(cc.typeSpec.get.isInstanceOf[NamedTypeSpec])
    cc.typeSpec.get.asInstanceOf[NamedTypeSpec].name.name
  }
  
  //for building cfg
  val siff : ControlFlowGraph.ShouldIncludeFlowFunction =
    { (loc, catchclauses) => 
      	var result = isetEmpty[CatchClause]
      	val thrownExcNames = ExceptionCenter.getExceptionsMayThrow(loc)
      	if(thrownExcNames.forall(_ != ExceptionCenter.ANY_EXCEPTION)){
	      	val thrownExceptions = thrownExcNames.map(Center.resolveRecord(_, Center.ResolveLevel.BODIES))
	      	thrownExceptions.foreach{
	      	  thrownException=>
	      	    val ccOpt = 
		      	    catchclauses.find{
				          catchclause =>
				            val excName = if(getExceptionName(catchclause) == ExceptionCenter.ANY_EXCEPTION) Center.DEFAULT_TOPLEVEL_OBJECT else getExceptionName(catchclause)
				            val exc = Center.resolveRecord(excName, Center.ResolveLevel.BODIES)
				          	thrownException == exc || thrownException.isChildOf(exc)
		      	    }
	      	    ccOpt match{
	      	      case Some(cc) => result += cc
	      	      case None =>
	      	    }
	      	}
      	} else {
      	  result ++= catchclauses
      	}
      	
      	(result, false)
    } 
  
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
	
	def buildRda (pst : ProcedureSymbolTable, cfg : ControlFlowGraph[VirtualLabel], initialFacts : ISet[AmandroidReachingDefinitionAnalysis.RDFact] = isetEmpty) = {
	  val iiopp = iopp(pst)
	  AmandroidReachingDefinitionAnalysis[VirtualLabel](pst,
	    cfg,
	    dr(pst.symbolTable),
	    first2(iiopp),
	    saom,
	    initialFacts)
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

case class TransformIntraProcedureResult(pst : ProcedureSymbolTable, cfg : ControlFlowGraph[String], rda : AmandroidReachingDefinitionAnalysis.Result)

