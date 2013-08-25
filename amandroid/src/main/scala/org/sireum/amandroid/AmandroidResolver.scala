package org.sireum.amandroid

import org.sireum.pilar.symbol.SymbolTableProducer
import org.sireum.pilar.ast._
import org.sireum.util._
import scala.collection.GenMap
import org.sireum.pilar.symbol.ProcedureSymbolTable

/**
 * collect info from symbol table and build Center, AmandroidRecord, and AmandroidProcedure
 */

object AmandroidResolver {
  
  val DEBUG : Boolean = true
  val TITLE : String = "AmandroidResolver"
    
  val DEFAULT_TOPLEVEL_OBJECT = "[|java:lang:Object|]"
	
	def resolveFromSTP(stp : SymbolTableProducer, par : Boolean) = {
    if(GlobalConfig.wholeProgram && !AmandroidCodeSource.isPreLoaded) throw new RuntimeException("In whole program mode but library code did not pre-loaded, call AmandroidCodeSource.preLoad first.")
	  resolveRecords(stp, par)
	  resolveProcedures(stp, par)
	  if(DEBUG){
	    Center.printDetails
	  }
	}
	
	/**
	 * collect record info from symbol table
	 */
	
	def resolveRecords(stp : SymbolTableProducer, par : Boolean) = {
	  if(DEBUG) println("Doing " + TITLE + ". Resolve records parallel: " + par)
	  val col : GenMap[ResourceUri, RecordDecl] = if(par) stp.tables.recordTable.par else stp.tables.recordTable
	  val records = col.map{
	    case (uri, rd) =>
	      val recName = rd.name.name
	      val recAccessFlag =					// It can be PUBLIC ... or empty (which means no access flag class)
	        rd.getValueAnnotation("AccessFlag") match {
            case Some(exp : NameExp) =>
              exp.name.name
            case _ => ""
          }
	      val rec : AmandroidRecord = new AmandroidRecord
	      rec.init(recName)
	      rec.setAccessFlags(recAccessFlag)
	      var exs = rd.extendsClauses.map {_.name.name}.toSet
	      if((exs.isEmpty && recName != DEFAULT_TOPLEVEL_OBJECT) || rec.isInterface) exs += DEFAULT_TOPLEVEL_OBJECT
	      rec.addNeedToResolveExtends(exs)
	      rec
	  }
	  records.foreach(Center.addRecord(_))
	  Center.resolveRecordsRelation
	}
	
	/**
	 * collect procedure info from symbol table
	 */
	
	def resolveProcedures(stp : SymbolTableProducer, par : Boolean) = {
	  if(DEBUG) println("Doing " + TITLE + ". Resolve procedures parallel: " + par)
	  val col : GenMap[ResourceUri, ProcedureDecl] = if(par) stp.tables.procedureAbsTable.par else stp.tables.procedureAbsTable
	  val ownerRelation = col.map{
	    case (uri, pd) =>
	      val procName = pd.name.name
	      val procSig = 
	        pd.getValueAnnotation("signature") match {
			      case Some(exp : NameExp) =>
			        exp.name.name
			      case _ => throw new RuntimeException("Doing " + TITLE + ": Can not find signature from: " + procName)
			    }
	      val procAccessFlag = 
	        pd.getValueAnnotation("Access") match {
			      case Some(exp : NameExp) =>
			        exp.name.name
			      case _ => ""
			    }
	      val ownerName =
          pd.getValueAnnotation("owner") match {
            case Some(exp : NameExp) => 
              exp.name.name
            case _ => throw new RuntimeException("Doing " + TITLE + ": Can not find owner from: " + procName)
          }
	      
	      val proc : AmandroidProcedure = new AmandroidProcedure
	      proc.init(procName, procSig)
	      proc.setAccessFlags(procAccessFlag)
	      val ownerRecord = Center.getRecord(ownerName)
	      proc.setProcedureBody(stp.procedureSymbolTableProducer(uri).asInstanceOf[ProcedureBody])
	      (proc, ownerRecord)
	  }
	  if(ownerRelation.isParallel) throw new RuntimeException("Doing " + TITLE + ": ownerRelation is parallel, but we are try to adding things to AmandroidRecord.")
	  ownerRelation.foreach{
	    case (proc, own) =>
	      own.addProcedure(proc)
	  }
	}
	
//	/**
//	 * resolve records relations include whole library
//	 */
//	
//	def resolveRecordsRelationWholeProgram = {
//	  
//	}

//	def resolveProcedureBody(pst : ProcedureSymbolTable, par : Boolean) = {
//	  pst.locations
//	}
	
}