package org.sireum.amandroid.alir.interProcedural.reachingFactsAnalysis

import org.sireum.jawa.ScopeManager
import org.sireum.util._
import org.sireum.jawa.JawaRecord
import org.sireum.jawa.alir.interProcedural.reachingFactsAnalysis.RFAFact
import org.sireum.jawa.JawaProcedure
import org.sireum.jawa.alir.Context
import org.sireum.jawa.alir.interProcedural.reachingFactsAnalysis.VarSlot
import org.sireum.amandroid.alir.AndroidConstants
import org.sireum.jawa.alir.UnknownInstance
import org.sireum.jawa.alir.interProcedural.reachingFactsAnalysis.ReachingFactsAnalysisHelper
import org.sireum.jawa.Center

object AndroidRFAScopeManager extends ScopeManager{
  private var packages : ISet[String] = isetEmpty
  private var includeMode = true
  def setMode(includeMode : Boolean) = this.includeMode = includeMode
	/**
   * return true means use in scope mode, any package defined in ScopeManager will be keep
   * during the analysis, and vice versa.
   */
	def isIncludeMode : Boolean = this.includeMode
	
	def addPackage(packageName : String) = this.packages += packageName
	def addPackages(packageNames : ISet[String]) = this.packages ++= packageNames
	def removePackage(packageName : String) = this.packages -= packageName
	def removePackages(packageNames : ISet[String]) = this.packages --= packageNames
	
	/**
	 * return true if given package name contained in the scope manager
	 */
	def contains(packageName : String) : Boolean = this.packages.contains(packageName)
	def clear = this.packages = isetEmpty
	
	/**
	 * return true if given record needs to be bypassed
	 */
	def shouldBypass(rec : JawaRecord) : Boolean = {
    rec.getName == Center.UNKNOWN_RECORD ||
    {
	    rec.isLibraryRecord &&
	    {
		    if(isIncludeMode){
		    	if(rec.getPackageName != null) !contains(rec.getPackageName) else true
		    } else {
		      if(rec.getPackageName != null) contains(rec.getPackageName) else false
		    }
	    }
    }
  }
  
  def handleBypass(s : ISet[RFAFact], calleeProc : JawaProcedure, args : List[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact], Boolean) = {
    (isetEmpty, isetEmpty, true)
  }
}