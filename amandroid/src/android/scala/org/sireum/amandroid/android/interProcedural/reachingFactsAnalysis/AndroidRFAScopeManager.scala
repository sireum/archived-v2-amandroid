package org.sireum.amandroid.android.interProcedural.reachingFactsAnalysis

import org.sireum.amandroid.ScopeManager
import org.sireum.util._
import org.sireum.amandroid.AmandroidRecord
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAFact
import org.sireum.amandroid.AmandroidProcedure
import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.VarSlot
import org.sireum.amandroid.android.AndroidConstants
import org.sireum.amandroid.UnknownInstance
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.ReachingFactsAnalysisHelper

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
	
	/**
	 * return true if given package name contained in the scope manager
	 */
	def contains(packageName : String) : Boolean = this.packages.contains(packageName)
	def clear = this.packages = isetEmpty
	
	/**
	 * return true if given record needs to be bypassed
	 */
	def shouldBypass(rec : AmandroidRecord) : Boolean = {
    rec.getName == AndroidConstants.UNKNOWN_RECORD ||
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
  
  def handleBypass(s : ISet[RFAFact], calleeProc : AmandroidProcedure, args : List[String], retVars : Seq[String], currentContext : Context) : ISet[RFAFact] = {
    s ++ ReachingFactsAnalysisHelper.checkAndGetUnknownObjectForRetVar(isetEmpty, retVars, currentContext)
  }
}