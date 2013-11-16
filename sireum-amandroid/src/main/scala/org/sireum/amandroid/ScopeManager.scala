package org.sireum.amandroid

import org.sireum.util.ISet

trait ScopeManager {
  def setMode(inScopeMode : Boolean)
  /**
   * return true means use include mode, any package defined in ScopeManager will be keep
   * during the analysis, and vice versa.
   */
	def isIncludeMode : Boolean
	
	def addPackage(packageName : String)
	def addPackages(packageNames : ISet[String])
  def removePackage(packageName : String)
	def removePackages(packageNames : ISet[String])
	
	/**
	 * return true if given package name contained in the scope manager
	 */
	def contains(packageName : String) : Boolean
	/**
	 * return true if given record needs to be bypassed
	 */
	def shouldBypass(rec : AmandroidRecord) : Boolean
	def clear
}