package org.sireum.amandroid

import org.sireum.amandroid.interProcedural.callGraph.CallGraph

object Center {
  type VirtualLabel = String
	private var records : Set[AmandroidRecord] = Set()
	private var applicationRecords : Set[AmandroidRecord] = Set()
	private var libraryRecords : Set[AmandroidRecord] = Set()
	private var mainRecord : AmandroidRecord = null
	private var entryPoints : Set[AmandroidProcedure] = Set()
	private var hierarchy : RecordHierarchy = null
	private var callGraph : CallGraph[VirtualLabel] = null
	
	/**
	 * get all application records
	 */
	
	def getApplicationRecords = this.applicationRecords
	
	/**
	 * get all library records
	 */
	
	def getLibraryRecords = this.libraryRecords
	
	/**
	 * add application record
	 */
	
	def addApplicationRecords(ar : AmandroidRecord) = {
    if(this.applicationRecords.contains(ar)) throw new RuntimeException("record " + ar.getName + " already exists in application record set.")
    else this.applicationRecords += ar
  }
	
	/**
	 * add library record
	 */
	
	def addLibraryRecords(l : AmandroidRecord) = {
    if(this.libraryRecords.contains(l)) throw new RuntimeException("record " + l.getName + " already exists in library record set.")
    else this.libraryRecords += l
	}
	
	/**
	 * remove application record
	 */
	
	def removeApplicationRecords(ar : AmandroidRecord) = {
    if(!this.applicationRecords.contains(ar)) throw new RuntimeException("record " + ar.getName + " does not exists in application record set.")
    else this.applicationRecords -= ar
  }
	
	/**
	 * remove library record
	 */
	
	def removeLibraryRecords(l : AmandroidRecord) = {
    if(!this.libraryRecords.contains(l)) throw new RuntimeException("record " + l.getName + " does not exists in library record set.")
    else this.libraryRecords -= l
	}
	
	/**
	 * get containing set of given record
	 */
	
	def getContainingSet(ar : AmandroidRecord) : Set[AmandroidRecord] = {
    if(ar.isApplicationRecord) this.applicationRecords
    else if(ar.isLibraryRecord) this.libraryRecords
    else null
  }
	
	/**
	 * remove given record from containing set
	 */
	
	def removeFromContainingSet(ar : AmandroidRecord) = {
    if(ar.isApplicationRecord) removeApplicationRecords(ar)
    else if(ar.isLibraryRecord) removeLibraryRecords(ar)
  }
	
	def addRecord(ar : AmandroidRecord) = {
    if(ar.isInCenter) System.err.println("already in center: " + ar)
  }
}