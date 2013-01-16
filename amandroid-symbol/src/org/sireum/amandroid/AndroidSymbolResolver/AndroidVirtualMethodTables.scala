package org.sireum.amandroid.AndroidSymbolResolver

import com.google.common.collect.HashMultimap
import org.sireum.util._

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
trait AndroidVirtualMethodTables {
  def recordHierarchyTable : HashMultimap[ResourceUri, ResourceUri]
  def virtualMethodTable : HashMultimap[ResourceUri, ResourceUri]
  protected def recordUriTable : MMap[String, ResourceUri]
  protected def procedureUriTable : MMap[String, ResourceUri]
  protected def procedureTypeTable : MMap[ResourceUri, String]
  def getRecordUri(recordName : String) : ResourceUri
  def getProcedureUriBySignature(sig : String) : ResourceUri
  def getCalleeOptionsByUri(procedureUri : ResourceUri) : java.util.Set[ResourceUri]
  def getCalleeOptionsBySignature(sig : String) : java.util.Set[ResourceUri]
  def isConstructor(sig : String) : Boolean
  def isStaticMethod(sig : String) : Boolean
  def isVirtualMethod(sig : String) : Boolean
}

object AndroidVirtualMethodTables{
    def apply(rht : HashMultimap[ResourceUri, ResourceUri],
              vmt : HashMultimap[ResourceUri, ResourceUri],
              rut : MMap[String, ResourceUri],
              put : MMap[String, ResourceUri],
              ptt : MMap[String, String],
              avmtConstructor : Unit => AndroidVirtualMethodTablesProducer) = {
      collectTables(rht, vmt, rut, put, ptt, avmtConstructor)
    }
    
    def collectTables(rht : HashMultimap[ResourceUri, ResourceUri],
                      vmt : HashMultimap[ResourceUri, ResourceUri],
                      rut : MMap[String, ResourceUri],
                      put : MMap[String, ResourceUri],
                      ptt : MMap[String, String],
                      avmtConstructor : Unit => AndroidVirtualMethodTablesProducer) = {
      val avmt = avmtConstructor()
      avmt.tables.recordHierarchyTable.putAll(rht)
      avmt.tables.virtualMethodTable.putAll(vmt)
      avmt.tables.recordUriTable ++= rut
      avmt.tables.procedureUriTable ++= put
      avmt.tables.procedureTypeTable ++= ptt
      avmt.toAndroidVirtualMethodTables
    }
}

trait AndroidVirtualMethodTablesProducer{
  def tables : AndroidVirtualMethodTablesData

  def toAndroidVirtualMethodTables : AndroidVirtualMethodTables
}

sealed case class AndroidVirtualMethodTablesData
(recordHierarchyTable : HashMultimap[ResourceUri, ResourceUri] = HashMultimap.create(),
 virtualMethodTable : HashMultimap[ResourceUri, ResourceUri] = HashMultimap.create(),
 recordUriTable : MMap[String, ResourceUri] = mmapEmpty[String, ResourceUri],
 procedureUriTable : MMap[String, ResourceUri] = mmapEmpty[String, ResourceUri],
 procedureTypeTable : MMap[ResourceUri, String] = mmapEmpty[ResourceUri, String]
)