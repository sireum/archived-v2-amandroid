package org.sireum.amandroid.AndroidSymbolResolver

import com.google.common.collect.HashMultimap

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
trait AndroidVirtualMethodTables {
  def recordHierarchyTable : HashMultimap[String, String]
  def virtualMethodTable : HashMultimap[String, String]
}

object AndroidVirtualMethodTables{
    def apply(rht : HashMultimap[String, String],
              vmt : HashMultimap[String, String],
              avmtConstructor : Unit => AndroidVirtualMethodTablesProducer) = {
      collectTables(rht, vmt, avmtConstructor)
    }
    
    def collectTables(rht : HashMultimap[String, String],
                      vmt : HashMultimap[String, String],
                      avmtConstructor : Unit => AndroidVirtualMethodTablesProducer) = {
      val avmt = avmtConstructor()
      avmt.tables.recordHierarchyTable.putAll(rht)
      avmt.tables.virtualMethodTable.putAll(vmt)
      avmt.toAndroidVirtualMethodTables
    }
}

trait AndroidVirtualMethodTablesProducer{
  def tables : AndroidVirtualMethodTablesData

  def toAndroidVirtualMethodTables : AndroidVirtualMethodTables
}

sealed case class AndroidVirtualMethodTablesData
(recordHierarchyTable : HashMultimap[String, String] = HashMultimap.create(),
 virtualMethodTable : HashMultimap[String, String] = HashMultimap.create())