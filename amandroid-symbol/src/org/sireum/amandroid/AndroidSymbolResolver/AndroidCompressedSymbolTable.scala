package org.sireum.amandroid.AndroidSymbolResolver

import org.sireum.pilar.symbol._
import org.sireum.util._
import org.sireum.pilar.ast._

final case class AndroidCompressedSymbolTable (
  var stData : AndroidSymbolTableData = AndroidSymbolTableData()
//  var pstsData : MMap[ResourceUri, ProcedureSymbolTableData] = mmapEmpty
)
