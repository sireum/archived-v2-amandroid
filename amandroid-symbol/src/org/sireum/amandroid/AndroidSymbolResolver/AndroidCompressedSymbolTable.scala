package org.sireum.amandroid.AndroidSymbolResolver

import org.sireum.pilar.symbol._
import org.sireum.util._
import org.sireum.pilar.ast._

final case class AndroidCompressedSymbolTable (
  var stData : AndroidSymbolTableData = AndroidSymbolTableData(),
  var pstsData : MMap[ResourceUri, ProcedureSymbolTableData] = mmapEmpty
)

//object AndroidCompressedSymbolTable {
//  def apply(std : AndroidSymbolTableData,
//            pstsd : MMap[ResourceUri, ProcedureSymbolTableData])
//            : AndroidCompressedSymbolTable ={
//    buildAndroidCompressedSymbolTable(std, pstsd)
//  }
//  
//  def buildAndroidCompressedSymbolTable(std : AndroidSymbolTableData,
//            pstsd : MMap[ResourceUri, ProcedureSymbolTableData])
//            : AndroidCompressedSymbolTable ={
//    stData
//  }
//}

final case class CompressedProcedureDecl(
  name : NameDefinition,
  annotations : ISeq[Annotation],
  typeVars : ISeq[(NameDefinition, ISeq[Annotation])],
  params : ISeq[ParamDecl],
  returnType : Option[TypeSpec],
  varArity : Boolean
)

sealed case class AndroidCompressedSymbolTableData //
(declaredSymbols : MMap[FileResourceUri, MSet[ResourceUri]] = mmapEmpty,
// constTable : MMap[ResourceUri, MBuffer[ConstDecl]] = mmapEmpty,
// constElementTable : MMap[ResourceUri, ConstElement] = mmapEmpty,
// enumTable : MMap[ResourceUri, MBuffer[EnumDecl]] = mmapEmpty,
// enumElementTable : MMap[ResourceUri, EnumElement] = mmapEmpty,
// extensionTable : MMap[ResourceUri, MBuffer[ExtensionDecl]] = mmapEmpty,
// extensionElementTable : MMap[ResourceUri, ExtElement] = mmapEmpty,
// funTable : MMap[ResourceUri, FunDecl] = mmapEmpty,
 globalVarTable : MMap[ResourceUri, GlobalVarDecl] = mmapEmpty,
 procedureTable : MMap[ResourceUri, MBuffer[ResourceUri]] = mmapEmpty,
 procedureAbsTable : MMap[ResourceUri, ProcedureDecl] = mmapEmpty,
 recordTable : MMap[ResourceUri, RecordDecl] = mmapEmpty,
// attributeTable : MMap[ResourceUri, AttributeDecl] = mmapEmpty,
// typeVarTable : MMap[ResourceUri, NameDefinition] = mmapEmpty,
// typeAliasTable : MMap[ResourceUri, TypeAliasDecl] = mmapEmpty,
// vsetTable : MMap[ResourceUri, VSetDecl] = mmapEmpty,
 dependency : DependencyMap = mmapEmpty)