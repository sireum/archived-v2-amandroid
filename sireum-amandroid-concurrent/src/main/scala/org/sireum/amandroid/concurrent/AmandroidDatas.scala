package org.sireum.amandroid.concurrent

import org.sireum.util._
import org.sireum.jawa.JawaType

// DecompileActor's input
case class DecompileData(fileUri: FileResourceUri, outputUri: FileResourceUri, dpsuri: Option[FileResourceUri], removeSupportGen: Boolean, forceDelete: Boolean)
// DecompileActor internal datas
case class DexDecodeData(index: Int, dexUri: FileResourceUri, outUri: FileResourceUri, dpsuri: Option[FileResourceUri], pkg: String, removeSupportGen: Boolean, forceDelete: Boolean)
case class DecodeOutputData(index: Int, dexUri: FileResourceUri, recType: JawaType, code: String, outputUri: FileResourceUri)
case class DecodeInfoData(index: Int, dexUri: FileResourceUri, src: String, dependent: ISet[String])
case class DecodeOutputResult(index: Int, dexUri: FileResourceUri)
case class DexDecodeResult(index: Int, dexUri: FileResourceUri, recordCount: Int)
// DecompileActor's result
case class DecompilerResult(fileUri: FileResourceUri, outUri: FileResourceUri, srcFolders: ISet[String], dependencies: ISet[String])