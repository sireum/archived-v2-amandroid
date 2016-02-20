package org.sireum.amandroid.concurrent

import org.sireum.util._
import org.sireum.jawa.JawaType

trait AmandroidData

// DecompileActor's input
case class DecompileData(fileUri: FileResourceUri, outputUri: FileResourceUri, dpsuri: Option[FileResourceUri], removeSupportGen: Boolean, forceDelete: Boolean) extends AmandroidData
case class DecompileTimeout(fileUri: FileResourceUri, outputUri: FileResourceUri) extends AmandroidData
// DecompileActor's result
case class DecompilerResult(fileUri: FileResourceUri, outUri: FileResourceUri, srcFolders: ISet[String], dependencies: ISet[String]) extends AmandroidData