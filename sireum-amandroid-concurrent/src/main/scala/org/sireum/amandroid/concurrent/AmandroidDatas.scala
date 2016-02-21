package org.sireum.amandroid.concurrent

import org.sireum.util._
import org.sireum.jawa.JawaType
import scala.concurrent.duration.Duration

trait AmandroidData

// DecompileActor's input
case class DecompileData(fileUri: FileResourceUri, outputUri: FileResourceUri, dpsuri: Option[FileResourceUri], removeSupportGen: Boolean, forceDelete: Boolean, timeout: Duration) extends AmandroidData
// DecompileActor's result
trait DecompilerResult extends AmandroidData
case class DecompileSuccResult(fileUri: FileResourceUri, outUri: FileResourceUri, srcFolders: ISet[String], dependencies: ISet[String]) extends DecompilerResult
case class DecompileFailResult(fileUri: FileResourceUri, e: Option[Exception]) extends DecompilerResult

// ApkInfoCollectActor's input
case class ApkInfoCollectData(fileUri: FileResourceUri, outUri: FileResourceUri, srcFolders: ISet[String]) extends DecompilerResult