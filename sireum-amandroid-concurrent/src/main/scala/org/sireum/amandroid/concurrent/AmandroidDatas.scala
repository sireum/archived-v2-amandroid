/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Main Contributors:
 *    Fengguo Wei - Argus Lab @ University of South Florida
 *    Sankardas Roy - Bowling Green State University
 *    
 * Contributors:
 *    Robby - Santos Lab @ Kansas State University
 *    Wu Zhou - Fireeye
 *    Fengchi Lin - Chinese People's Public Security University
 ******************************************************************************/
package org.sireum.amandroid.concurrent

import org.sireum.util._
import org.sireum.jawa.JawaType
import scala.concurrent.duration.Duration
import org.sireum.amandroid.Apk
import org.sireum.jawa.Signature
import org.sireum.jawa.alir.pta.PTAResult

trait AmandroidData

// AmandroidSupervisorActor's input
case class AnalysisSpec(fileUri: FileResourceUri, outputUri: FileResourceUri, dpsuri: Option[FileResourceUri], removeSupportGen: Boolean, forceDelete: Boolean) extends AmandroidData

// DecompileActor's input
case class DecompileData(fileUri: FileResourceUri, outputUri: FileResourceUri, dpsuri: Option[FileResourceUri], removeSupportGen: Boolean, forceDelete: Boolean, timeout: Duration) extends AmandroidData
// DecompileActor's result
trait DecompilerResult extends AmandroidData {
  def fileUri: FileResourceUri
}
case class DecompileSuccResult(fileUri: FileResourceUri, outApkUri: FileResourceUri, srcFolders: ISet[String], dependencies: ISet[String]) extends DecompilerResult
case class DecompileFailResult(fileUri: FileResourceUri, e: Exception) extends DecompilerResult

// ApkInfoCollectActor's input
case class ApkInfoCollectData(fileUri: FileResourceUri, outApkUri: FileResourceUri, srcFolders: ISet[String], timeout: Duration) extends DecompilerResult
// ApkInfoCollectActor's result
trait ApkInfoCollectResult extends AmandroidData {
  def fileUri: FileResourceUri
}
case class ApkInfoCollectSuccResult(apk: Apk, outApkUri: FileResourceUri, srcFolders: ISet[String]) extends ApkInfoCollectResult {
  def fileUri: FileResourceUri = apk.nameUri
}
case class ApkInfoCollectFailResult(fileUri: FileResourceUri, e: Exception) extends ApkInfoCollectResult

// PointsToAnalysisActor's input
case class PointsToAnalysisData(apk: Apk, outApkUri: FileResourceUri, srcFolders: ISet[String], algos: PTAAlgorithms.Value, stage: Boolean) extends AmandroidData
// RFAActor's input
case class RFAData(apk: Apk, outApkUri: FileResourceUri, srcFolders: ISet[String], stage: Boolean, ep: Signature, timeout: Duration)
// RFAActor's result
trait RFAResult extends AmandroidData {
  def fileUri: FileResourceUri
}
case class RFASuccResult(apk: Apk, ptaresult: PTAResult) extends RFAResult {
  def fileUri: FileResourceUri = apk.nameUri
}
case class RFASuccStageResult(fileUri: FileResourceUri, outApkUri: FileResourceUri) extends RFAResult
case class RFAFailResult(fileUri: FileResourceUri, e: Exception) extends RFAResult