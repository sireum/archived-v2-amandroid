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
package org.sireum.amandroid.alir.componentSummary

import org.sireum.util._
import org.sireum.jawa.JawaClass
import org.sireum.jawa.alir.dataFlowAnalysis.InterProceduralDataFlowGraph
import org.sireum.jawa.alir.dataDependenceAnalysis.InterproceduralDataDependenceInfo
import org.sireum.jawa.alir.taintAnalysis.TaintAnalysisResult
import org.sireum.amandroid.Apk
import org.sireum.amandroid.decompile.ApkDecompiler
import org.sireum.jawa.Global
import org.sireum.amandroid.util.AndroidLibraryAPISummary
import org.sireum.jawa.Constants
import org.sireum.amandroid.appInfo.AppInfoCollector
import org.sireum.jawa.alir.interProcedural.InterProceduralNode
import org.sireum.alir.AlirEdge
import java.io.File
import org.sireum.jawa.JawaType

/**
 * @author fgwei
 */
class ApkYard(global: Global) {
  private val apks: MMap[FileResourceUri, Apk] = mmapEmpty
  def addApk(apk: Apk) = this.synchronized(apks(apk.nameUri) = apk)
  def removeApk(apk: Apk) = this.synchronized(apks -= apk.nameUri)
  def removeApk(nameUri: FileResourceUri) = this.synchronized(apks -= nameUri)
  def getApk(nameUri: FileResourceUri): Option[Apk] = this.synchronized(apks.get(nameUri))
  def getApks = this.apks.toMap
  
  private val componentToApkMap: MMap[JawaType, Apk] = mmapEmpty
  def addComponent(component: JawaType, apk: Apk) = this.synchronized(componentToApkMap(component) = apk)
  def removeComponent(component: JawaType) = this.synchronized(componentToApkMap -= component)
  def getOwnerApk(component: JawaType): Option[Apk] = this.synchronized(componentToApkMap.get(component))
  def getComponentToApkMap = this.componentToApkMap.toMap
  
  def loadApk(nameUri: FileResourceUri, outputUri: FileResourceUri, dpsuri: Option[FileResourceUri], dexLog: Boolean, debugMode: Boolean, forceDelete: Boolean = true): Apk = {
    val apk = new Apk(nameUri)
    val apkFile = FileUtil.toFile(nameUri)
    val resultDir = FileUtil.toFile(outputUri)
    val (outUri, srcs, _) = ApkDecompiler.decompile(apkFile, resultDir, dpsuri, dexLog, debugMode, true, forceDelete)
    // convert the dex file to the "pilar" form
    srcs foreach {
      src =>
        val fileUri = FileUtil.toUri(FileUtil.toFilePath(outUri) + File.separator + src)
        if(FileUtil.toFile(fileUri).exists()) {
          //store the app's pilar code in AmandroidCodeSource which is organized class by class.
          global.load(fileUri, Constants.PILAR_FILE_EXT, AndroidLibraryAPISummary)
        }
    }
    AppInfoCollector.collectInfo(apk, global, outUri)
    addApk(apk)
    apk.getComponents.foreach(addComponent(_, apk))
    apk
  }
  
  private val idfgResults: MMap[JawaType, InterProceduralDataFlowGraph] = mmapEmpty
  
  def addIDFG(key: JawaType, idfg: InterProceduralDataFlowGraph) = this.synchronized(this.idfgResults += (key -> idfg))
  def hasIDFG(key: JawaType): Boolean = this.synchronized(this.idfgResults.contains(key))
  def getIDFG(key: JawaType): Option[InterProceduralDataFlowGraph] = this.synchronized(this.idfgResults.get(key))
  def getIDFGs = this.idfgResults.toMap
  
  private val iddaResults: MMap[JawaType, InterproceduralDataDependenceInfo] = mmapEmpty
  
  def addIDDG(key: JawaType, iddi: InterproceduralDataDependenceInfo) = this.synchronized(this.iddaResults += (key -> iddi))
  def hasIDDG(key: JawaType): Boolean = this.iddaResults.contains(key)
  def getIDDG(key: JawaType): Option[InterproceduralDataDependenceInfo] = this.synchronized(this.iddaResults.get(key))
  def getIDDGs = this.iddaResults.toMap
  
  private val summaryTables: MMap[JawaType, ComponentSummaryTable] = mmapEmpty
  
  def addSummaryTable(key: JawaType, summary: ComponentSummaryTable) = this.synchronized(this.summaryTables += (key -> summary))
  def hasSummaryTable(key: JawaType): Boolean = this.summaryTables.contains(key)
  def getSummaryTable(key: JawaType): Option[ComponentSummaryTable] = this.synchronized(this.summaryTables.get(key))
  def getSummaryTables = this.summaryTables.toMap
  
  private var apkTaintResult: MMap[FileResourceUri, Any] = mmapEmpty
  
  def addTaintAnalysisResult[N <: InterProceduralNode, E <: AlirEdge[N]](fileUri: FileResourceUri, tar: TaintAnalysisResult[N, E]) = this.synchronized(this.apkTaintResult(fileUri) = tar)
  def hasTaintAnalysisResult(fileUri: FileResourceUri): Boolean = this.apkTaintResult.contains(fileUri)
  def getTaintAnalysisResult[N <: InterProceduralNode, E <: AlirEdge[N]](fileUri: FileResourceUri) = this.apkTaintResult.get(fileUri).map(_.asInstanceOf[TaintAnalysisResult[N, E]])
  
  private var interAppTaintResult: Option[Any] = None
  def setInterAppTaintAnalysisResult[N <: InterProceduralNode, E <: AlirEdge[N]](tar: TaintAnalysisResult[N, E]) = this.synchronized(this.interAppTaintResult = Option(tar))
  def hasInterAppTaintAnalysisResult: Boolean = interAppTaintResult.isDefined
  def getInterAppTaintAnalysisResult[N <: InterProceduralNode, E <: AlirEdge[N]] = this.interAppTaintResult.map(_.asInstanceOf[TaintAnalysisResult[N, E]])
  
  def reset = {
    this.apks.clear()
    this.componentToApkMap.clear()
    this.idfgResults.clear()
    this.iddaResults.clear()
    this.summaryTables.clear()
    this.apkTaintResult.clear()
    this.interAppTaintResult = None
  }
}
