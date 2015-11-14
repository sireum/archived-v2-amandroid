/*
Copyright (c) 2015-2016 Fengguo Wei, University of South Florida.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
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
  
  private val componentToApkMap: MMap[JawaClass, Apk] = mmapEmpty
  def addComponent(component: JawaClass, apk: Apk) = this.synchronized(componentToApkMap(component) = apk)
  def removeComponent(component: JawaClass) = this.synchronized(componentToApkMap -= component)
  def getOwnerApk(component: JawaClass): Option[Apk] = this.synchronized(componentToApkMap.get(component))
  def getComponentToApkMap = this.componentToApkMap.toMap
  
  def loadApk(nameUri: FileResourceUri, outputUri: FileResourceUri, dpsuri: Option[FileResourceUri], dexLog: Boolean, debugMode: Boolean, refactor: Boolean, forceDelete: Boolean = true): Apk = {
    val apk = new Apk(nameUri)
    val apkFile = FileUtil.toFile(nameUri)
    val name = try{apkFile.getName.substring(0, apkFile.getName().lastIndexOf(".apk"))} catch {case e: Exception => apkFile.getName}
    val resultDir = FileUtil.toFile(outputUri + "/" + name)
    val (outUri, _) = ApkDecompiler.decompile(apkFile, resultDir, dpsuri, global.reporter, dexLog, debugMode, true, refactor, forceDelete)
    // convert the dex file to the "pilar" form
    val fileUri = outUri + "/src"
    if(FileUtil.toFile(fileUri).exists()) {
      //store the app's pilar code in AmandroidCodeSource which is organized class by class.
      global.load(fileUri, Constants.PILAR_FILE_EXT, AndroidLibraryAPISummary)
    }
    val app_info = new AppInfoCollector(global, apk, outUri, None)
    app_info.collectInfo
    addApk(apk)
    apk.getComponents.foreach(addComponent(_, apk))
    apk
  }
  
  private val idfgResults: MMap[JawaClass, InterProceduralDataFlowGraph] = mmapEmpty
  
  def addIDFG(key: JawaClass, idfg: InterProceduralDataFlowGraph) = this.synchronized(this.idfgResults += (key -> idfg))
  def hasIDFG(key: JawaClass): Boolean = this.synchronized(this.idfgResults.contains(key))
  def getIDFG(key: JawaClass): Option[InterProceduralDataFlowGraph] = this.synchronized(this.idfgResults.get(key))
  def getIDFGs = this.idfgResults.toMap
  
  private val iddaResults: MMap[JawaClass, InterproceduralDataDependenceInfo] = mmapEmpty
  
  def addIDDG(key: JawaClass, iddi: InterproceduralDataDependenceInfo) = this.synchronized(this.iddaResults += (key -> iddi))
  def hasIDDG(key: JawaClass): Boolean = this.iddaResults.contains(key)
  def getIDDG(key: JawaClass): Option[InterproceduralDataDependenceInfo] = this.synchronized(this.iddaResults.get(key))
  def getIDDGs = this.iddaResults.toMap
  
  private val summaryTables: MMap[JawaClass, ComponentSummaryTable] = mmapEmpty
  
  def addSummaryTable(key: JawaClass, summary: ComponentSummaryTable) = this.synchronized(this.summaryTables += (key -> summary))
  def hasSummaryTable(key: JawaClass): Boolean = this.summaryTables.contains(key)
  def getSummaryTable(key: JawaClass): Option[ComponentSummaryTable] = this.synchronized(this.summaryTables.get(key))
  def getSummaryTables = this.summaryTables.toMap
  
  private var taintResult: Option[TaintAnalysisResult[InterProceduralNode, AlirEdge[InterProceduralNode]]] = None
  
  def setTaintAnalysisResult(tar: TaintAnalysisResult[InterProceduralNode, AlirEdge[InterProceduralNode]]) = this.synchronized(this.taintResult = Option(tar))
  def hasTaintAnalysisResult: Boolean = taintResult.isDefined
  def getTaintAnalysisResult: Option[TaintAnalysisResult[InterProceduralNode, AlirEdge[InterProceduralNode]]] = this.taintResult
  
}