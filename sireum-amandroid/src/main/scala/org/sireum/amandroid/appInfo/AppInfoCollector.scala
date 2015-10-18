/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.appInfo

import org.sireum.util._
import org.sireum.amandroid.AndroidConstants
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.alir.ControlFlowGraph
import org.sireum.amandroid.parser.LayoutControl
import org.sireum.amandroid.parser.ARSCFileParser
import org.sireum.amandroid.parser.IntentFilterDataBase
import org.sireum.alir.ReachingDefinitionAnalysis
import org.sireum.amandroid.parser.ManifestParser
import org.sireum.jawa.JawaClass
import org.sireum.jawa.JawaMethod
import org.sireum.amandroid.parser.LayoutFileParser
import scala.util.control.Breaks._
import org.sireum.amandroid.parser.ComponentInfo
import java.io.InputStream
import org.sireum.jawa.util.ResourceRetriever
import org.sireum.amandroid.pilarCodeGenerator.AndroidEnvironmentGenerator
import org.sireum.amandroid.pilarCodeGenerator.AndroidSubstituteClassMap
import org.sireum.amandroid.pilarCodeGenerator.AndroidEntryPointConstants
import java.io.File
import java.net.URI
import org.sireum.jawa.util.IgnoreException
import org.sireum.amandroid.parser.ARSCFileParser_apktool
import java.io.FileInputStream
import java.io.BufferedInputStream
import org.sireum.jawa.util.MyTimer
import org.sireum.jawa.Global
import org.sireum.jawa.Signature
import org.sireum.jawa.ObjectType
import org.sireum.amandroid.Apk
import org.sireum.jawa.Reporter

/**
 * It takes the apkUri and outputDir as input parameters.
 * 
 * adapted from Steven Arzt of the FlowDroid group
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
class AppInfoCollector(global: Global, apk: Apk, outputUri: FileResourceUri, timer: Option[MyTimer]) {  
  private final val TITLE = "AppInfoCollector"
  protected val apkUri: String = apk.nameUri
  protected val uses_permissions: MSet[String] = msetEmpty
  protected val callbackMethods: MMap[JawaClass, Set[JawaMethod]] = mmapEmpty
  protected val componentInfos: MSet[ComponentInfo] = msetEmpty
  protected val layoutControls: MMap[Int, LayoutControl] = mmapEmpty
  protected var appPackageName: String = null
  protected var taintWrapperFile: String = null
  protected val intentFdb: IntentFilterDataBase = new IntentFilterDataBase
  protected var codeLineCounter: Int = 0

  /**
   * Map from record name to it's env method code.
   */
  protected var envProcMap: Map[JawaClass, JawaMethod] = Map()
  def getAppName: String = new File(new URI(apk.nameUri)).getName()
  def getPackageName: String = this.appPackageName
  def getUsesPermissions: ISet[String] = this.uses_permissions.toSet
  def getLayoutControls: IMap[Int, LayoutControl] = this.layoutControls.toMap
  def getCallbackMethodMapping: IMap[JawaClass, Set[JawaMethod]] = this.callbackMethods.toMap
  def getCallbackMethods: ISet[JawaMethod] = if(!this.callbackMethods.isEmpty)this.callbackMethods.map(_._2).reduce(iunion[JawaMethod]) else isetEmpty[JawaMethod]
  def printEnvs() =
    envProcMap.foreach{case(k, v) => println("Environment for " + k + "\n" + v)}

  def printEntrypoints() = {
    if (this.componentInfos == null)
      println("Entry points not initialized")
    else {
      println("Classes containing entry points:")
      for (record <- componentInfos)
        println("\t" + record)
      println("End of Entrypoints")
    }
  }

  def setTaintWrapperFile(taintWrapperFile: String) = {
    this.taintWrapperFile = taintWrapperFile;
  }

  def getIntentDB = this.intentFdb
  def getEntryPoints: ISet[ObjectType] = this.componentInfos.map(_.compType).toSet

  def getComponentInfos = this.componentInfos
  def getEnvMap = this.envProcMap
  def getEnvString: String = {
    val sb = new StringBuilder
    this.envProcMap.foreach{
      case (k, v) =>
        sb.append("*********************** Environment for " + k + " ************************\n")
        sb.append(v + "\n\n")
    }
    sb.toString.intern()
  }

  def hasEnv(rec: JawaClass): Boolean = this.envProcMap.contains(rec)

  /**
   * generates env code for a component like Activity, BroadcastReceiver, etc.
   * @param recordName component name
   * @param codeCtr code line number of the last generated env
   * @return codeCtr + newly generated number of lines
   */
  def generateEnvironment(record: JawaClass, envName: String, codeCtr: Int): Int = {
    if(record == null) return 0
    //generate env main method
    global.reporter.echo(TITLE, "Generate environment for " + record)
    val dmGen = new AndroidEnvironmentGenerator(global)
    dmGen.setSubstituteClassMap(AndroidSubstituteClassMap.getSubstituteClassMap)
    dmGen.setCurrentComponent(record.getType)
    dmGen.setComponentInfos(this.componentInfos.toSet)
    dmGen.setCodeCounter(codeCtr)
    var callbackMethodSigs: Map[ObjectType, Set[Signature]] = Map()
    this.callbackMethods.foreach{
      case (record, procs) =>
        procs.foreach{
          p =>
            callbackMethodSigs += (record.getType -> (callbackMethodSigs.getOrElse(record.getType, isetEmpty) + p.getSignature))
        }
    }
    dmGen.setCallbackFunctions(callbackMethodSigs)
    val proc = dmGen.generateWithParam(List(new ObjectType(AndroidEntryPointConstants.INTENT_NAME)), envName)
    this.envProcMap += (record -> proc)
    dmGen.getCodeCounter
  }

  def dynamicRegisterReceiver(comRec: JawaClass, iDB: IntentFilterDataBase) = {
    this.synchronized{
      if(!comRec.declaresMethodByName(AndroidConstants.COMP_ENV)){
        global.reporter.echo(TITLE, "*************Dynamically Register Component**************")
        global.reporter.echo(TITLE, "Component name: " + comRec)
        this.intentFdb.merge(iDB)
        val analysisHelper = new ReachableInfoCollector(global, Set(comRec.getType), timer) 
        analysisHelper.collectCallbackMethods()
        this.callbackMethods ++= analysisHelper.getCallbackMethods
        analysisHelper.getCallbackMethods.foreach {
          case(k, v) =>
            this.callbackMethods += (k -> (this.callbackMethods.getOrElse(k, isetEmpty) ++ v))
        }
        global.reporter.echo(TITLE, "Found " + this.callbackMethods.size + " callback methods")
        val clCounter = generateEnvironment(comRec, AndroidConstants.COMP_ENV, this.codeLineCounter)
        this.codeLineCounter = clCounter
        apk.addDynamicRegisteredReceiver(comRec)
        apk.updateIntentFilterDB(iDB)
        global.reporter.echo(TITLE, "~~~~~~~~~~~~~~~~~~~~~~~~~Done~~~~~~~~~~~~~~~~~~~~~~~~~~")
      }
    }
  }
  
  /**
   * Get rpc method list for Android component
   * originally designed by Sankardas Roy, modified by Fengguo Wei
   */
  def getRpcMethods(comp: JawaClass): ISet[JawaMethod] = {
    if(apk.getServices.contains(comp)){
      comp.getMethods.filter { 
        method => 
          !(method.isConstructor || AndroidEntryPointConstants.getServiceLifecycleMethods().contains(method.getSubSignature)
              || method.getName == AndroidConstants.MAINCOMP_ENV || method.getName == AndroidConstants.COMP_ENV) 
      }
    } else isetEmpty
  }

  def collectInfo: Unit = {
    val manifestUri = outputUri + "AndroidManifest.xml"
    val mfp = AppInfoCollector.analyzeManifest(global.reporter, manifestUri)
    val afp = AppInfoCollector.analyzeARSC(global.reporter, apkUri)
    val lfp = AppInfoCollector.analyzeLayouts(global, apkUri, mfp)
    val ra = AppInfoCollector.reachabilityAnalysis(global, mfp, timer)
    val callbacks = AppInfoCollector.analyzeCallback(global.reporter, afp, lfp, ra)

    this.appPackageName = mfp.getPackageName
    this.componentInfos ++= mfp.getComponentInfos
    this.uses_permissions ++= mfp.getPermissions
    this.intentFdb.merge(mfp.getIntentDB)
    this.layoutControls ++= lfp.getUserControls
    this.callbackMethods ++= callbacks

    var components = isetEmpty[JawaClass]
    mfp.getComponentInfos.foreach{
      f =>
        if(f.enabled){
          val comp = global.getClassOrResolve(f.compType)
          if(!comp.isUnknown && comp.isApplicationClass){
            components += comp
            val clCounter = generateEnvironment(comp, if(f.exported)AndroidConstants.MAINCOMP_ENV else AndroidConstants.COMP_ENV, codeLineCounter)
            codeLineCounter = clCounter
          }
        }
    }
    components.foreach {
      comp =>
        val rpcs = getRpcMethods(comp)
        apk.addRpcMethods(comp, rpcs)
    }
    apk.setComponents(components)
    apk.updateIntentFilterDB(this.intentFdb)
    apk.setAppInfo(this)
    global.reporter.echo(TITLE, "Entry point calculation done.")
  }
}

object AppInfoCollector {
  final val TITLE = "AppInfoCollector"
  def analyzeManifest(reporter: Reporter, manifestUri: FileResourceUri): ManifestParser = {
    val manifestIS = new FileInputStream(FileUtil.toFile(manifestUri))
    val mfp = new ManifestParser
    mfp.loadClassesFromTextManifest(manifestIS)
    manifestIS.close()
    reporter.echo(TITLE, "entrypoints--->" + mfp.getComponentClasses)
    reporter.echo(TITLE, "packagename--->" + mfp.getPackageName)
    reporter.echo(TITLE, "permissions--->" + mfp.getPermissions)
    reporter.echo(TITLE, "intentDB------>" + mfp.getIntentDB)
    mfp
  }

  def analyzeARSC(reporter: Reporter, apkUri: FileResourceUri): ARSCFileParser_apktool = {
    // Parse the resource file
    val afp = new ARSCFileParser_apktool()
    afp.parse(apkUri)
    reporter.echo(TITLE, "arscstring-->" + afp.getGlobalStringPool)
    reporter.echo(TITLE, "arscpackage-->" + afp.getPackages)
    afp
  }

  def analyzeLayouts(global: Global, apkUri: FileResourceUri, mfp: ManifestParser): LayoutFileParser = {
    // Find the user-defined sources in the layout XML files
    val lfp = new LayoutFileParser(global)
    lfp.setPackageName(mfp.getPackageName)
    lfp.parseLayoutFile(apkUri)
    global.reporter.echo(TITLE, "layoutcallback--->" + lfp.getCallbackMethods)
    global.reporter.echo(TITLE, "layoutuser--->" + lfp.getUserControls)
    lfp
  }

  def analyzeCallback(reporter: Reporter, afp: ARSCFileParser_apktool, lfp: LayoutFileParser, analysisHelper: ReachableInfoCollector): Map[JawaClass, Set[JawaMethod]] = {
    val callbackMethods: MMap[JawaClass, Set[JawaMethod]] = mmapEmpty
    analysisHelper.collectCallbackMethods()
    callbackMethods ++= analysisHelper.getCallbackMethods
    reporter.echo(TITLE, "LayoutClasses --> " + analysisHelper.getLayoutClasses)
  
    analysisHelper.getCallbackMethods.foreach {
      case(k, v) =>
        callbackMethods += (k -> (callbackMethods.getOrElse(k, isetEmpty) ++ v))
    }
    // Collect the XML-based callback methods
    analysisHelper.getLayoutClasses.foreach {
      case (k, v) =>
        v.foreach {
          i =>
            val resource = afp.findResource(i)
            if(resource != null && resource.getType.getName == "layout"){
              val includes = lfp.getIncludes.filter(_._1.contains(resource.getName)).flatten(_._2).toSet
              val resources = includes.map(i => afp.findResource(i)) + resource
              lfp.getCallbackMethods.find{case (file, _) => resources.map(_.getName).exists { x => file.contains(x) }}.foreach{
                case (_, methodNames) =>
                  for(methodName <- methodNames) {
                    //The callback may be declared directly in the class or in one of the superclasses
                    var callbackClass = k
                    val callbackMethod: MSet[JawaMethod] = msetEmpty
                    breakable{ 
                      while(callbackMethod.isEmpty){
                        if(callbackClass.declaresMethodByName(methodName))
                          callbackMethod ++= callbackClass.getMethodsByName(methodName)
                        if(callbackClass.hasSuperClass)
                          callbackClass = callbackClass.getSuperClass.get
                        else break
                      }
                    }
                    if(callbackMethod != null){
                      callbackMethods += (k -> (callbackMethods.getOrElse(k, isetEmpty) ++ callbackMethod))
                    } else {
                      reporter.echo(TITLE, "Callback method " + methodName + " not found in class " + k);
                    }
                  }
              }
          } else {
            reporter.echo(TITLE, "Unexpected resource type for layout class: " + resource.getType)
          }
      }
    }
    callbackMethods.toMap
  }

  def reachabilityAnalysis(global: Global, mfp: ManifestParser, timer: Option[MyTimer]): ReachableInfoCollector = {
    // Collect the callback interfaces implemented in the app's source code
    val analysisHelper = new ReachableInfoCollector(global, mfp.getComponentInfos.map(_.compType), timer) 
    analysisHelper.init
    //  this.sensitiveLayoutContainers = analysisHelper.getSensitiveLayoutContainer(this.layoutControls)
    analysisHelper
  }
}