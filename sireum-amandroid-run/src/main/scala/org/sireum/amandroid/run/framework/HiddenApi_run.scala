/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
package org.sireum.amandroid.run.framework

import org.sireum.util._
import org.sireum.jawa.Signature
import org.sireum.jawa.PrintReporter
import org.sireum.jawa.MsgLevel
import org.sireum.jawa.Global
import org.sireum.amandroid.AndroidGlobalConfig
import org.sireum.amandroid.decompile.ApkDecompiler
import java.io.File
import org.sireum.jawa.Constants
import org.sireum.jawa.DefaultLibraryAPISummary
import org.sireum.jawa.Reporter
import org.sireum.jawa.JawaType
import org.sireum.amandroid.util.PScoutTranslator
import org.sireum.jawa.alir.pta.suspark.InterproceduralSuperSpark
import java.io.PrintWriter
import org.sireum.jawa.util.MyFileUtil

/**
 * @author fgwei
 */
object HiddenApi_run {
  def main(args: Array[String]): Unit = {
    val fullPath = args(0)
    val outPath = args(1)
//    val pscoutFilePath = args(2)
    val fullUri = FileUtil.toUri(fullPath)
    val outUri = FileUtil.toUri(outPath)
//    val pscoutFileUri = FileUtil.toUri(pscoutFilePath)
    val reporter = new PrintReporter(MsgLevel.ERROR)
    val fullGlobal = new Global("AndroidFullFramework", reporter)
    val dexUris = FileUtil.listFiles(fullUri, ".dex", true)
    dexUris foreach {
      dexUri =>
        val (src, _) = ApkDecompiler.decompileDex(dexUri, outUri, None, "", false, false, false, false)
        val fileUri = FileUtil.toUri(FileUtil.toFilePath(outUri) + File.separator + src)
        if(FileUtil.toFile(fileUri).exists()) {
          //store the app's pilar code in AmandroidCodeSource which is organized class by class.
          fullGlobal.load(fileUri, Constants.PILAR_FILE_EXT, DefaultLibraryAPISummary)
        }
    }
    println("Total framework classes: " + fullGlobal.getApplicationClassCodes.size)
    var i = 0
    fullGlobal.getApplicationClassCodes.foreach {
      case (typ, file) =>
        i += 1
        if(i % 1000 == 0) println(i)
        fullGlobal.getClassOrResolve(typ)
    }
    val hiddenapis = getHiddenApiList(fullGlobal)
    val nonPublicMethods = fullGlobal.getClasses.filter(c => c.getName.startsWith("android.") || c.getName.startsWith("com.android.")).map(_.getDeclaredMethods.filter(m => !m.isPublic && !m.isAbstract && !m.isConstructor).map(_.getSignature)).fold(isetEmpty)(_ ++ _)
    writeApiToFile(hiddenapis, outUri, "HideApis.txt")
    writeApiToFile(nonPublicMethods, outUri, "NonPublicMethods.txt")
    println("Total Non-Public Methods: " + nonPublicMethods.size)
    val systemServices = getSystemServiceList(fullGlobal)
//    systemServices.foreach{println(_)}
    val systemServiceHiddenApis = getSystemServiceMethods(fullGlobal, systemServices, hiddenapis)
    val systemServiceNonPublicMethods = getSystemServiceMethods(fullGlobal, systemServices, nonPublicMethods)
    fullGlobal.reset(false)
    println("Total system services: " + systemServices.size)
    println("System service hidden apis: " + systemServiceHiddenApis.size)
    println("System service Non-Public Methods: " + systemServiceNonPublicMethods.size)
//    val permissionMapping = PScoutTranslator.translate(pscoutFileUri)
//    println("Analyze systemServiceHiddenApis:")
//    methodsAnalysis(fullGlobal, systemServiceHiddenApis)
//    println("Analyze systemServiceNonPublicMethods:")
//    methodsAnalysis(fullGlobal, systemServiceNonPublicMethods)
  }
  
  def writeApiToFile(apis: ISet[Signature], outputUri: FileResourceUri, fileName: String) = {
    val apiFile = FileUtil.toFile(MyFileUtil.appendFileName(outputUri, fileName))
    val out = new PrintWriter(apiFile)
    apis.toList.sortBy(_.signature).foreach {
      api =>
        out.println(api.signature)
    }
    out.flush()
    out.close()
  }
  
  def methodsAnalysis(global: Global, sigs: ISet[Signature]) = {
    sigs foreach {
      sig =>
        try{
          val reachableSigs = reachabilityAnalysis(global, Set(sig))
          val rNative = reachableNative(global, reachableSigs)
          if(!rNative.isEmpty) {
            println(sig + " reaches native methods: ")
            rNative.foreach{println(_)}
            if(!reachCheckPermission(global, reachableSigs)) {
              System.err.println("Not protected by permission check!")
            }
          }
        } catch {
          case e: Exception =>
            System.err.println("Error in " + sig + " " + e.getMessage)
        } finally {
          global.reset(false)
        }
    }
  }
  
  def reachableNative(global: Global, sigs: ISet[Signature]): ISet[Signature] = {
    sigs.filter {
      sig =>
        val clazz = global.getClassOrResolve(sig.classTyp)
        clazz.getMethod(sig.getSubSignature) match {
          case Some(m) => 
            m.isNative &&
            m.getDeclaringClass.getName != "android.util.Log" &&
            m.getDeclaringClass.getName != "android.os.SystemClock" &&
            m.getDeclaringClass.getName != "android.os.Binder"
          case None => false
        }
    }
  }
  
  def reachCheckPermission(global: Global, sigs: ISet[Signature]): Boolean = {
    sigs.exists {
      sig =>
        sig.methodName == "checkCallingOrSelfPermission" ||
        sig.methodName == "checkCallingPermission" ||
        sig.methodName == "checkCallingUriPermission" ||
        sig.methodName == "checkPermission" ||
        sig.methodName == "checkUriPermission" ||
        sig.methodName == "enforceCallingPermission" ||
        sig.methodName == "enforceCallingOrSelfPermission" ||
        sig.methodName == "enforceCallingOrSelfUriPermission" ||
        sig.methodName == "enforceCallingUriPermission" ||
        sig.methodName == "enforcePermission" ||
        sig.methodName == "enforceUriPermission"
    }
  }
  
  def reachabilityAnalysis(global: Global, sigs: ISet[Signature]): ISet[Signature] = {
    val idfg = InterproceduralSuperSpark(global, sigs)
    idfg.icfg.getCallGraph.getReachableMethods(sigs)
  }
  
  def getHiddenApiList(fullGlobal: Global): ISet[Signature] = {
    val result: MSet[Signature] = msetEmpty
    val total: MSet[Signature] = msetEmpty
    val sdkGlobal = new Global("AndroidSdk", fullGlobal.reporter)
    sdkGlobal.setJavaLib(AndroidGlobalConfig.lib_files)
    fullGlobal.getApplicationClassCodes.foreach {
      case (typ, file) =>
        if(typ.name.startsWith("android.") || typ.name.startsWith("com.android.")) {
          val fullclazz = fullGlobal.getClassOrResolve(typ)
          total ++= fullclazz.getDeclaredMethods.filter(m => m.isPublic && !m.isAbstract && !m.isConstructor).map(_.getSignature)
          sdkGlobal.getMyClass(typ) match {
            case Some(mc) =>
              val sdkmethods = mc.methods.map(_.signature).toSet
              val fullmethods = fullclazz.getDeclaredMethods.filter(m => m.isPublic && !m.isAbstract && !m.isConstructor).map(_.getSignature)
              result ++= (fullmethods -- sdkmethods)
            case None =>
              result ++= fullclazz.getDeclaredMethods.filter(m => m.isPublic && !m.isAbstract && !m.isConstructor).map(_.getSignature)
          }
        }
    }
    println("Total public methods: " + total.size)
    println("Hidden Api methods: " + result.size)
    sdkGlobal.reset()
    result.toSet
  }
  
  def getSystemServiceList(fullGlobal: Global): ISet[JawaType] = {
    val result: MSet[JawaType] = msetEmpty
    println("fullGlobal.getClasses:" + fullGlobal.getClasses.size)
    fullGlobal.getClasses.foreach {
      c =>
        if(c.getName.endsWith("$Stub")) {
          fullGlobal.getClassHierarchy.getAllSubClassesOf(c) foreach {
            sc =>
              if(!sc.getOuterClass.isDefined)
                result += sc.typ
          }
        }
//        if(c.isChildOf(new JawaType("android.os.Binder"))) {
//          fullGlobal.getClassHierarchy.getAllSubClassesOf(c).foreach {
//            sc =>
//              var temp = sc
//              if(!temp.getOuterClass.isDefined)
//                result += sc.typ
//              while(temp.getOuterClass.isDefined) {
//                result += temp.getOuterClass.get.typ
//                temp = temp.getOuterClass.get
//              }
//          }
//        }
    }
    result.toSet
  }
  
  def getSystemServiceMethods(global: Global, systemServices: ISet[JawaType], hiddenApis: ISet[Signature]): ISet[Signature] = {
    systemServices.map{
      ss =>
        val ssclass = global.getClassOrResolve(ss)
        ssclass.getDeclaredMethods.map(_.getSignature).filter(hiddenApis.contains(_))
    }.fold(isetEmpty)(_ ++ _)
  }
}
