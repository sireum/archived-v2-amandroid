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
package org.sireum.amandroid.run.test

import org.sireum.util._
import org.sireum.jawa.PrintReporter
import org.sireum.jawa.MsgLevel
import org.sireum.jawa.Global
import org.sireum.amandroid.Apk
import org.sireum.amandroid.security.AmandroidSocket
import org.sireum.amandroid.util.AndroidLibraryAPISummary
import org.sireum.amandroid.AndroidGlobalConfig

/**
 * @author fgwei
 */
object ProposalNeeds_run {
  final val TITLE = "ProposalNeeds_run"
  
  object ProposalNeedsCounter {
    var total = 0
    var haveresult = 0
    var haveDynamicLoading = 0
    var haveReflection = 0
    var haveNativeCode = 0
    
    override def toString: String = "total: " + total + ", haveResult: " + haveresult + ", haveDynamicLoading: " + haveDynamicLoading + ", haveReflection: " + haveReflection + ", haveNativeCode: " + haveNativeCode
  }
  
  def main(args: Array[String]): Unit = {
    if(args.size < 2){
      System.err.print("Usage: source_path output_path [dependence_path]")
      return
    }
    val sourcePath = args(0)
    val outputPath = args(1)
    val dpsuri = try{Some(FileUtil.toUri(args(2)))} catch {case e: Exception => None}
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), "", true).filter(Apk.isValidApk(_)).toSet
    files.foreach{
      file =>
        println("Processing " + file)
        ProposalNeedsCounter.total += 1
        val reporter = new PrintReporter(MsgLevel.ERROR)
        val global = new Global(file, reporter)
        global.setJavaLib(AndroidGlobalConfig.lib_files)
        val apk = new Apk(file)
        val socket = new AmandroidSocket(global, apk)
        try{
          val outUri = socket.loadApk(outputPath, AndroidLibraryAPISummary, dpsuri, false, false, true) 
          var dynamicLoading = false
          var reflection = false
          var nativecode = false
          val code = global.getApplicationClassCodes.filter{
            case (typ, c) =>
              val res = 
              !typ.name.startsWith("com.amazon.") &&
              !typ.name.startsWith("com.google.") &&
              !typ.name.startsWith("com.inmobi.") &&
              !typ.name.startsWith("com.millennialmedia.") &&
              !typ.name.startsWith("org.apache.") &&
              !typ.name.startsWith("com.adobe.") &&
              !typ.name.startsWith("com.android.") &&
              !typ.name.startsWith("com.facebook.") &&
              !typ.name.startsWith("com.flurry.") &&
              !typ.name.startsWith("org.slf4j.") &&
              !typ.name.startsWith("com.admob.") &&
              !typ.name.startsWith("com.baidu.") &&
              !typ.name.startsWith("com.renren.") &&
              !typ.name.startsWith("com.mobfox.") &&
              !typ.name.startsWith("com.applovin.") &&
              !typ.name.startsWith("com.tapjoy.") &&
              !typ.name.startsWith("com.adwhirl.") &&
              !typ.name.startsWith("com.badlogic.") &&
              !typ.name.startsWith("org.json.") &&
              !typ.name.startsWith("com.mongodb.") &&
              !typ.name.startsWith("com.appbrain.") &&
              !typ.name.startsWith("com.dropbox.") &&
              !typ.name.startsWith("com.github.") &&
              !typ.name.startsWith("com.twitter.") &&
              !typ.name.startsWith("com.umeng.") &&
              !typ.name.startsWith("com.netqin")
              res
          }.map (_._2.code).mkString("\n")
          if(code.contains(";.loadClass:(Ljava/lang/String;)Ljava/lang/Class;"))
            dynamicLoading = true
          if(code.contains("Ljava/lang/Class;.forName:") || code.contains("java.lang.reflect."))
            reflection = true
          if(code.contains("NATIVE {"))
            nativecode = true
          if(dynamicLoading) ProposalNeedsCounter.haveDynamicLoading += 1
          if(reflection) ProposalNeedsCounter.haveReflection += 1
          if(nativecode) ProposalNeedsCounter.haveNativeCode += 1
          ProposalNeedsCounter.haveresult += 1
        } catch {
          case e: Throwable => 
            reporter.error(TITLE, "Error:" + e)
            if(true) e.printStackTrace()
        } finally {
          println(ProposalNeedsCounter.toString)
          socket.cleanEnv
        }
    }
  }
}
