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

import akka.actor._
import akka.pattern.ask
import akka.routing.RoundRobinPool
import org.sireum.util._
import org.sireum.amandroid.Apk
import org.sireum.amandroid.decompile.ApkDecompiler
import org.sireum.amandroid.parser.ManifestParser
import org.sireum.jawa.util.MyFileUtil
import org.sireum.amandroid.dedex._
import org.sireum.jawa.JawaType
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import akka.pattern.AskTimeoutException
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import java.util.concurrent.TimeoutException
import org.sireum.jawa.util.FutureUtil

/**
 * This is an actor for managing the whole decompile process.
 *   
 * @author Fengguo Wei
 */
class DecompilerActor extends Actor with ActorLogging {
  
  def receive: Receive = {
    case ddata: DecompileData =>
      sender ! decompile(ddata)
  }
  
  def decompile(ddata: DecompileData): DecompilerResult = {
    log.info("Start decompile " + ddata.fileUri)
    if(Apk.isValidApk(ddata.fileUri)) {
        var opUri: Option[FileResourceUri] = None
        val codes: MMap[JawaType, String] = mmapEmpty
        val listener = new PilarStyleCodeGeneratorListener {
          def onRecodeGenerated(recType: JawaType, code: String, outputUri: Option[FileResourceUri]) = {
            opUri = outputUri
            codes(recType) = code
          }
          def onGenerateEnd(recordCount: Int) = {}
        }
        val apkFile = FileUtil.toFile(ddata.fileUri)
        val resultDir = FileUtil.toFile(ddata.outputUri)
        val (f, cancel) = FutureUtil.interruptableFuture[DecompilerResult] { () =>
          val res = 
            try {
              val (outApkUri, srcs, deps) = ApkDecompiler.decompile(apkFile, resultDir, ddata.dpsuri, false, false, ddata.removeSupportGen, ddata.forceDelete, Some(listener))
              DecompileSuccResult(ddata.fileUri, outApkUri, srcs, deps)
            } catch {
              case e: Exception =>
                DecompileFailResult(ddata.fileUri, Some(e))
            }
          res
        }
        val res = try {
          Await.result(f, ddata.timeout)
        } catch {
          case te: TimeoutException =>
            cancel()
            log.warning("Decompile timeout for " + ddata.fileUri)
            DecompileFailResult(ddata.fileUri, Some(te))
        }
        res match {
          case dfr: DecompileFailResult =>
            val dirName = try{apkFile.getName().substring(0, apkFile.getName().lastIndexOf("."))} catch {case e: Exception => apkFile.getName()}
            val outDir = FileUtil.toFile(MyFileUtil.appendFileName(ddata.outputUri, dirName))
            MyFileUtil.deleteDir(outDir)
          case _ =>
            codes.foreach {
              case (typ, code) =>
                PilarStyleCodeGenerator.outputCode(typ, code, opUri)
            }
        }
        res
      } else {
        DecompileFailResult(ddata.fileUri, None)
      }
  }
}

object DecompileTestApplication extends App {
  val _system = ActorSystem("DecompileApp", ConfigFactory.load)
  val supervisor = _system.actorOf(Props[DecompilerActor], name = "decompile_supervisor")
  val fileUris = FileUtil.listFiles(FileUtil.toUri("/Users/fgwei/Develop/Sireum/apps/amandroid/sources/icc-bench"), ".apk", true)
  val outputUri = FileUtil.toUri("/Users/fgwei/Work/output/icc-bench")
  val futures = fileUris map {
    fileUri =>
      (supervisor.ask(DecompileData(fileUri, outputUri, None, true, true, 10 seconds))(30 seconds)).mapTo[DecompilerResult].recover{
          case te: AskTimeoutException =>
            (fileUri, false)
          case ex: Exception => 
            (fileUri, false)
        }
  }
  val fseq = Future.sequence(futures)
  Await.result(fseq, Duration.Inf).foreach {
    dr => println(dr)
  }
  _system.shutdown
}
