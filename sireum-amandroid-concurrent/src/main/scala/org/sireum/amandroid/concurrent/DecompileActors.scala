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
import akka.dispatch.UnboundedPriorityMailbox
import com.typesafe.config.Config
import akka.dispatch.PriorityGenerator
import com.typesafe.config.ConfigFactory
import akka.routing.Broadcast

/**
 * This is an actor for managing the whole decompile process.
 *   
 * @author Fengguo Wei
 */
class DecompilerActor(timeout: Duration) extends Actor with ActorLogging {
  private var currentOutDirUri: FileResourceUri = null
  def receive: Receive = {
    case ddata: DecompileData =>
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
        val dirName = try{apkFile.getName().substring(0, apkFile.getName().lastIndexOf("."))} catch {case e: Exception => apkFile.getName()}
        currentOutDirUri = MyFileUtil.appendFileName(ddata.outputUri, dirName)
        context.setReceiveTimeout(timeout)
        val (outUri, srcs, deps) = ApkDecompiler.decompile(apkFile, resultDir, ddata.dpsuri, false, false, ddata.removeSupportGen, ddata.forceDelete)
        context.setReceiveTimeout(Duration.Undefined)
        codes.foreach {
          case (typ, code) =>
            PilarStyleCodeGenerator.outputCode(typ, code, opUri)
        }
        sender ! DecompilerResult(true, ddata.fileUri, outUri, srcs, deps)
      } else {
        sender ! DecompilerResult(false, ddata.fileUri, null, null, null)
      }
    case ReceiveTimeout =>
      context.setReceiveTimeout(Duration.Undefined)
      if(currentOutDirUri != null)
        MyFileUtil.deleteDir(FileUtil.toFile(currentOutDirUri))
  }
}

object DecompileTestApplication extends App {
  val _system = ActorSystem("DecompileApp", ConfigFactory.load)
  val supervisor = _system.actorOf(Props(new DecompilerActor(10 seconds)).withDispatcher("akka.decompile-prio-dispatcher"), name = "decompile_supervisor")
//  implicit val timeout = Timeout(40 seconds)
  val fileUris = FileUtil.listFiles(FileUtil.toUri("/Users/fgwei/Develop/Sireum/apps/amandroid/sources/icc-bench"), ".apk", true)
  val outputUri = FileUtil.toUri("/Users/fgwei/Work/output/icc-bench")
  val futures = fileUris map {
    fileUri =>
      (supervisor.ask(DecompileData(fileUri, outputUri, None, true, true))(30 seconds)).mapTo[DecompilerResult].recover{
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