package org.sireum.amandroid.concurrent

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import akka.pattern.ask
import akka.routing.RoundRobinPool
import org.sireum.util._
import org.sireum.amandroid.Apk
import org.sireum.amandroid.decompile.ApkDecompiler
import org.sireum.amandroid.parser.ManifestParser
import org.sireum.jawa.util.MyFileUtil
import org.sireum.amandroid.dedex._
import org.sireum.jawa.JawaType
import akka.actor.ActorSystem
import akka.actor.ReceiveTimeout
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import akka.actor.ActorLogging
import akka.pattern.AskTimeoutException

/**
 * This is a supervisor actor for managing the whole decompile process.
 * It has two sub actors: 
 * 	 DexDecodeActor, decode given dex file and send back record type to code mapping data.
 *   DecodeOutputActor, output given code to file, console, etc.
 *   
 * @author Fengguo Wei
 */
class DecompilerActor(nrOfDecodeInstances: Int, nrOfOutputInstances: Int) extends Actor with ActorLogging {
  private val decodeActor = context.actorOf(Props[DexDecodeActor].withRouter(
      RoundRobinPool(nrOfInstances = nrOfDecodeInstances)), name = "decompile_decode")
  private val outputActor = context.actorOf(Props[DecodeOutputActor].withRouter(
      RoundRobinPool(nrOfInstances = nrOfOutputInstances)), name = "decompile_output")
  
  class ApkDecResult(val fileUri: FileResourceUri) {
    var outUri: FileResourceUri = null
    val dexUrisNotDone: MMap[FileResourceUri, Int] = mmapEmpty
    val srcFolders: MSet[String] = msetEmpty
    val dependencies: MSet[String] = msetEmpty
  }
  private val filesUnderProcessing: MMap[Int, ApkDecResult] = mmapEmpty
  private val dexRecordCount: MMap[FileResourceUri, Int] = mmapEmpty
  private val decodeInfo: MSet[FileResourceUri] = msetEmpty
  private val senderShip: MMap[Int, ActorRef] = mmapEmpty
  
  def receive: Receive = {
    case ddata: DecompileData =>
      log.info("Start processing " + ddata.fileUri)
      val index = System.identityHashCode(ddata.fileUri, ddata.outputUri)
      senderShip(index) = sender
      filesUnderProcessing.getOrElseUpdate(index, new ApkDecResult(ddata.fileUri))
      decompile(index, ddata)
    case dodata: DecodeOutputData => // get from DexDecodeActor
      outputActor ! dodata  // send to DecodeOutputActor
    case didata: DecodeInfoData => // get from DexDecodeActor
      val apkres = filesUnderProcessing(didata.index)
      apkres.srcFolders += didata.src
      apkres.dependencies ++= didata.dependent
      decodeInfo += didata.dexUri
      checkAndRespond(didata.index, didata.dexUri)
    case ddr: DexDecodeResult => // get from DexDecodeActor
      filesUnderProcessing(ddr.index).dexUrisNotDone(ddr.dexUri) = ddr.recordCount
      checkAndRespond(ddr.index, ddr.dexUri)
    case dopr: DecodeOutputResult => // get from DecodeOutputActor
      dexRecordCount(dopr.dexUri) = dexRecordCount(dopr.dexUri) + 1
      checkAndRespond(dopr.index, dopr.dexUri)
    case ReceiveTimeout =>
      outputActor ! ReceiveTimeout
  }
  
  private def checkAndRespond(index: Int, dexUri: FileResourceUri) = {
    val apkres = filesUnderProcessing(index)
    val desiredCount = apkres.dexUrisNotDone(dexUri)
    val actualCount = dexRecordCount(dexUri)
    if(desiredCount != -1 && actualCount >= desiredCount && decodeInfo.contains(dexUri)) {
      apkres.dexUrisNotDone -= dexUri
      dexRecordCount -= dexUri
      decodeInfo -= dexUri
      if(apkres.dexUrisNotDone.isEmpty) {
        log.info("End processing " + apkres.fileUri)
        val decompieResultReceiver = senderShip(index)
        decompieResultReceiver ! DecompilerResult(apkres.fileUri, apkres.outUri, apkres.srcFolders.toSet, apkres.dependencies.toSet)
        filesUnderProcessing -= index
        senderShip -= index
      }
    }
  }
  
  private def decompile(index: Int, ddata: DecompileData): Boolean = {
    if(Apk.isValidApk(ddata.fileUri)) {
      val outUri = ApkDecompiler.decodeApk(ddata.fileUri, ddata.outputUri, ddata.forceDelete)
      val manifestUri = MyFileUtil.appendFileName(outUri, "AndroidManifest.xml")
      val pkg = ManifestParser.loadPackageName(manifestUri)
      if(FileUtil.toFile(outUri).exists()) {
        val dexUris = FileUtil.listFiles(outUri, ".dex", true)
        val apkres = filesUnderProcessing(index)
        apkres.outUri = outUri
        dexUris.foreach {
          dexUri =>
            apkres.dexUrisNotDone.getOrElseUpdate(dexUri, -1)
            dexRecordCount.getOrElseUpdate(dexUri, 0)
            decodeActor ! DexDecodeData(index, dexUri, outUri, ddata.dpsuri, pkg, ddata.removeSupportGen, ddata.forceDelete)
        }
      }
      true
    } else false
  }
}

class DexDecodeActor extends Actor with ActorLogging {
  def receive: Receive = {
    case dddata: DexDecodeData =>
      sender ! dexDecode(dddata)
  }
  
  def dexDecode(dddata: DexDecodeData): DecodeInfoData = {
    val listener = new PilarStyleCodeGeneratorListener {
      def onRecodeGenerated(recType: JawaType, code: String, outputUri: Option[FileResourceUri]) = {
        sender ! DecodeOutputData(dddata.index, dddata.dexUri, recType, code, outputUri.get) // in this case outputUri always have some value
      }
      def onGenerateEnd(recordCount: Int) {
        sender ! DexDecodeResult(dddata.index, dddata.dexUri, recordCount)
      }
    }
    val (sf, dependent) = ApkDecompiler.decompileDex(dddata.dexUri, dddata.outUri, dddata.dpsuri, dddata.pkg, false, false, dddata.removeSupportGen, dddata.forceDelete, Some(listener))
    DecodeInfoData(dddata.index, dddata.dexUri, sf, dependent)
  }
}

class DecodeOutputActor extends Actor with ActorLogging {
  def receive: Receive = {
    case dodata: DecodeOutputData =>
      sender ! output(dodata)
    case ReceiveTimeout =>
      log.info("DecodeOutputActor -> ReceiveTimeout")
  }
  
  def output(dodata: DecodeOutputData): DecodeOutputResult = {
    try {
      PilarStypeCodeGenerator.outputCode(dodata.recType, dodata.code, Some(dodata.outputUri))
    } catch {
      case e: Exception =>
    }
    DecodeOutputResult(dodata.index, dodata.dexUri)
  }
}

object DecompileTestApplication extends App {
  val _system = ActorSystem("DecompileApp")
  val supervisor = _system.actorOf(Props(new DecompilerActor(1, 2)), name = "decompile_supervisor")
//  implicit val timeout = Timeout(40 seconds)
  val fileUris = FileUtil.listFiles(FileUtil.toUri("/Users/fgwei/Develop/Sireum/apps/amandroid/sources/icc-bench"), ".apk", true)
  val outputUri = FileUtil.toUri("/Users/fgwei/Work/output/icc-bench")
  val futures = fileUris map {
    fileUri =>
      (supervisor.ask(DecompileData(fileUri, outputUri: FileResourceUri, None, true, true))(10 seconds)).mapTo[DecompilerResult].recover{
          case te: AskTimeoutException =>
//            MyFileUtil.deleteDir(outputUri)
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