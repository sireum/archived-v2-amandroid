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
 * This is a supervisor actor for managing the whole decompile process.
 * It has two sub actors: 
 * 	 DexDecodeActor, decode given dex file and send back record type to code mapping data.
 *   DecodeOutputActor, output given code to file, console, etc.
 *   
 * @author Fengguo Wei
 */
class DecompilerActor(nrOfDecodeInstances: Int) extends Actor with ActorLogging {
  
  def receive: Receive = {
    case ddata: DecompileData =>
      log.info("Start decompile " + ddata.fileUri)
      if(Apk.isValidApk(ddata.fileUri)) {
        val apkFile = FileUtil.toFile(ddata.fileUri)
        val resultDir = FileUtil.toFile(ddata.outputUri)
        val (outUri, srcs, deps) = ApkDecompiler.decompile(apkFile, resultDir, ddata.dpsuri, false, false, ddata.removeSupportGen, ddata.forceDelete)
      }
    case dt: DecompileTimeout =>
      
  }
}

class DecompilePrioMailbox(settings: ActorSystem.Settings, config: Config) extends UnboundedPriorityMailbox (
  PriorityGenerator {
    case DecompileTimeout => 0
    case _ => 1
  }
)

object DecompileTestApplication extends App {
  val _system = ActorSystem("DecompileApp", ConfigFactory.load)
  val supervisor = _system.actorOf(Props(new DecompilerActor(2)).withDispatcher("akka.decompile-prio-dispatcher"), name = "decompile_supervisor")
//  implicit val timeout = Timeout(40 seconds)
  val fileUris = FileUtil.listFiles(FileUtil.toUri("/Users/fgwei/Develop/Sireum/apps/amandroid/sources/icc-bench"), ".apk", true)
  val outputUri = FileUtil.toUri("/Users/fgwei/Work/output/icc-bench")
  val futures = fileUris map {
    fileUri =>
      (supervisor.ask(DecompileData(fileUri, outputUri, None, true, true))(10 seconds)).mapTo[DecompilerResult].recover{
          case te: AskTimeoutException =>
            supervisor ! DecompileTimeout(fileUri, outputUri)
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