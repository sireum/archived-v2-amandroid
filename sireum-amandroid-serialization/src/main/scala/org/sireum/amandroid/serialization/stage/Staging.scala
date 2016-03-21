package org.sireum.amandroid.serialization.stage

import org.sireum.util._
import org.sireum.amandroid.Apk
import org.sireum.jawa.alir.pta.PTAResult
import org.sireum.jawa.Signature
import org.sireum.jawa.util.MyFileUtil
import java.io.PrintWriter
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.{read, write}
import org.sireum.amandroid.serialization.ApkSerializer
import org.sireum.amandroid.serialization.PTAResultSerializer
import java.io.FileReader

object Staging {
  
  def stage(apk: Apk, ptaresult: PTAResult, outApkUri: FileResourceUri): Unit = {
    val outStageUri = MyFileUtil.appendFileName(outApkUri, "stage")
    val apkRes = FileUtil.toFile(MyFileUtil.appendFileName(outStageUri, "apk.json"))
    val oapk = new PrintWriter(apkRes)
    val ptsRes = FileUtil.toFile(MyFileUtil.appendFileName(outStageUri, "ptaresult.json"))
    val opts = new PrintWriter(ptsRes)
    implicit val formats = Serialization.formats(NoTypeHints) + ApkSerializer + PTAResultSerializer
    try {
      write(apk, oapk)
      write(ptaresult, opts)
    } catch {
      case e: Exception =>
        apkRes.delete()
        ptsRes.delete()
        throw e
    } finally {
      oapk.flush()
      oapk.close()
      opts.flush()
      opts.close()
    }
  }
  
  def recoverStage(outApkUri: FileResourceUri): (Apk, PTAResult) = {
    val outStageUri = MyFileUtil.appendFileName(outApkUri, "stage")
    val outStageDir = FileUtil.toFile(outStageUri)
    if(!outStageDir.exists()) outStageDir.mkdirs()
    val apkRes = FileUtil.toFile(MyFileUtil.appendFileName(outStageUri, "apk.json"))
    val rapk = new FileReader(apkRes)
    val ptsRes = FileUtil.toFile(MyFileUtil.appendFileName(outStageUri, "ptaresult.json"))
    val rpts = new FileReader(ptsRes)
    implicit val formats = Serialization.formats(NoTypeHints) + ApkSerializer + PTAResultSerializer
    try {
      val apk = read[Apk](rapk)
      val ptaresult = read[PTAResult](rpts)
      (apk, ptaresult)
    } catch {
      case e: Exception =>
        throw e
    } finally {
      rapk.close()
      rpts.close()
    }
  }
  
  def isStageAvailable(outApkUri: FileResourceUri): Boolean = {
    val outStageUri = MyFileUtil.appendFileName(outApkUri, "stage")
    val outStageDir = FileUtil.toFile(outStageUri)
    outStageDir.exists()
  }
}