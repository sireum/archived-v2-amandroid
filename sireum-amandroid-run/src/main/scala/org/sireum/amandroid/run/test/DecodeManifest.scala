package org.sireum.amandroid.run.test

import org.sireum.amandroid.decompile.AmDecoder
import org.sireum.util.FileUtil

object DecodeManifest extends App {
  val apkF = args(0)
  val outPath = args(1)
  AmDecoder.decodeManifest(FileUtil.toUri(apkF), FileUtil.toUri(outPath))
}