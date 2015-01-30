package org.sireum.amandroid.util

import org.sireum.util._
import org.sireum.amandroid.appInfo.AppInfoCollector
import org.sireum.amandroid.parser.ResourceFileParser
import org.sireum.jawa.JawaCodeSource
import org.sireum.jawa.util.URLInString

object AndroidUrlCollector {
  def collectUrls(file : FileResourceUri, outUri : FileResourceUri) : ISet[String] = {
    val man = AppInfoCollector.analyzeManifest(outUri + "AndroidManifest.xml")
    val afp = AppInfoCollector.analyzeARSC(file)    
    val strs = msetEmpty[String]
    val rfp = new ResourceFileParser
    rfp.parseResourceFile(file)
    strs ++= rfp.getAllStrings
    strs ++= afp.getGlobalStringPool.map(_._2)
    val codes = JawaCodeSource.getAppRecordsCodes
    val code_urls : Set[String] =
      if(!codes.isEmpty){
        codes.map{
          case (name, code) =>
            URLInString.extract(code)
        }.reduce(iunion[String])
      } else isetEmpty[String]
    val res_urls : Set[String] =
      if(!strs.isEmpty){
        strs.map{
          str =>
            URLInString.extract(str)
        }.reduce(iunion[String])
      } else isetEmpty[String]   
    code_urls ++ res_urls
  }
}