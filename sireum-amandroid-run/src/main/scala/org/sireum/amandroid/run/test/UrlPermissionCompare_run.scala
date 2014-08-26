package org.sireum.amandroid.run.test

import org.sireum.jawa.MessageCenter
import org.sireum.util._
import org.sireum.jawa.MessageCenter._
import org.sireum.amandroid.android.appInfo.AppInfoCollector
import org.sireum.amandroid.android.parser.ResourceFileParser
import org.sireum.amandroid.android.parser.ARSCFileParser
import java.io.File
import java.net.URI
import org.sireum.jawa.util.APKFileResolver
import org.sireum.amandroid.android.decompile.Dex2PilarConverter
import org.sireum.jawa.JawaCodeSource
import org.sireum.jawa.GlobalConfig
import org.sireum.jawa.DefaultLibraryAPISummary
import org.sireum.jawa.util.URLInString
import java.io.FileWriter
import java.io.BufferedReader
import java.io.FileReader

object UrlPermissionCompare_run {
  private final val TITLE = "UrlPermissionCompare_run"
  
  def main(args: Array[String]): Unit = {
    if(args.size != 1){
      System.err.print("Usage: source_path")
      return
    }
    MessageCenter.msglevel = MessageCenter.MSG_LEVEL.CRITICAL
    val outputpath = "/Volumes/ArgusGroup/Stash/outputs/url_collection"
    val outputUri = FileUtil.toUri(outputpath)
    
    val base_url : MSet[String] = msetEmpty
    val uncomm_url : MSet[String] = msetEmpty
    val uncomm_norAd_url : MSet[String] = msetEmpty
    val uncomm_norAd_nororg_url : MSet[String] = msetEmpty
    
    val base_file = new File(outputpath + "/baseurl.txt")
    val base_br = new BufferedReader(new FileReader(base_file))
    var line : String = base_br.readLine()
    while(line != null) {
      base_url ++= URLInString.extract(line)
      line = base_br.readLine()
    }
    base_br.close()
    
    val uncomm_url_file = new File(outputpath + "/uncommon.txt")
    val uncomm_url_br = new BufferedReader(new FileReader(uncomm_url_file))
    line = uncomm_url_br.readLine()
    while(line != null) {
      uncomm_url ++= URLInString.extract(line)
      line = uncomm_url_br.readLine()
    }
    uncomm_url_br.close()
    
    val uncomm_norAd_url_file = new File(outputpath + "/uncommon_norAd.txt")
    val uncomm_norAd_url_br = new BufferedReader(new FileReader(uncomm_norAd_url_file))
    line = uncomm_norAd_url_br.readLine()
    while(line != null) {
      uncomm_norAd_url ++= URLInString.extract(line)
      line = uncomm_norAd_url_br.readLine()
    }
    uncomm_norAd_url_br.close()
    
    val uncomm_norAd_nororg_url_file = new File(outputpath + "/uncommon_norAd_norOrg.txt")
    val uncomm_norAd_nororg_url_br = new BufferedReader(new FileReader(uncomm_norAd_nororg_url_file))
    line = uncomm_norAd_nororg_url_br.readLine()
    while(line != null) {
      base_url ++= URLInString.extract(line)
      line = uncomm_norAd_nororg_url_br.readLine()
    }
    uncomm_norAd_nororg_url_br.close()
    
    val base_url_perm : MSet[String] = msetEmpty
    val uncomm_url_perm : MSet[String] = msetEmpty
    val uncomm_norAd_url_perm : MSet[String] = msetEmpty
    val uncomm_norAd_nororg_url_perm : MSet[String] = msetEmpty
    
    val base_url_perm_reverse : MSet[String] = msetEmpty
    val uncomm_url_perm_reverse : MSet[String] = msetEmpty
    val uncomm_norAd_url_perm_reverse : MSet[String] = msetEmpty
    val uncomm_norAd_nororg_url_perm_reverse : MSet[String] = msetEmpty
    
    val sourcePath = args(0)
    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
//    val results : MMap[String, (Set[String], Set[String])] = mmapEmpty
    files.foreach{
      file =>
        msg_critical(TITLE, "####" + file + "#####")
        try{
          val man = AppInfoCollector.analyzeManifest(file)
          val perms = man.getPermissions
          val strs = msetEmpty[String]
        	val rfp = new ResourceFileParser
        	rfp.parseResourceFile(file)
        	strs ++= rfp.getAllStrings
        	val arsc = new ARSCFileParser
        	arsc.parse(file)
        	strs ++= arsc.getGlobalStringPool.map(_._2)
          
          val srcFile = new File(new URI(file))
        	val dexFile = APKFileResolver.getDexFile(file, FileUtil.toUri(srcFile.getParentFile()))
        	
        	// convert the dex file to the "pilar" form
        	val pilarRootUri = Dex2PilarConverter.convert(dexFile)
        	val pilarFile = new File(new URI(pilarRootUri))
        	//store the app's pilar code in AmandroidCodeSource which is organized record by record.
        	JawaCodeSource.load(pilarRootUri, GlobalConfig.PILAR_FILE_EXT, DefaultLibraryAPISummary)
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
          
          val urls = code_urls ++ res_urls
          
          urls.foreach{
            url =>
              if(base_url.exists(p => p.contains(url))){
                base_url_perm ++= perms
              } else {
                base_url_perm_reverse ++= perms
              }
              if(uncomm_url.exists(p => p.contains(url))){
                uncomm_url_perm ++= perms
              } else {
                uncomm_url_perm_reverse ++= perms
              }
              if(uncomm_norAd_url.exists(p => p.contains(url))){
                uncomm_norAd_url_perm ++= perms
              } else {
                uncomm_norAd_url_perm_reverse ++= perms
              }
              if(uncomm_norAd_nororg_url.exists(p => p.contains(url))){
                uncomm_norAd_nororg_url_perm ++= perms
              } else {
                uncomm_norAd_nororg_url_perm_reverse ++= perms
              }
          }
        } catch {
          case e : Exception =>
            err_msg_critical(TITLE, e.getMessage())
        } finally {
          JawaCodeSource.clearAppRecordsCodes
    	    APKFileResolver.deleteOutputs(file, outputUri)
    	    System.gc
        }
    }
    
    val base_url_perm_only : MSet[String] = base_url_perm -- base_url_perm_reverse
    val uncomm_url_perm_only : MSet[String] = uncomm_url_perm -- uncomm_url_perm_reverse
    val uncomm_norAd_url_perm_only : MSet[String] = uncomm_norAd_url_perm -- uncomm_norAd_url_perm_reverse
    val uncomm_norAd_nororg_url_perm_only : MSet[String] = uncomm_norAd_nororg_url_perm -- uncomm_norAd_nororg_url_perm_reverse

    val basefile = outputpath + "/baseurl_perm.txt"
    val basepw = new FileWriter(basefile)
    val uncommfile = outputpath + "/uncommon_perm.txt"
    val uncommpw = new FileWriter(uncommfile)
    val uncomm_norAd_file = outputpath + "/uncommon_norAd_perm.txt"
    val uncomm_norAd_pw = new FileWriter(uncomm_norAd_file)
    val uncomm_norAd_nororg_file = outputpath + "/uncommon_norAd_norOrg_perm.txt"
    val uncomm_norAd_nororg_pw = new FileWriter(uncomm_norAd_nororg_file)
    try{
      base_url_perm_only.foreach(u => basepw.write(u + "\n"))
      uncomm_url_perm_only.foreach(u => uncommpw.write(u + "\n"))
      uncomm_norAd_url_perm_only.foreach(u => uncomm_norAd_pw.write(u + "\n"))
      uncomm_norAd_nororg_url_perm_only.foreach(u => uncomm_norAd_nororg_pw.write(u + "\n"))
    } finally {
      basepw.close()
      uncommpw.close()
      uncomm_norAd_pw.close()
      uncomm_norAd_nororg_pw.close()
    }
  }
}