package org.sireum.amandroid.run.test

import java.io.BufferedReader
import java.io.FileReader
import org.sireum.util._
import org.sireum.jawa.util.URLInString
import java.net.URL
import java.net.MalformedURLException
import java.io.File
import java.io.PrintWriter
import java.io.FileWriter

object UrlCategorize_run {
  private final val TITLE = "UrlCategorize_run"
  
  final val comm_reg = ".*(android|google|facebook|baidu|twitter|feedburner|jabber|foxnews|espn|apache|xml.org|youtube|gstatic|slf4j|microsoft|washingtonpost|forbes|xmlsoap|xmlpull|htc|samsung|amazon|openxmlformats|sina)\\..*"
  final val ad_reg = ".*(adfonic|winad|admarvel|appmedia|smartadserver|immob|polar|medialets|baidu|sense|mads|tapjoy|innerActive|zestadz|admogo|smaato|daum|inmobi|imobile|flurry|youmi|airad|mopub|millennialmedia|adchina|mobclix|adlib_android|vpon|mobfox|casee|cauly|adserver|waps|pontiflex|googleads|adwo|adwhirl|domob|energysource|madvertise|madhouse|qwapi|mdotm|guohead|nexage|wiyun|mobisage|sktelecom|vdopia|adlocal|admob|greystripe|uplusad|adagogo|jumptap|everbadge|microad|mobgold|yicha|mobot|mediba|moolah|adknowledge|transpera|adview|adlantis|airpush|leadbolt|wqmobile|wooboo)\\..*"
  final val org_reg = ".*\\.(com|org)"
  
  def main(args: Array[String]): Unit = {
    if(args.size != 1){
      System.err.print("Usage: source_path")
      return
    }
    
    val urls : MSet[String] = msetEmpty
    val base_url : MSet[String] = msetEmpty
    val uncomm_url : MSet[String] = msetEmpty
    val uncomm_norAd_url : MSet[String] = msetEmpty
    val uncomm_norAd_nororg : MSet[String] = msetEmpty
    
    val filepath = args(0)
    val file = new File(filepath)
    val br = new BufferedReader(new FileReader(file))
    var line : String = br.readLine()
    while(line != null) {
      urls ++= URLInString.extract(line)
      line = br.readLine()
    }
    br.close()
    
    urls.foreach{
      url =>
        try{
          val u = new URL(url)
          base_url += u.getHost()
        } catch {
          case e : MalformedURLException =>
        }
    }
    uncomm_url ++= base_url.filter{
      burl =>
        !burl.matches(comm_reg)
    }
    uncomm_norAd_url ++= uncomm_url.filter{
      uurl =>
        !uurl.matches(ad_reg)
    }
    uncomm_norAd_nororg ++= uncomm_norAd_url.filter{
      uaurl =>
        !uaurl.matches(org_reg)
    }
    val outputdir = file.getParent()
    val basefile = outputdir + "/baseurl.txt"
    val basepw = new FileWriter(basefile)
    val uncommfile = outputdir + "/uncommon.txt"
    val uncommpw = new FileWriter(uncommfile)
    val uncomm_norAd_file = outputdir + "/uncommon_norAd.txt"
    val uncomm_norAd_pw = new FileWriter(uncomm_norAd_file)
    val uncomm_norAd_nororg_file = outputdir + "/uncommon_norAd_norOrg.txt"
    val uncomm_norAd_nororg_pw = new FileWriter(uncomm_norAd_nororg_file)
    try{
      base_url.foreach(u => basepw.write(u + "\n"))
      uncomm_url.foreach(u => uncommpw.write(u + "\n"))
      uncomm_norAd_url.foreach(u => uncomm_norAd_pw.write(u + "\n"))
      uncomm_norAd_nororg.foreach(u => uncomm_norAd_nororg_pw.write(u + "\n"))
    } finally {
      basepw.close()
      uncommpw.close()
      uncomm_norAd_pw.close()
      uncomm_norAd_nororg_pw.close()
    }
  }
}