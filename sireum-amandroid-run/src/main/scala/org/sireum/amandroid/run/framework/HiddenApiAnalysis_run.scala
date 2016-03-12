/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
package org.sireum.amandroid.run.framework

import org.sireum.util._
import java.io.FileInputStream
import java.io.InputStreamReader
import java.io.BufferedReader
import java.util.regex.Pattern

object HiddenApiAnalysis_run {
  def main(args: Array[String]): Unit = {
    val benignApps = Set("/Volumes/USFLab4T/Work/output/playdrone-00/HiddenApiUsage.txt", "/Volumes/USFLab4T/Work/output/playdrone-01/HiddenApiUsage.txt")
    val malApps = Set("/Users/fgwei/Google Drive/SmartPhoneSec/hiddenApiResult/VirusShare_Android_20140324/HiddenApiUsage.txt")
    val benignAppsUri = benignApps.map(FileUtil.toUri(_))
    val malAppsUri = malApps.map(FileUtil.toUri(_))
    compareDataSetApi(benignAppsUri, malAppsUri)
  }
  
  private def compareDataSetApi(benignAppsUri: ISet[FileResourceUri], malAppsUri: ISet[FileResourceUri]) = {
    val benignHideApiList: MSet[String] = msetEmpty
    val benignNonPublicApiList: MSet[String] = msetEmpty
    val malHideApiList: MSet[String] = msetEmpty
    val malNonPublicApiList: MSet[String] = msetEmpty
    benignAppsUri foreach {
      fileUri =>
        val (bh, bn) = fillList(fileUri)
        benignHideApiList ++= bh
        benignNonPublicApiList ++= bn
    }
    malAppsUri foreach {
      fileUri =>
        val (mh, mn) = fillList(fileUri)
        malHideApiList ++= mh
        malNonPublicApiList ++= mn
    }
    val bHaveMNotHide: IList[String] = (benignHideApiList -- malHideApiList).toList.sortBy(a => a)
    val bHaveMNotNonPublic: IList[String] = (benignNonPublicApiList -- malNonPublicApiList).toList.sortBy(a => a)
    val mHaveBNotHide: IList[String] = (malHideApiList -- benignHideApiList).toList.sortBy(a => a)
    val mHaveBNotNonPublic: IList[String] = (malNonPublicApiList -- benignNonPublicApiList).toList.sortBy(a => a)
    println("HideApi, Benign -- Malware:")
    bHaveMNotHide foreach {
      api => println(api)
    }
    println("Non Public Api, Benign -- Malware:")
    bHaveMNotNonPublic foreach {
      api => println(api)
    }
    println("HideApi, Malware -- Benign:")
    mHaveBNotHide foreach {
      api => println(api)
    }
    println("Non Public Api, Malware -- Benign:")
    mHaveBNotNonPublic foreach {
      api => println(api)
    }
  }
  
  private def fillList(fileUri: FileResourceUri): (ISet[String], ISet[String]) = {
    var isHide: Boolean = true
    val apiPattern = Pattern.compile("([^\\s]+)\\s([0-9]+)")
    val hideApiList: MSet[String] = msetEmpty
    val nonPublicApiList: MSet[String] = msetEmpty
    val f = FileUtil.toFile(fileUri)
    val fbr: BufferedReader = new BufferedReader(new InputStreamReader(new FileInputStream(f)))
    var line = fbr.readLine()
    while(line != null) {
      if(line == "Non Public Method List:") isHide = false
      val m = apiPattern.matcher(line)
      if(m.find()) {
        if(isHide)
          hideApiList += m.group(1)
        else nonPublicApiList += m.group(1)
      }
      line = fbr.readLine()
    }
    (hideApiList.toSet, nonPublicApiList.toSet)
  }
}
