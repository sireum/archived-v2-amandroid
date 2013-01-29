package org.sireum.analyseApp.amandroid


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.sireum.analyseApp.framework.amandroid.AmandroidAnalyseAppFrameWork
import org.sireum.appFiles.amandroid.AmandroidFiles


@RunWith(classOf[JUnitRunner])
class AmandroidAnalyseApp extends AmandroidAnalyseAppFrameWork{
  AmandroidFiles.apkModelFiles.
  foreach{fileUri=>
    if(fileUri.indexOf("0eb4b") > 0)
    Analyzing title fileUri file fileUri
  }
}