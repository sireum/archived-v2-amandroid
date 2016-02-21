/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Main Contributors:
 *    Fengguo Wei - Argus Lab @ University of South Florida
 *    Sankardas Roy - Bowling Green State University
 *    
 * Contributors:
 *    Robby - Santos Lab @ Kansas State University
 *    Wu Zhou - Fireeye
 *    Fengchi Lin - Chinese People's Public Security University
 ******************************************************************************/
//package org.sireum.amandroid.run.test
//
//import org.sireum.util._
//import java.io.File
//import java.net.URI
//import org.sireum.jawa.util.APKFileResolver
//import org.sireum.amandroid.decompile.Dex2PilarConverter
//import org.sireum.jawa.DefaultLibraryAPISummary
//import org.sireum.jawa.util.URLInString
//import java.io.PrintWriter
//import org.sireum.amandroid.appInfo.AppInfoCollector
//import org.sireum.amandroid.parser.ResourceFileParser
//import org.sireum.amandroid.parser.ARSCFileParser
//import java.io.FileWriter
//import org.sireum.amandroid.util.AndroidLibraryAPISummary
//import org.sireum.amandroid.security.AmandroidSocket
//import org.sireum.jawa.DefaultReporter
//import org.sireum.jawa.Global
//import org.sireum.amandroid.Apk
//import org.sireum.jawa.Constants
//
///**
// * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
// * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
// */ 
//object UrlCollection_run {
//  private final val TITLE = "UrlCollection_run"
//  
//  def main(args: Array[String]): Unit = {
//    if(args.size != 2){
//      System.err.print("Usage: source_path output_path")
//      return
//    }
////    MessageCenter.msglevel = MessageCenter.MSG_LEVEL.CRITICAL
//    val outputpath = args(1)
//    val outputUri = FileUtil.toUri(outputpath)
//    val sourcePath = args(0)
//    
//    val files = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
////    val results : MMap[String, (Set[String], Set[String])] = mmapEmpty
//    files.foreach{
//      file =>
//        val reporter = new DefaultReporter
//        val global = new Global(file, reporter)
//        val apk = new Apk(file)
//        val socket = new AmandroidSocket(global, apk)
//        reporter.echo(TITLE, "####" + file + "#####")
//        try{
//          val outUri = socket.loadApk(outputpath, AndroidLibraryAPISummary)
//          val man = AppInfoCollector.analyzeManifest(reporter, outUri + "AndroidManifest.xml")
//          
//          val strs = msetEmpty[String]
//          val rfp = new ResourceFileParser
//          rfp.parseResourceFile(file)
//          strs ++= rfp.getAllStrings
//          val arsc = new ARSCFileParser
//          arsc.parse(file)
//          strs ++= arsc.getGlobalStringPool.map(_._2)
//          
//          val srcFile = new File(new URI(file))
//          val dexFile = APKFileResolver.getDexFile(file, FileUtil.toUri(srcFile.getParentFile()))
//        
//          // convert the dex file to the "pilar" form
//          val pilarRootUri = Dex2PilarConverter.convert(Set(dexFile), FileUtil.toUri(srcFile.getParentFile()))
//          val pilarFile = new File(new URI(pilarRootUri))
//          //store the app's pilar code in AmandroidCodeSource which is organized record by record.
//          global.load(pilarRootUri, Constants.PILAR_FILE_EXT, DefaultLibraryAPISummary)
//          val codes = global.getApplicationClassCodes
//          val code_urls : Set[String] =
//            if(!codes.isEmpty){
//              codes.map{
//                case (name, source) =>
//                  URLInString.extract(source.code)
//              }.reduce(iunion[String])
//            } else isetEmpty[String]
//          val res_urls : Set[String] =
//            if(!strs.isEmpty){
//              strs.map{
//                str =>
//                  URLInString.extract(str)
//              }.reduce(iunion[String])
//            } else isetEmpty[String]
//          val summaryfile = new File(outputpath + "/summary.txt")
//          val sos = new FileWriter(summaryfile, true)
//          try{
//            sos.write(file + ":\n")
//            sos.write("package name: " + man.getPackageName + "\n")
//            sos.write("resource:\n")
//            res_urls.foreach{
//              case rurl =>
//                sos.write(rurl + "\n")
//            }
//            sos.write("code:\n")
//            code_urls.foreach{
//              case curl =>
//                sos.write(curl + "\n")
//            }
//            sos.write("\n\n")
//          } finally {
//            sos.close()
//            APKFileResolver.deleteOutputs(file, outputUri)
//            System.gc
//          }
//        } catch {
//          case e : Exception =>
//            reporter.error(TITLE, e.getMessage())
//        }
//    }
//    
//  }
//}
