package org.sireum.amandroid.alir.sideEffectAnalysis

import org.sireum.amandroid.alir.AndroidGlobalConfig
import org.sireum.jawa.JawaCodeSource
import org.sireum.amandroid.android.libPilarFiles.AndroidLibPilarFiles
import org.sireum.jawa.Center
import org.sireum.jawa.alir.JawaAlirInfoProvider
import org.sireum.jawa.alir.sideEffectAnalysis.SideEffectAnalysis
import org.sireum.util._
import org.sireum.jawa.JawaProcedure
import org.sireum.jawa.xml.AndroidXStream
import java.io.File
import java.io.FileOutputStream
import java.util.zip.ZipOutputStream
import java.io.BufferedOutputStream
import java.io.OutputStreamWriter
import java.util.zip.GZIPOutputStream
import java.util.zip.GZIPInputStream
import java.io.FileInputStream
import org.sireum.jawa.alir.sideEffectAnalysis.InterProceduralSideEffectAnalysisResult
import scala.collection.parallel.immutable.ParMap
import org.sireum.jawa.GlobalConfig

object AndroidLibSideEffectAnalysis {
  
	def main(args: Array[String]) {
	  AndroidGlobalConfig.initJawaAlirInfoProvider
	  val androidLibDir = AndroidGlobalConfig.android_lib_dir
	  if(androidLibDir != null){
	    val startTime = System.currentTimeMillis()
			JawaCodeSource.preLoad(FileUtil.toUri(androidLibDir), GlobalConfig.PILAR_FILE_EXT)
			var x : Float = 0
			val recSize = JawaCodeSource.getLibraryRecordsCodes.size
			val recs =
				JawaCodeSource.getLibraryRecordsCodes.par.map{
				  case (recName, code) =>
				    this.synchronized(x += 1)
				    if(x%100==0)println((x/recSize)*100 + "%")
				    if(x == recSize) println("Record resolving Done!")
				    Center.resolveRecord(recName, Center.ResolveLevel.BODY)
				}
	    val procedures = recs.par.map(_.getProcedures.filter(_.isConcrete)).reduce(iunion[JawaProcedure])
	    val procSize = procedures.size
	    x = 0
		  var intraPSEResults = procedures.par.map{
        p => 
          this.synchronized(x += 1)
			    if(x%1000==0)println((x/procSize)*100 + "%")
			    if(x == procSize) println("Intra side effect Done!")
          (p.getSignature, SideEffectAnalysis.intraProceduralSideEffect(p))
      }.toMap
      Center.reset
      System.gc()
      System.gc()
	    val interPSEResult = SideEffectAnalysis.interProceduralSideEffect(intraPSEResults)
	    intraPSEResults = ParMap()
	    System.gc()
	    System.gc()
	    val outputDir = AndroidGlobalConfig.amandroid_home + "/output"
	  	val fileDir = new File(outputDir + "/sideEffectAnalysis")
	  	if(!fileDir.exists()) fileDir.mkdirs()
	  	val file = new File(fileDir + "/AndroidLibSideEffectResult.xml.zip")
	    val w = new FileOutputStream(file)
      val zipw = new GZIPOutputStream(new BufferedOutputStream(w))
	    AndroidXStream.toXml(interPSEResult, zipw)
	    zipw.close()
	    println("Result stored!")
	    val reader = new GZIPInputStream(new FileInputStream(file))
	    val xmlObject = AndroidXStream.fromXml(reader).asInstanceOf[InterProceduralSideEffectAnalysisResult]
      reader.close()
      println("xml loaded!")
      println(interPSEResult.equals(xmlObject))
			val endTime = System.currentTimeMillis()
			println("Total time: " + (endTime - startTime)/1000 + "s")
	  } else {
	  	System.err.println("Wrong!")
	  }
  }
}