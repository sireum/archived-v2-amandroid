//package org.sireum.analyseLibrary.amandroid
//
//import org.sireum.androidLibraryFile.amandroid.AmandroidAndroidLibraryXmlFiles
//import java.io.File
//import org.sireum.amandroid.xml.AndroidXStream
//import java.util.zip.GZIPInputStream
//import java.io.FileInputStream
//import java.util.zip.GZIPOutputStream
//import java.io.FileOutputStream
//import org.sireum.amandroid.symbolResolver.AndroidLibInfoTables
//import org.sireum.amandroid.symbolResolver.AndroidLibInfoResolver
//import org.sireum.amandroid.symbolResolver.AndroidLibInfoTablesProducer
//
//object CombineLibInfoTables{
//  def main(args: Array[String]) {
//	  val DEBUG = true
//	  //create directory        
//	  val libDir = new File(System.getProperty("user.home") + "/AndroidLibData/")
//	  if(!libDir.exists()){
//	    libDir.mkdir()
//	  }
//	  val resDir = new File(libDir + "/libInfoTables/")
//	  if(!resDir.exists()){
//	    resDir.mkdir()
//	  }
//	  
//	  val xStream = AndroidXStream
//	  xStream.xstream.alias("AndroidLibInfoTables", classOf[AndroidLibInfoTables])
//	  
//	  val libInfoTablesFile = new File(resDir + "/libInfoTables.xml.zip")
//	  var libInfoTables : AndroidLibInfoTables = null
//	  if(libInfoTablesFile.exists()){
//	    val interAVMT = new GZIPInputStream(new FileInputStream(libInfoTablesFile))
//	    libInfoTables = xStream.fromXml(interAVMT).asInstanceOf[AndroidLibInfoTables]
//	    interAVMT.close()
//	  }
//		AmandroidAndroidLibraryXmlFiles.xmlModelFiles.foreach{
//		    fileUri =>
//		      if(fileUri.indexOf("LibInfoTables") > 0){
//		        val currentLibInfoTablesFile = new File(fileUri.toString().substring(5))
//		        val currentLibInfoTables = xStream.fromXml(currentLibInfoTablesFile).asInstanceOf[AndroidLibInfoTables]
//			      if(libInfoTables == null){
//			        libInfoTables = currentLibInfoTables
//			      } else {
//			        // if libVmTables already have something means we need to merge.
//			        libInfoTables.mergeWith(currentLibInfoTables)
//			      }
//		      }
//		}
//		if(libInfoTables == null){
//		  System.err.println("Please run step 1 first.")
//		} else{
//			libInfoTables.asInstanceOf[AndroidLibInfoResolver].separateInterfaceImplementAndClassExtend
//			if(DEBUG)
//				println("cannotFindRecordTable size: " + libInfoTables.asInstanceOf[AndroidLibInfoTablesProducer].tables.cannotFindRecordTable.size
//							+ " classExtendTable size: " + libInfoTables.asInstanceOf[AndroidLibInfoTablesProducer].tables.classExtendTable.size
//							+ " interfaceImplementTable size: " + libInfoTables.asInstanceOf[AndroidLibInfoTablesProducer].tables.interfaceImplementTable.size)
//			val outerAVMT = new GZIPOutputStream(new FileOutputStream(libInfoTablesFile))
//			println("start convert AVMT to xml!")
//		  xStream.toXml(libInfoTables, outerAVMT)
//		  outerAVMT.close()
//		}
//  }
//}