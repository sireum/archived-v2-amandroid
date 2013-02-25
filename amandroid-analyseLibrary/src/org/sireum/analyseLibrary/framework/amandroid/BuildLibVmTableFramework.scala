package org.sireum.analyseLibrary.framework.amandroid

import org.sireum.test.framework.TestFramework
import org.sireum.util._
import java.io._
import org.sireum.amandroid.xml.AndroidXStream
import org.sireum.amandroid.AndroidSymbolResolver._
import java.util.zip.GZIPOutputStream
import java.util.zip.GZIPInputStream


class BuildLibVmTableFramework extends TestFramework { 
  
  //////////////////////////////////////////////////////////////////////////////
  // Implemented Public Methods
  //////////////////////////////////////////////////////////////////////////////

  def Analyzing : this.type = this

  def title(s : String) : this.type = {
    _title = caseString + s
    this
  }

  def file(fileUri : FileResourceUri) =
    AmandroidConfiguration(title, fileUri)

  //////////////////////////////////////////////////////////////////////////////
  // Public Case Classes
  //////////////////////////////////////////////////////////////////////////////

  case class AmandroidConfiguration //
  (title : String, srcs : FileResourceUri) {

    ////////////////////////////////////////////////////////////////////////////
    // Test Constructor
    ////////////////////////////////////////////////////////////////////////////

    test(title) {
      println("####" + title + "#####")
        val currentVmTablesFile = new File(srcs.toString().substring(5))
        //create directory
        val nameArray = currentVmTablesFile.getName().split("\\.")
        var dirName : String = ""
        for(i <- 0 until nameArray.length-1){
          dirName += nameArray(i)
        }
        val d = srcs.substring(srcs.indexOf("/"), srcs.lastIndexOf("/")+1)
        val srcFiles = mlistEmpty[FileResourceUri]
        val resDir = new File(d+"libVmTables")
        if(!resDir.exists()){
          resDir.mkdir()
        }
        
        val xStream = AndroidXStream
        xStream.xstream.alias("AndroidVirtualMethodTables", classOf[AndroidVirtualMethodTables])
        
        val libVmTablesFile = new File(resDir + "/" + "libVmTables.xml.zip")
        var libVmTables : AndroidVirtualMethodTables = null
        if(libVmTablesFile.exists()){
          val interAVMT = new GZIPInputStream(new FileInputStream(libVmTablesFile))
          libVmTables = xStream.fromXml(interAVMT).asInstanceOf[AndroidVirtualMethodTables]
          interAVMT.close()
        }
        
        val currentVmTables = xStream.fromXml(currentVmTablesFile).asInstanceOf[AndroidVirtualMethodTables]
        
        if(libVmTables == null){
          libVmTables = currentVmTables
        } else {
          println("before merge: " + libVmTables.asInstanceOf[AndroidVirtualMethodTablesProducer].tables.cannotFindRecordTable)
          // if libVmTables already have something means we need to merge.
          libVmTables.mergeWith(currentVmTables)
          println("after merge: " + libVmTables.asInstanceOf[AndroidVirtualMethodTablesProducer].tables.cannotFindRecordTable)
        }
        
        val outerAVMT = new GZIPOutputStream(new FileOutputStream(libVmTablesFile))
        
        println("start convert AVMT to xml!")
        xStream.toXml(libVmTables, outerAVMT)
        outerAVMT.close()
        println("###############################################")
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Implemented Protected Methods and Fields
  //////////////////////////////////////////////////////////////////////////////

  protected var _title : String = null
  protected var num = 0
  protected def title() = if (_title == null) {
    num += 1
    "Analysis #" + num
  } else _title

}