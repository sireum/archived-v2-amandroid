package org.sireum.analyseLibrary.amandroid

import scala.collection.JavaConversions._
import org.sireum.amandroid.cache.AndroidCacheFile
import org.sireum.amandroid.xml.AndroidXStream
import org.sireum.util._
import java.io._
import org.sireum.amandroid.AndroidSymbolResolver.AndroidLibInfoTables
import java.util.zip.GZIPInputStream
import org.sireum.androidLibraryFile.amandroid.AmandroidAndroidLibraryXmlFiles
import org.sireum.amandroid.AndroidSymbolResolver.AndroidLibInfoTablesProducer
import org.sireum.amandroid.scfg.CompressedControlFlowGraph
import org.sireum.alir.AlirLocationNode
import java.util.zip.GZIPOutputStream


// sankar comments: the main method takes a list of atomic security APIs S as input, and it finds the set of procedures which 
// can reach (i.e. there is an control flow in between) an API in S.

object Step3_IdentifySecurityRelatedAPIs {
  
  def time(block: => Unit) = {
      val start = System.currentTimeMillis
      block
      val stop = System.currentTimeMillis
      println((stop - start)/1000 + "s")
      
  }
  
  def main(args : Array[String]) {
    time({
    val srcs = AmandroidAndroidLibraryXmlFiles.xmlModelFiles(0)
    val d = srcs.substring(srcs.indexOf("/"), srcs.lastIndexOf("/")+1)
    val xStream = AndroidXStream
    val libVmTablesFile = new File(d + "libVmTables/libVmTables.xml.zip")
    var libInfoTables : AndroidLibInfoTables = null
    if(libVmTablesFile.exists()){
      val interAVMT = new GZIPInputStream(new FileInputStream(libVmTablesFile))
      libInfoTables = xStream.fromXml(interAVMT).asInstanceOf[AndroidLibInfoTables]
      interAVMT.close()
    }
    
    
    val libraryFilePath = d + "ccfgs/"
    val aCache = new AndroidCacheFile[ResourceUri]
    val serializer : (Any, OutputStream) --> Unit = {
      case (v, o) =>
        xStream.toXml(v, o)
    }
    val unSerializer : InputStream --> Any = {
      case o =>
        xStream.fromXml(o)
    }
    aCache.setRootDirectory(libraryFilePath)
    aCache.setValueSerializer(serializer, unSerializer)
    aCache.setCacheSize(100000)
    aCache.setRemovePercent(20)
    
    val tempList : MList[ResourceUri] = mlistEmpty
    val procedures = libInfoTables.asInstanceOf[AndroidLibInfoTablesProducer].tables.procedureUriTable
    println("total method number = " + procedures.size())
    tempList +="pilar:/procedure/default/%5B%7Candroid::location::LocationManager.requestLocationUpdates%7C%5D/1/23/100d0973"
    tempList += "pilar:/procedure/default/%5B%7Candroid::location::LocationManager.requestLocationUpdates%7C%5D/1/23/5ab1845"
    tempList += "pilar:/procedure/default/%5B%7Candroid::location::LocationManager.requestLocationUpdates%7C%5D/1/23/a14cd0d2"
    tempList += "pilar:/procedure/default/%5B%7Candroid::location::LocationManager.requestLocationUpdates%7C%5D/1/23/6a9426cb"
    tempList += "pilar:/procedure/default/%5B%7Candroid::location::LocationManager.requestLocationUpdates%7C%5D/1/23/dd6d2b6f"
    tempList += "pilar:/procedure/default/%5B%7Candroid::location::LocationManager.requestLocationUpdates%7C%5D/1/23/9b52afde"
    tempList += "pilar:/procedure/default/%5B%7Candroid::location::LocationManager.requestLocationUpdates%7C%5D/1/23/53535e43"
    tempList += "pilar:/procedure/default/%5B%7Candroid::location::LocationManager.requestLocationUpdates%7C%5D/1/23/20986b88"
      
    // tempList += "pilar:/procedure/default/%5B%7Candroid::location::Location.getLatitude%7C%5D/1/25/7340288d"
    tempList += "pilar:/procedure/default/%5B%7Candroid::telephony::SmsManager.sendTextMessage%7C%5D/1/23/b3fa9950"

    val atomicSecAPIs = tempList.toSeq
    println(atomicSecAPIs)
    val pUriToBitsetMap : MMap[ResourceUri, (MBitSet,MMap[ResourceUri, Integer])] = mmapEmpty
    atomicSecAPIs.foreach(
      procUri =>
        {
          val bitset : MBitSet = mbitsetEmpty(atomicSecAPIs.size)
          bitset(atomicSecAPIs.indexOf(procUri)) = true
          pUriToBitsetMap(procUri) = (bitset,(mmapEmpty))
          println(procUri + " 's bitset = " + bitset.toString())
        }
    )
    type VirtualLabel = String
    procedures.values.foreach(
      procUri =>
        {
          if(!atomicSecAPIs.contains(procUri)){
            val CCfg = aCache.load[CompressedControlFlowGraph[VirtualLabel]](procUri)
            val bitset : MBitSet = mbitsetEmpty(atomicSecAPIs.size)
            pUriToBitsetMap(procUri) = (bitset,(mmapEmpty))
          }
        }
    )
    println("init done!")
    var flag = true
    var i = 0
    while(flag) {
      i+=1
      println("round" + i)
      flag = false
      procedures.values.foreach(
        procUri =>
          {
            if(!atomicSecAPIs.contains(procUri)){
              val CCfg = aCache.load[CompressedControlFlowGraph[VirtualLabel]](procUri)
              var myBs = pUriToBitsetMap(procUri)._1
              val pToRMap : MMap[ResourceUri, Integer] = mmapEmpty
              CCfg.nodes.foreach(
                node =>
                  node match{
                    case n : AlirLocationNode =>
                      val calleeSig = n.getProperty[String]("calleeSig")
                      if(calleeSig != null){  // so, the current locDecl is actually a caller node
                        val calleeOptions = libInfoTables.getCalleeOptionsBySignature(calleeSig)
  //                        println(calleeOptions)
                        if(calleeOptions != null) {                    
                          import scala.collection.JavaConversions._  // we need this for the next for loop 
                          for (callee <- calleeOptions)
                          {
                            var yourBs = pUriToBitsetMap(callee)._1
  //                          println("myBs = " + myBs(0) + myBs(1) + " yourBs = " + yourBs(0) + yourBs(1))
                            if(!myBs.equals(yourBs)){
                              val tempBs = myBs.clone
                              myBs = myBs | yourBs
                              if(!myBs.equals(tempBs)){
                                pToRMap(callee) = i
                                pUriToBitsetMap(procUri) = (myBs, pToRMap.clone)
                                flag = true
                              }
                            }
                          } 
                        }
                      }
                    case _ =>
                  }
              )
            }
          }
      )
    }
    println("fix point algo done!")
    val lbsp = new File(d+"libBitSet/")
    if(!lbsp.exists())
      lbsp.mkdir()
    val libBitSetFile = new File(d+"libBitSet/libBitSet.xml.zip")
    val outerAVMT = new GZIPOutputStream(new FileOutputStream(libBitSetFile))
    xStream.toXml(pUriToBitsetMap, outerAVMT)
    outerAVMT.close()
    })
  }
}