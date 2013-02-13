package org.sireum.amandroid.cache

import org.sireum.util.cache.CacheProvider
import org.sireum.util.cache.FileCaseFactory
import org.sireum.util._
import java.io._

final class AndroidCacheFile[K] extends CacheProvider[K] with FileCaseFactory[K]{
  
  val cacheMap : MMap[K, (Any, Integer)] = mmapEmpty
  var size : Integer = 0
  var removePercent : Integer = 20
  var serializer : (Any, OutputStream) --> Unit = null
  var unSerializer : InputStream --> Any = null
  var outer : FileOutputStream = null
  var inner : FileInputStream = null
  var rootDirectory : FileResourceUri = null

  def save[T](key : K, value : T) = {
    require(!rootDirectory.equals(null))
    setFileOutputStream(key)
    serializer((value, outer))
    outer.close()
  }

  def load[T](key : K) : T = {
    require(!rootDirectory.equals(null))
    if(cacheMap.contains(key)){
      cacheUpdateAndSort(key)
      cacheMap(key)._1.asInstanceOf[T]
    } else {
      setFileInputStream(key)
      val value = unSerializer(inner).asInstanceOf[T]
      if(size == 0){
        //do nothing
      } else if(cacheMap.size <= size){
        cacheMap(key) = (value, 1)
      } else {
        collectCacheMap()
        cacheMap(key) = (value, 1)
      }
      value
    }
  }
  
  def setCacheSize(s : Integer) = {
    size = s
  }
  
  def setRemovePercent(p : Integer) {
    assert(p != 0)
    removePercent = p
  }
  
  def cacheUpdateAndSort(key : K) = {
    cacheMap(key) = (cacheMap(key)._1, cacheMap(key)._2 + 1)
    cacheMap.toSeq.sortBy(_._2._2)
    cacheMap.foreach(
      item => println(item._2._1 + " " + item._2._2)  
    )
  }
  
  def collectCacheMap() = {
    val i = (size * removePercent) / 100
    cacheMap.dropRight(i)
  }
  
  def setValueSerializer(f : (Any, OutputStream) --> Unit, g : InputStream --> Any) = {
    serializer = f
    unSerializer = g
  }
  
  def setRootDirectory(path : FileResourceUri) = {
    rootDirectory = path
  }
  
  def formatProcedureUri(pUri : ResourceUri) : String = {
    pUri.split("%5B%7C")(1).split("%7C%5D")(0).replaceAll("::", ".").replaceAll("%3", ".").replace('$', '.')
  }
  
  def fileNameBuilder(pUri : ResourceUri) : FileResourceUri = {
    formatProcedureUri(pUri) + Integer.toHexString(pUri.hashCode) + ".xml"
  }
  
  def setFileInputStream(key : K) = {
    var fileName : String = null
    key match {
      case pUri : ResourceUri =>
        fileName = rootDirectory + fileNameBuilder(pUri)
      case _ =>
    }
    inner = new FileInputStream(fileName)
  }
    
  def setFileOutputStream(key : K) = {
    var fileName : String = null
    key match {
      case pUri : ResourceUri =>
        fileName = rootDirectory + fileNameBuilder(pUri)
      case _ =>
    }
    val file = new File(fileName)
    outer = new FileOutputStream(file)
  }
}