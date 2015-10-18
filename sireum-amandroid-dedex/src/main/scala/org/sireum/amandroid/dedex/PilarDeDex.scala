/*
Copyright (c) 2015-2016 Fengguo Wei, University of South Florida.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.dedex

import java.io.RandomAccessFile
import org.sireum.util._
import java.io.PrintStream
import hu.uw.pallergabor.dedexer._
import java.io.File
import java.io.IOException

/**
 * @author fgwei
 * 
 * adapted from Dedexer hu.uw.pallergabor.dedexer.Dedexer
 */
class PilarDeDex {
  
  private var dexLogStream: Option[PrintStream] = None
  private val depFiles: MList[DexDependencyFile] = mlistEmpty
  
  def decompile(
      sourceFileUri: FileResourceUri,
      targetDirUri: Option[FileResourceUri],
      depsDirUri: Option[FileResourceUri],
      dexlog: Boolean,
      debugMode: Boolean): Unit = {
    try{
      val raf = new RandomAccessFile(FileUtil.toFilePath(sourceFileUri), "r")
      if(dexlog){ 
        if(targetDirUri.isDefined)
          dexLogStream = Some(new PrintStream(FileUtil.toFilePath(targetDirUri.get) + "/dex.log"))
        else {
          System.err.println("If want dexlog please specify the target dir")
          return
        }
      }
  
      val dexSignatureBlock = new DexSignatureBlock()
      dexSignatureBlock.setRandomAccessFile(raf)
      dexSignatureBlock.setDumpFile(dexLogStream.getOrElse(null))
      dexSignatureBlock.parse()
  
      var depsParser: DexDependencyParser = null
      var dexOffsetResolver: DexOffsetResolver = null
      if(depsDirUri.isDefined &&
          dexSignatureBlock.getDexOptimizationData() != null &&
          dexSignatureBlock.getDexOptimizationData().isOptimized()) {
        depsParser = new DexDependencyParser()
        depsParser.setDexSignatureBlock(dexSignatureBlock)
        depsParser.setRandomAccessFile(raf)
        depsParser.setDumpFile(dexLogStream.getOrElse(null))
        depsParser.parse()
        dexOffsetResolver = new DexOffsetResolver()
        dexOffsetResolver.setDumpFile(dexLogStream.getOrElse(null))
        if(handleDependencies(depsDirUri.get, depsParser, dexOffsetResolver)) {
          close()
          return
        }
      }
      val dexPointerBlock = new DexPointerBlock()
      dexPointerBlock.setRandomAccessFile(raf)
      dexPointerBlock.setDumpFile(dexLogStream.getOrElse(null))
      dexPointerBlock.setDexSignatureBlock(dexSignatureBlock)
      dexPointerBlock.parse()
  
      val dexStringIdsBlock = new DexStringIdsBlock()
      dexStringIdsBlock.setRandomAccessFile(raf)
      dexStringIdsBlock.setDumpFile(dexLogStream.getOrElse(null))
      dexStringIdsBlock.setDexPointerBlock(dexPointerBlock)
      dexStringIdsBlock.setDexSignatureBlock(dexSignatureBlock)
      dexStringIdsBlock.parse()
  
      val dexTypeIdsBlock = new DexTypeIdsBlock()
      dexTypeIdsBlock.setRandomAccessFile(raf)
      dexTypeIdsBlock.setDumpFile(dexLogStream.getOrElse(null))
      dexTypeIdsBlock.setDexPointerBlock(dexPointerBlock)
      dexTypeIdsBlock.setDexStringIdsBlock(dexStringIdsBlock)
      dexTypeIdsBlock.parse();
  
      val dexProtoIdsBlock = new DexProtoIdsBlock()
      dexProtoIdsBlock.setRandomAccessFile(raf)
      dexProtoIdsBlock.setDumpFile(dexLogStream.getOrElse(null))
      dexProtoIdsBlock.setDexPointerBlock(dexPointerBlock)
      dexProtoIdsBlock.setDexStringIdsBlock(dexStringIdsBlock)
      dexProtoIdsBlock.setDexTypeIdsBlock(dexTypeIdsBlock)
      dexProtoIdsBlock.setDexSignatureBlock(dexSignatureBlock)
      dexProtoIdsBlock.parse()
  
      val dexFieldIdsBlock = new DexFieldIdsBlock()
      dexFieldIdsBlock.setRandomAccessFile(raf)
      dexFieldIdsBlock.setDumpFile(dexLogStream.getOrElse(null))
      dexFieldIdsBlock.setDexPointerBlock(dexPointerBlock)
      dexFieldIdsBlock.setDexStringIdsBlock(dexStringIdsBlock)
      dexFieldIdsBlock.setDexTypeIdsBlock(dexTypeIdsBlock)
      dexFieldIdsBlock.parse()
  
      val dexMethodIdsBlock = new DexMethodIdsBlock()
      dexMethodIdsBlock.setRandomAccessFile(raf)
      dexMethodIdsBlock.setDumpFile(dexLogStream.getOrElse(null))
      dexMethodIdsBlock.setDexPointerBlock(dexPointerBlock)
      dexMethodIdsBlock.setDexStringIdsBlock(dexStringIdsBlock)
      dexMethodIdsBlock.setDexTypeIdsBlock(dexTypeIdsBlock)
      dexMethodIdsBlock.setDexProtoIdsBlock(dexProtoIdsBlock)
      dexMethodIdsBlock.parse()
  
      val dexClassDefsBlock = new DexClassDefsBlock()
      dexClassDefsBlock.setRandomAccessFile(raf)
      dexClassDefsBlock.setDumpFile(dexLogStream.getOrElse(null))
      dexClassDefsBlock.setDexPointerBlock(dexPointerBlock)
      dexClassDefsBlock.setDexStringIdsBlock(dexStringIdsBlock)
      dexClassDefsBlock.setDexTypeIdsBlock(dexTypeIdsBlock)
      dexClassDefsBlock.setDexFieldIdsBlock(dexFieldIdsBlock)
      dexClassDefsBlock.setDexMethodIdsBlock(dexMethodIdsBlock)
      dexClassDefsBlock.setDexSignatureBlock(dexSignatureBlock)
      dexClassDefsBlock.parse()
      if(dexOffsetResolver != null)
        dexOffsetResolver.addToOffsetResolver(dexClassDefsBlock)
        
      val pscg = new PilarStyleCodeGenerator(
          dexSignatureBlock,
          dexStringIdsBlock,
          dexTypeIdsBlock,
          dexFieldIdsBlock,
          dexMethodIdsBlock,
          dexClassDefsBlock,
          dexOffsetResolver,
          targetDirUri,
          raf,
          dexLogStream)
      pscg.generate
      raf.close()
    } catch {
      case ex: IOException =>
        System.err.println("I/O error: "+ex.getMessage())
        if(debugMode)
            ex.printStackTrace()
      case ex: UnknownInstructionException =>
        System.err.println(ex.getMessage())
      case ex: Exception =>
        ex.printStackTrace()
    }
    close()
  }
  
  private def handleDependencies(
      depsDirUri: FileResourceUri,
      depsParser: DexDependencyParser,
      resolver: DexOffsetResolver): Boolean = {
    var error = false
    for(i <- 0 to depsParser.getDependencySize() - 1) {
      val dependencyFileName = depsParser.getDependencyElement(i)
      val slashIndex = dependencyFileName.lastIndexOf('/')
      val shortFileName = 
        if(slashIndex < 0) dependencyFileName
        else dependencyFileName.substring(slashIndex + 1)
      var raf: RandomAccessFile = null
      var f: File = null
      try {
        f = new File(FileUtil.toFilePath(depsDirUri), shortFileName)
        raf = new RandomAccessFile(f, "r")
        println( "Reading dependency file " + f +
            " (derived from ODEX file dependency "+
            dependencyFileName + ")" )
        val depFile: DexDependencyFile = new DexDependencyFile(raf, dexLogStream.getOrElse(null))
        depFile.setDexOffsetResolver(resolver)
        depFile.parse()
        depFiles += depFile
        raf.close()
      } catch {
          case ex: IOException =>
            System.err.println("Cannot open dependency file: " + f +
                " (derived from ODEX file dependency " + dependencyFileName + ")");
            error = true
      }
    }
    error
  }
  
  private def close() = {
    if(dexLogStream.isDefined)
      dexLogStream.get.close()
  }
}

object PilarDeDex {
  def main(args: Array[String]): Unit = {
    val srcRes = args(0)
    val srcResUri = FileUtil.toUri(srcRes)
    val pdd = new PilarDeDex
    val dpsuri = FileUtil.toUri("/Users/fgwei/Downloads/dedexer/system/deps")
    pdd.decompile(srcResUri, None, Some(dpsuri), false, true)
  }
}