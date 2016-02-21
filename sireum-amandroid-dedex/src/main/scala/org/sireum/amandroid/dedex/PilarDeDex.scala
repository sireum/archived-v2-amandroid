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
package org.sireum.amandroid.dedex

import java.io.RandomAccessFile
import org.sireum.util._
import java.io.PrintStream
import hu.uw.pallergabor.dedexer._
import java.io.File
import java.io.IOException
import org.sireum.jawa.JawaType
import org.sireum.jawa.JawaPackage
import org.sireum.jawa.JavaKnowledge

/**
 * @author fgwei
 * 
 * adapted from Dedexer hu.uw.pallergabor.dedexer.Dedexer
 */
class PilarDeDex {
  
  private var dexLogStream: Option[PrintStream] = None
  private val depFiles: MList[DexDependencyFile] = mlistEmpty
  
  private var pkgNameMapping: IMap[JawaPackage, String] = imapEmpty
  private var recordNameMapping: IMap[JawaType, String] = imapEmpty
  private var procedureNameMapping: IMap[String, String] = imapEmpty
  private var attributeNameMapping: IMap[(String, JawaType), String] = imapEmpty
  def getPkgNameMapping = pkgNameMapping
  def getRecordNameMapping = recordNameMapping
  def getProcedureNameMapping = procedureNameMapping
  def getAttributeNameMapping = attributeNameMapping
  def haveRenamedElements: Boolean = !pkgNameMapping.isEmpty || !recordNameMapping.isEmpty || !procedureNameMapping.isEmpty || !attributeNameMapping.isEmpty
  def mapPackage(pkg: String): String = {
    val pkglist: MList[String] = mlistEmpty
    val jawapkg = JavaKnowledge.formatPackageStringToPackage(pkg)
    jawapkg.getPkgList foreach {
      pkg =>
        getPkgNameMapping.get(pkg) match {
          case Some(name) =>
            pkglist += name
          case None =>
            pkglist += pkg.name
        }
    }
    pkglist.mkString(".")
  }
  def mapRecord(className: String): String = {
    val typ = new JawaType(className)
    val pkgstr = typ.baseType.pkg match {
      case Some(pkg) => mapPackage(pkg.toPkgString("."))
      case None => ""
    }
    val classnamestr = getRecordNameMapping.get(typ) match {
      case Some(name) =>
        name
      case None =>
        typ.baseType.name
    }
    pkgstr + "." + classnamestr
  }
  
  def decompile(
      sourceFileUri: FileResourceUri,
      targetDirUri: Option[FileResourceUri],
      depsDirUri: Option[FileResourceUri],
      recordFilter: (JawaType => Boolean),
      dexlog: Boolean,
      debugMode: Boolean,
      listener: Option[PilarStyleCodeGeneratorListener] = None): Unit = {
    try{
      val raf = new RandomAccessFile(FileUtil.toFilePath(sourceFileUri), "r")
      if(dexlog){ 
        if(targetDirUri.isDefined) {
          dexLogStream = Some(new PrintStream(FileUtil.toFilePath(targetDirUri.get) + File.separator + "dex.log"))
        } else {
          System.err.println("If want dexlog please specify the target dir")
          return
        }
      }
  
      val dexSignatureBlock = new DexSignatureBlock()
      dexSignatureBlock.setRandomAccessFile(raf)
      dexSignatureBlock.setDumpFile(dexLogStream.getOrElse(null))
      dexSignatureBlock.parse()
  
      var depsParser: DexDependencyParser = null
      val dexOffsetResolver: DexOffsetResolver = new DexOffsetResolver()
      if(depsDirUri.isDefined &&
          dexSignatureBlock.getDexOptimizationData() != null &&
          dexSignatureBlock.getDexOptimizationData().isOptimized()) {
        depsParser = new DexDependencyParser()
        depsParser.setDexSignatureBlock(dexSignatureBlock)
        depsParser.setRandomAccessFile(raf)
        depsParser.setDumpFile(dexLogStream.getOrElse(null))
        depsParser.parse()
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
          raf,
          targetDirUri,
          dexLogStream,
          recordFilter)
      pscg.generate(listener)
      raf.close()
      this.pkgNameMapping = pscg.pkgNameMapping.toMap
      this.recordNameMapping = pscg.recordNameMapping.toMap
      this.procedureNameMapping = pscg.procedureNameMapping.toMap
      this.attributeNameMapping = pscg.attributeNameMapping.toMap
    } catch {
      case ex: IOException =>
        System.err.println("I/O error: "+ex.getMessage())
        if(debugMode)
          ex.printStackTrace()
      case ex: UnknownInstructionException =>
        System.err.println(ex.getMessage())
      case ex: Exception =>
        if(debugMode)
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
    val dpsPath = args(1)
    val srcResUri = FileUtil.toUri(srcRes)
    val pdd = new PilarDeDex
    val dpsuri = FileUtil.toUri(dpsPath)
    pdd.decompile(srcResUri, None, Some(dpsuri), ((_) => true), false, true)
  }
}
