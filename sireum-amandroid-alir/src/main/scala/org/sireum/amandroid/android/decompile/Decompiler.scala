package org.sireum.amandroid.android.decompile

import org.sireum.option.SireumAmandroidDecompileMode
import org.sireum.option.DumpSource
import org.sireum.jawa.util.APKFileResolver
import org.sireum.util.FileUtil
import java.io.File
import org.sireum.util.FileResourceUri
import java.net.URI

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object Decompiler {
	def run(sadmode : SireumAmandroidDecompileMode) {
    val sourceType = sadmode.typ
    val sourceUri = FileUtil.toUri(sadmode.srcFile)
    val sourceFile = new File(new URI(sourceUri))
    val outputDirOpt = if(sadmode.outFile == "") None else Some(sadmode.outFile)
    val outputDir = outputDirOpt match{
	    case Some(path) => path
	    case None => sourceFile.getParent()
	  }
    val outputUri = FileUtil.toUri(outputDir)
    val apkFileUris = sourceType match{
      case DumpSource.APK =>
        require(sourceUri.endsWith(".apk"))
        Set(sourceUri)
      case DumpSource.DEX =>
        require(sourceUri.endsWith(".dex") || sourceUri.endsWith(".odex"))
        Set(sourceUri)
      case DumpSource.DIR =>
        require(new File(new URI(sourceUri)).isDirectory())
        FileUtil.listFiles(sourceUri, ".apk", true).toSet
    }
    val dexFileUris = apkFileUris.map{
      apkFileUri=>
        APKFileResolver.getDexFile(apkFileUri, outputUri)
    }
    decompile(dexFileUris)
    println("Decompile finish!")
  }
	
	def decompile(dexFileUris : Set[FileResourceUri]) : Set[FileResourceUri] = {
    dexFileUris.map{
      dexFileUri =>
        Dex2PilarConverter.convert(dexFileUri)
    }
	}
}