package org.sireum.amandroid.android.decompile

import org.sireum.option.SireumAmandroidDecompileMode
import org.sireum.option.DumpSource
import org.sireum.jawa.util.APKFileResolver
import org.sireum.util.FileUtil
import java.io.File

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object Decompiler {
	def run(sadmode : SireumAmandroidDecompileMode) {
	  System.out.println("Start decompiler!")
    val sourceType = sadmode.typ
    val sourceDir = sadmode.srcFile
    val outputDir = if(sadmode.outFile == "") None else Some(sadmode.outFile)
    
    val dexFiles = sourceType match{
      case DumpSource.APK =>
        require(sourceDir.endsWith(".apk"))
        Set(APKFileResolver.getDexFile(sourceDir, outputDir))
      case DumpSource.DEX =>
        require(sourceDir.endsWith(".dex") || sourceDir.endsWith(".odex"))
        Set(sourceDir)
      case DumpSource.DIR =>
        require(new File(sourceDir).isDirectory())
        FileUtil.listFiles(FileUtil.toUri(sourceDir), ".apk", true)
    }
    
    dexFiles.foreach{
      dexFile =>
        Dex2PilarConverter.convert(dexFile)
    }
    System.out.println("Decompiler finish!")
  }
}