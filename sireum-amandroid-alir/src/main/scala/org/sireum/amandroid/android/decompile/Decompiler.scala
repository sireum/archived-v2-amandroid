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

object DecompilerCli {
	def run(sadmode : SireumAmandroidDecompileMode) {
    val sourceType = sadmode.typ match{
      case DumpSource.APK => "APK"
      case DumpSource.DIR => "DIR"
      case DumpSource.DEX => "DEX"}
    val sourceDir = sadmode.srcFile
    val sourceFile = new File(sourceDir)
    val outputDirOpt = if(sadmode.outFile == "") None else Some(sadmode.outFile)
    val outputDir = outputDirOpt match{
	    case Some(path) => path
	    case None => sourceFile.getParent()
	  }
    forkProcess(sourceType, sourceDir, outputDir)
    println("Decompile finish and results are saved in: " + outputDir)
  }
	
	def forkProcess(typSpec : String, sourceDir : String, outputDir : String) = {
	  val args = List("-t", typSpec, sourceDir, outputDir)
    org.sireum.jawa.util.JVMUtil.startSecondJVM(Decompiler.getClass(), "-Xmx2G", args, true)
  }
}

object Decompiler {
	def main(args: Array[String]) {
	  if(args.size != 4){
	    println("Usage: -t type[allows: APK, DIR, DEX] <source path> <output path>")
	    return
	  }
	  val typ = args(1)
	  val sourcePath = args(2)
	  val outputPath = args(3)
	  val outputUri = FileUtil.toUri(outputPath)
	  val dexFileUris = typ match{
      case "APK" =>
        require(sourcePath.endsWith(".apk"))
        val apkFileUri = FileUtil.toUri(sourcePath)
        Set(APKFileResolver.getDexFile(apkFileUri, outputUri))
      case "DIR" =>
        require(new File(sourcePath).isDirectory())
        val apkFileUris = FileUtil.listFiles(FileUtil.toUri(sourcePath), ".apk", true).toSet
        apkFileUris.map{
		      apkFileUri=>
		        APKFileResolver.getDexFile(apkFileUri, outputUri)
		    }
      case "DEX" =>
        require(sourcePath.endsWith(".dex") || sourcePath.endsWith(".odex"))
        Set(FileUtil.toUri(sourcePath))
      case _ => 
        println("Unexpected type: " + typ)
        return
    }
	  println(dexFileUris)
    decompile(dexFileUris)
  }
	
	def decompile(dexFileUris : Set[FileResourceUri]) : Set[FileResourceUri] = {
    dexFileUris.map{
      dexFileUri =>
        Dex2PilarConverter.convert(dexFileUri)
    }
	}
}