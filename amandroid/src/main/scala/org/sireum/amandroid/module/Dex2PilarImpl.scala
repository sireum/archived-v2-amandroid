package org.sireum.amandroid.module

import org.sireum.pipeline.PipelineJob
import org.sireum.pipeline.PipelineJobModuleInfo
import org.sireum.util._
import java.io.File
import java.net.URI
import java.io.FileInputStream
import java.io.FileOutputStream
import org.sireum.amandroid.util.Dex2PilarConverter

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */

class Dex2PilarWrapperDef (val job : PipelineJob, info : PipelineJobModuleInfo) extends Dex2PilarWrapperModule {
  val results = mlistEmpty[FileResourceUri]

  this.srcFiles.foreach(s => {
    if (s.endsWith("dex") || s.endsWith("odex")) {
      results += Dex2PilarConverter.convert(s) // check if little type mismatch
    }
  })
  val x = results.toList
  
  this.sources_=(x.map(f => Right(f.toString())))  // "sources" a misnomer? note that this represents the list of output pilar files
}

