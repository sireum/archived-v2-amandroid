/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.libPilarFiles

import org.sireum.util._
import org.sireum.jawa.util.MyFileUtil
import org.sireum.jawa.GlobalConfig

object AndroidLibPilarFiles{
  protected def sourceDirUri(path : String) = { 
    FileUtil.toUri(path)
  }
  protected def listFiles(dirUri : FileResourceUri,
                   ext : String) : ISeq[FileResourceUri] =
    FileUtil.listFiles(dirUri, ext, true)
    
  def pilarModelFiles(path : String) = listFiles(sourceDirUri(path), GlobalConfig.PILAR_FILE_EXT)
}