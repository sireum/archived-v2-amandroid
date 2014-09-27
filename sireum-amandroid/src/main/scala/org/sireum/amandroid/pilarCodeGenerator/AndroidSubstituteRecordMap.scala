/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.pilarCodeGenerator

import org.sireum.util._

object AndroidSubstituteRecordMap {
	def getSubstituteRecordMap : IMap[String, String] = {
	  val map : MMap[String, String] = mmapEmpty
	  map.put("android.content.Context", "android.content.ContextWrapper")
	  map.toMap
	}
}