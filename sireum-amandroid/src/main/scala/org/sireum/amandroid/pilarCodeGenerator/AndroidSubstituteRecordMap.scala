/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.pilarCodeGenerator

import org.sireum.util._

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object AndroidSubstituteRecordMap {
	def getSubstituteRecordMap : IMap[String, String] = {
	  val map : MMap[String, String] = mmapEmpty
	  map.put("android.content.Context", "android.content.ContextWrapper")
    map.put("android.view.Menu", "com.android.internal.view.menu.MenuBuilder")
    map.put("android.content.SharedPreferences", "android.app.SharedPreferencesImpl")
    map.put("android.os.IBinder", " android.os.Binder")
    map.put("android.hardware.display.IDisplayManager", "android.hardware.display.DisplayManager")
	  map.toMap
	}
}