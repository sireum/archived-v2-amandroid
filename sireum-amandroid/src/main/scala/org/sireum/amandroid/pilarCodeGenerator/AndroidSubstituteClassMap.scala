/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.pilarCodeGenerator

import org.sireum.util._
import org.sireum.jawa.JawaType

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object AndroidSubstituteClassMap {
	def getSubstituteClassMap : IMap[JawaType, JawaType] = {
	  val map : MMap[JawaType, JawaType] = mmapEmpty
	  map.put(new JawaType("android.content.Context"), new JawaType("android.content.ContextWrapper"))
    map.put(new JawaType("android.view.Menu"), new JawaType("com.android.internal.view.menu.MenuBuilder"))
    map.put(new JawaType("android.content.SharedPreferences"), new JawaType("android.app.SharedPreferencesImpl"))
    map.put(new JawaType("android.os.IBinder"), new JawaType("android.os.Binder"))
    map.put(new JawaType("android.hardware.display.IDisplayManager"), new JawaType("android.hardware.display.DisplayManager"))
	  map.toMap
	}
}