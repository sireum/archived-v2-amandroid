/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.pilarCodeGenerator

import org.sireum.util._
import org.sireum.jawa.ObjectType

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object AndroidSubstituteClassMap {
	def getSubstituteClassMap : IMap[ObjectType, ObjectType] = {
	  val map : MMap[ObjectType, ObjectType] = mmapEmpty
	  map.put(new ObjectType("android.content.Context"), new ObjectType("android.content.ContextWrapper"))
    map.put(new ObjectType("android.view.Menu"), new ObjectType("com.android.internal.view.menu.MenuBuilder"))
    map.put(new ObjectType("android.content.SharedPreferences"), new ObjectType("android.app.SharedPreferencesImpl"))
    map.put(new ObjectType("android.os.IBinder"), new ObjectType("android.os.Binder"))
    map.put(new ObjectType("android.hardware.display.IDisplayManager"), new ObjectType("android.hardware.display.DisplayManager"))
	  map.toMap
	}
}