package org.sireum.amandroid.pilarCodeGenerator

import org.sireum.util._

object AndroidSubstituteRecordMap {
	def getSubstituteRecordMap : IMap[String, String] = {
	  val map : MMap[String, String] = mmapEmpty
	  map.put("[|android:content:Context|]", "[|android:content:ContextWrapper|]")
	  map.toMap
	}
}