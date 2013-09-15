package org.sireum.amandroid.android

import org.sireum.util._
import org.sireum.amandroid.AmandroidRecord
import org.sireum.amandroid.android.parser.IntentFilterDataBase

/**
 * this is an object, which hold information of apps. e.g. components, intent-filter database, etc.
 */
object AppCenter {
	private var componentMap : IMap[String, ISet[AmandroidRecord]] = imapEmpty
	private var intentFdb : IntentFilterDataBase = null
	def setComponents(appName : String, comps : ISet[AmandroidRecord]) = this.componentMap += (appName -> comps)
	def getComponents = if(!this.componentMap.isEmpty)this.componentMap.values.reduce(iunion[AmandroidRecord]) else isetEmpty
	def getComponents(appName : String) = this.componentMap.getOrElse(appName, throw new RuntimeException("Didn't find given appName: "+ appName))
	def setIntentFilterDB(i : IntentFilterDataBase) = this.intentFdb = i
	def getIntentFilterDB ={
	  if(this.intentFdb == null) throw new RuntimeException("intent-filter database is null")
	  this.intentFdb
	}
}