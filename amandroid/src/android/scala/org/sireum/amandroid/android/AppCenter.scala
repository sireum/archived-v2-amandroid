package org.sireum.amandroid.android

import org.sireum.util._
import org.sireum.amandroid.AmandroidRecord
import org.sireum.amandroid.android.parser.IntentFilterDataBase

/**
 * this is an object, which hold information of apps. e.g. components, intent-filter database, etc.
 */
object AppCenter {
	private var components : ISet[AmandroidRecord] = isetEmpty
	private var intentFdb : IntentFilterDataBase = new IntentFilterDataBase()
	def setComponent(comp : AmandroidRecord) = this.components += comp
	def setComponents(comps : ISet[AmandroidRecord]) = this.components ++= comps
	def getComponents = this.components
//	def getComponents(appName : String) = this.componentMap.getOrElse(appName, throw new RuntimeException("Didn't find given appName: "+ appName))
	def setIntentFilterDB(i : IntentFilterDataBase) = this.intentFdb = i
	def updateIntentFilterDB(i : IntentFilterDataBase) = this.intentFdb.updateIntentFmap(i)
	def getIntentFilterDB ={
	  if(this.intentFdb == null) throw new RuntimeException("intent-filter database is null")
	  this.intentFdb
	}
}