package org.sireum.amandroid.parser

import org.sireum.util.ResourceUri

class IntentDataBase {
  /**
   * Map from record name to it's intent filter information
   */
  private var intentMap : Map[String, IntentInfo] = Map()
  def updateIntentMap(intent : IntentInfo) = {
    if(containsRecord(intent.getHolder)){
      val it = getIntentInfo(intent.getHolder)
      it.mergeIntentInfo(intent)
    } else intentMap += (intent.getHolder -> intent)
  }
  def containsRecord(rName : String) : Boolean = intentMap.contains(rName)
  def getIntentMap() = intentMap
  def getIntentInfo(rName : ResourceUri) = intentMap.getOrElse(rName, null)
  def getIntentActions(rName : ResourceUri) : Set[String] = {
    val intent = getIntentInfo(rName)
    if(intent != null)
      intent.getActions
    else null
  }
  override def toString() = intentMap.toString
}

class IntentInfo(holder : String) {
	private var actions : Set[String] = Set()
	private var categorys : Set[String] = Set()
	private var datas : Set[String] = Set()
	def mergeIntentInfo(info2 : IntentInfo) = {
	  if(holder == info2.getHolder){
	    actions ++= info2.actions
	    categorys ++= info2.categorys
	    datas ++= info2.datas
	  }
	}
	def setAction(action : String) = actions += action
	def setCategory(category : String) = categorys += category
	def setData(scheme : String, 
	    				host : String, 
	    				port : String, 
	    				path : String, 
	    				pathPrefix : String, 
	    				pathPattern : String,
	    				mimeType : String) = {
	  var uri : String = null
	  if(scheme != null){
	    uri = scheme
	    if(host != null){
	      uri += "://" + host
	      if(port != null){
	        uri += ":" + port
	        if(path != null){
	          uri += "/" + path
	        }
	      }
	    }
	    datas += uri
	  }
	  if(pathPrefix != null){
	    datas += pathPrefix
	  }
	  if(pathPattern != null){
	    datas += pathPattern
	  }
	  if(mimeType != null){
	    datas += mimeType
	  }
  }
  
  def getActions() = this.actions
  def getCategorys() = this.categorys
  def getDatas() = this.datas
  def getHolder() = this.holder
  
  override def toString() = "component: " + holder + "(actions: " + actions + "categorys: " + categorys + "datas: " + datas + ")"
}