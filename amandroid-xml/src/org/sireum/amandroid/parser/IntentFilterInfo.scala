package org.sireum.amandroid.parser

import org.sireum.util.ResourceUri
//import scala.collection.JavaConversions._



class IntentFilterDataBase {
  /**
   * Map from record name to it's intent filter information
   */
  private var intentFmap : Map[String, Set[IntentFilter]] = Map()
  def updateIntentFmap(intentFilter : IntentFilter) = {
    if(containsRecord(intentFilter.getHolder)){
      var filters = intentFmap(intentFilter.getHolder)
      filters += intentFilter
      intentFmap += (intentFilter.getHolder -> filters)
    } else intentFmap += (intentFilter.getHolder -> Set(intentFilter))
  }
  def containsRecord(rName : String) : Boolean = intentFmap.contains(rName)
  def getIntentFmap() = intentFmap
  def getIntentFilterInfo(rName : ResourceUri) = intentFmap.getOrElse(rName, null)
  def getIntentActions(rName : ResourceUri) : Set[String] = {
    val intentFilterS: Set[IntentFilter] = getIntentFilterInfo(rName)
    var actions:Set[String] = null
    if(intentFilterS != null){     
      actions = Set()
      intentFilterS.foreach{       
      intentFilter =>
        actions ++= intentFilter.getActions
      }      
    }
    actions
  }
  
  override def toString() = intentFmap.toString
}



class IntentFilter(holder : String) {
	private var actions : Set[String] = Set()
	private var categorys : Set[String] = Set()
	private var datas : Set[String] = Set()
//	def mergeIntentFilterInfo(info2 : IntentFilter) = {
//	  if(holder == info2.getHolder){
//	    actions ++= info2.actions
//	    categorys ++= info2.categorys
//	    datas ++= info2.datas
//	  }
//	}
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
  
  def getActions() = IntentFilter.this.actions
  def getCategorys() = IntentFilter.this.categorys
  def getDatas() = IntentFilter.this.datas
  def getHolder() = IntentFilter.this.holder
  
  override def toString() = "component: " + holder + " (actions: " + actions + " categorys: " + categorys + " datas: " + datas + ")"
}