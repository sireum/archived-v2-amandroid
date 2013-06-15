package org.sireum.amandroid.parser

import org.sireum.util.ResourceUri




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
	private var data = new Data

	
	
	def addAction(action : String) = actions += action
	def addCategory(category : String) = categorys += category
	def modData(scheme : String, 
	    				host : String, 
	    				port : String, 
	    				path : String, 
	    				pathPrefix : String, 
	    				pathPattern : String,
	    				mimeType : String) = {
	  data.add(scheme, host, port, path, pathPrefix, pathPattern, mimeType)
	  
		//	  var uri : String = null
		//	  if(scheme != null){
		//	    uri = scheme
		//	    if(host != null){
		//	      uri += "://" + host
		//	      if(port != null){
		//	        uri += ":" + port
		//	        if(path != null){
		//	          uri += "/" + path
		//	        }
		//	      }
		//	    }
		//	    data += uri
		//	  }
		//	  if(pathPrefix != null){
		//	    data += pathPrefix
		//	  }
		//	  if(pathPattern != null){
		//	    data += pathPattern
		//	  }
		//	  if(mimeType != null){
		//	    data += mimeType
		//	  }
  }
  
  def getActions() = IntentFilter.this.actions
  def getCategorys() = IntentFilter.this.categorys
  def getData() = IntentFilter.this.data
  def getHolder() = IntentFilter.this.holder
  
  override def toString() = "component: " + holder + " (actions: " + actions + " categorys: " + categorys + " datas: " + data + ")"
}

// A Data class represents all pieces of info associated with all <data> tags of a particular filter as declared in a manifest file 

class Data{
  private var scheme: Set[String] = Set()
  private var host: Set[String] = Set()  
  private var port: Set[String] = Set()
  private var path: Set[String] = Set()
  private var pathPrefix: Set[String] = Set()
  private var pathPattern: Set[String] = Set()
  private var mimeType: Set[String] = Set()
  def add(scheme : String, 
	    				host : String, 
	    				port : String, 
	    				path : String, 
	    				pathPrefix : String, 
	    				pathPattern : String, 
	    				mimeType : String) = {
    if(scheme!= null){
      this.scheme +=scheme
    }
    if(host!= null){
      this.host +=host
    }
    if(port!= null){
      this.port +=port
    }
    if(path!= null){
      this.path +=path
    }
    if(pathPrefix != null){
	    this.pathPrefix += pathPrefix
	}
	if(pathPattern != null){
	    this.pathPattern += pathPattern
	}
	if(mimeType != null){
	    this.mimeType += mimeType
	}
  }
  
  override def toString() = {"schemes= " + scheme + " host= " + host + " port= " + port + " path= " + path + " pathPrefix= " + pathPrefix + " pathPattern= " + pathPattern + " mimeType= " + mimeType}
}