package org.sireum.amandroid.android.parser

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
  def getIntentFilters(rName : ResourceUri) = intentFmap.getOrElse(rName, null)
  def getIntentFiltersActions(rName : ResourceUri) : Set[String] = {
    val intentFilterS: Set[IntentFilter] = getIntentFilters(rName)
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
    /**
     * checks if this filter can accept an intent with (action, categories, uriData, mType)
     */
	def isMatchWith(action:String, categories: Set[String], uriData:UriData, mType:String):Boolean = {
	  var actionTest = false
	  var categoryTest = false
	  var dataTest = false
	  if(action == null || hasAction(action)){
	    actionTest = true
	  }
	  
	  if(categories == null || categories.isEmpty || hasCategories(categories)){
	    categoryTest = true
	  }
	  
	  // note that in android there is some discrepancy regarding data and mType on the Intent side and the Intent Filter side
	  if(this.data.matchWith(uriData, mType))
	    dataTest = true
	  
	  actionTest && categoryTest && dataTest
	}
	
	def hasAction(action:String):Boolean = {
	  this.actions.contains(action)
	}
	def hasCategories(categories: Set[String]):Boolean = {
	  categories.subsetOf(this.categorys)
	}
	
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
		
  }
  
  def getActions() = IntentFilter.this.actions
  def getCategorys() = IntentFilter.this.categorys
  def getData() = IntentFilter.this.data
  def getHolder() = IntentFilter.this.holder
  
  override def toString() = "component: " + holder + " (actions: " + actions + " categorys: " + categorys + " datas: " + data + ")"
}

// A Data class represents all pieces of info associated with all <data> tags of a particular filter as declared in a manifest file 

class Data{
  private var schemes: Set[String] = Set()
  private var hosts: Set[String] = Set()  
  private var ports: Set[String] = Set()
  private var paths: Set[String] = Set()
  private var pathPrefixs: Set[String] = Set()
  private var pathPatterns: Set[String] = Set()
  private var mimeTypes: Set[String] = Set()
  // note that in android there is some discrepancy regarding data and mType on the Intent side compared to that on the Intent Filter side
  def matchWith(uriData:UriData, mType:String):Boolean = {
    var dataTest = false
    var typeTest = false
    if(this.schemes.isEmpty && uriData == null) // **** re-check this logic
      dataTest = true
    if(uriData != null && matchWith(uriData))  // **** re-check this logic
      dataTest = true
    if(this.mimeTypes.isEmpty && mType == null)
      typeTest = true
    else if(this.mimeTypes.contains(mType))
      typeTest = true
      
    dataTest && typeTest
  }
  def matchWith(uriData:UriData):Boolean = {
    val scheme = uriData.getScheme()
    val host = uriData.getHost()
    val port = uriData.getPort()
    val path = uriData.getPath()
    var schemeTest = false
    var hostTest = false 
    var portTest = false 
    var pathTest = false
    
    if(this.schemes.isEmpty && scheme == null) // we need to extend the matching logic to include many cases
      return true
    if(this.schemes.contains(scheme)){
        schemeTest = true
        if(this.hosts.isEmpty && host == null)
          return true
	    if(this.hosts.contains(host)){
	      hostTest = true
	      if(this.ports.isEmpty && port == null)
	        portTest = true
	      else if(this.ports.contains(port))
		    portTest = true
		  if(this.paths.isEmpty && path == null)
	        pathTest = true
	      else if(this.paths.contains(path))
		    pathTest = true
		  portTest && pathTest
	    }
	    else
	      false
	 }
    else
        false
  }
  def add(scheme : String, 
	    				host : String, 
	    				port : String, 
	    				path : String, 
	    				pathPrefix : String, 
	    				pathPattern : String, 
	    				mimeType : String) = {
    if(scheme!= null){
      this.schemes +=scheme
    }
    if(host!= null){
      this.hosts +=host
    }
    if(port!= null){
      this.ports +=port
    }
    if(path!= null){
      this.paths +=path
    }
    if(pathPrefix != null){
	    this.pathPrefixs += pathPrefix
	}
	if(pathPattern != null){
	    this.pathPatterns += pathPattern
	}
	if(mimeType != null){
	    this.mimeTypes += mimeType
	}
  }
  
  def addScheme(scheme : String) ={
    if(scheme!= null){
      this.schemes +=scheme
    }
  }
  
  def addHost(host : String) ={
    if(host!= null){
      this.hosts +=host
    }
  }
  
  def addPort(port : String) ={
    if(port!= null){
      this.ports +=port
    }
  }
   def addPath(path : String) ={
    if(path!= null){
      this.paths +=path
    }
  }
  def addType(mimeType : String) ={
    if(mimeType!= null){
      this.mimeTypes +=mimeType
    }
  }
  override def toString() = {"schemes= " + schemes + " host= " + hosts + " port= " + ports + " path= " + paths + " pathPrefix= " + pathPrefixs + " pathPattern= " + pathPatterns + " mimeType= " + mimeTypes}
}

// A UriData class represents all pieces of info associated with the mData field of a particular Intent instance

class UriData{
  private var scheme: String = null
  private var host: String = null 
  private var port: String = null
  private var path: String = null
  private var pathPrefix: String = null
  private var pathPattern: String = null
  

  def set(scheme : String, 
	    				host : String, 
	    				port : String, 
	    				path : String, 
	    				pathPrefix : String, 
	    				pathPattern : String
	    				) = {
    if(scheme!= null){
      this.scheme =scheme
    }
    if(host!= null){
      this.host =host
    }
    if(port!= null){
      this.port =port
    }
    if(path!= null){
      this.path =path
    }
    if(pathPrefix != null){
	    this.pathPrefix = pathPrefix
	}
	if(pathPattern != null){
	    this.pathPattern = pathPattern
	}
	
  }
  
  def setScheme(scheme : String) ={
    if(scheme!= null){
      this.scheme =scheme
    }
  }
  def getScheme() = this.scheme
  
  def setHost(host : String) ={
    if(host!= null){
      this.host =host
    }
  }
  def getHost() = this.host
  def setPort(port : String) ={
    if(port!= null){
      this.port =port
    }
  }
  def getPort() = this.port
  def setPath(path : String) ={
    if(path!= null){
      this.path =path
    }
  }
  def getPath() = this.path
  override def toString() = {"schemes= " + scheme + " host= " + host + " port= " + port + " path= " + path + " pathPrefix= " + pathPrefix + " pathPattern= " + pathPattern }
}