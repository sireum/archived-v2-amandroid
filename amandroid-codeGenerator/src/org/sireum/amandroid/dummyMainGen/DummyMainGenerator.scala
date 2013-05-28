package org.sireum.amandroid.dummyMainGen

import org.sireum.util._
import org.stringtemplate.v4.STGroupFile
import org.stringtemplate.v4.ST
import org.sireum.amandroid.variableGen.VariableGenerator
import java.util.ArrayList
import org.sireum.amandroid.AndroidSymbolResolver.AndroidLibInfoTables
import org.sireum.amandroid.util.SignatureParser
import org.sireum.amandroid.entryPointConstants.AndroidEntryPointConstants

class DummyMainGenerator {
  
  private var currentComponent : ResourceUri = ""
  private var androidClasses : Set[ResourceUri] = Set()
  /**
   * Map from record uri to list of callback procedure uri
   */
  private var callbackFunctions : Map[ResourceUri, MList[ResourceUri]] = Map()
  private var intCounter : String = ""
  private var conditionCounter : Int = 0
  private val template = new STGroupFile("org/sireum/amandroid/pilarCodeTemplate/PilarCode.stg")
  private val procDeclTemplate = template.getInstanceOf("ProcedureDecl")
  private val localVarsTemplate = template.getInstanceOf("LocalVars")
  private val bodyTemplate = template.getInstanceOf("Body")
  private val varGen = new VariableGenerator()
  private val localVars = new ArrayList[String]
  private val codeFragments = new ArrayList[CodeFragmentGenerator]
  private var androidLibInfoTables : AndroidLibInfoTables = null
  private var jumpFutureTable : Map[CodeFragmentGenerator, CodeFragmentGenerator] = Map()
  /**
   * Map from record resource uri to it's local variable
   */
  private var localVarsForClasses : Map[String, String] = Map()
  
  def setAndroidLibInfoTables(alit : AndroidLibInfoTables) = {
    this.androidLibInfoTables = alit
  }
  /**
	 * Registers a list of classes to be automatically scanned for Android
	 * lifecycle methods
	 * @param androidClasses The list of classes to be automatically scanned for
	 * Android lifecycle methods
	 */
  def setEntryPointClasses(androidClasses : Set[String]) = {
    this.androidClasses = androidClasses
  }
  
  def setCurrentComponent(clazzName : ResourceUri) = {
    this.currentComponent = clazzName
  }
  
  /**
	 * Sets the list of callback functions to be integrated into the Android
	 * lifecycle
	 * @param callbackFunctions The list of callback functions to be integrated
	 * into the Android lifecycle. This is a mapping from the Android element
	 * class (activity, service, etc.) to the list of callback methods for that
	 * element.
	 */
	def setCallbackFunctions(callbackFunctions : Map[String, MList[ResourceUri]]) {
		this.callbackFunctions = callbackFunctions
	}
  
	def generate() : String = {
	  generateInternal(List())
	}
  
  def generate(methods : List[ResourceUri]) = {
    generateInternal(methods)
  }
	
	def generateInternal(methods : List[ResourceUri]) : String = {
	  var classMap : Map[ResourceUri, MList[ResourceUri]] = Map()
	  classMap += (this.currentComponent -> mlistEmpty)
	  
	  procDeclTemplate.add("retTyp", "[|void|]")
	  procDeclTemplate.add("owner", this.currentComponent)
	  procDeclTemplate.add("procedureName", this.currentComponent.substring(0, this.currentComponent.length() - 2) + ".dummyMain|]")
	  procDeclTemplate.add("params", "")
	  
	  val intVar = template.getInstanceOf("LocalVar")
	  intCounter = varGen.generate("int")
	  intVar.add("typ", "[|int|]")
	  intVar.add("name", intCounter)
	  localVars.add(intVar.render())
	  val assignment = template.getInstanceOf("AssignmentStmt")
	  assignment.add("lhs", intCounter)
	  assignment.add("rhs", conditionCounter)
	  val outerStartFragment = new CodeFragmentGenerator
	  outerStartFragment.addLabel
	  outerStartFragment.setCode(assignment)
	  codeFragments.add(outerStartFragment)
	  classMap.foreach{
	    item =>
	      val rUri = androidLibInfoTables.getRecordUri(item._1)
	      val classStartFragment = new CodeFragmentGenerator
			  classStartFragment.addLabel
			  codeFragments.add(classStartFragment)
	      val entryExitFragment = new CodeFragmentGenerator
	      createIfStmt(entryExitFragment, classStartFragment)
	      val endClassFragment = new CodeFragmentGenerator
	      try{
	        var activity = false
	        var service = false
	        var broadcastReceiver = false
	        var contentProvider = false
	        var plain = false
	        val ancestors = androidLibInfoTables.getAncestors(rUri)
	        ancestors.foreach{
	          ancestor =>
	              if(ancestor.equals(AndroidEntryPointConstants.ACTIVITYCLASS)) activity = true
	              if(ancestor.equals(AndroidEntryPointConstants.SERVICECLASS)) service = true
	              if(ancestor.equals(AndroidEntryPointConstants.BROADCASTRECEIVERCLASS)) broadcastReceiver = true
	              if(ancestor.equals(AndroidEntryPointConstants.CONTENTPROVIDERCLASS)) contentProvider = true
	        }
	        if(!activity && !service && !broadcastReceiver && !contentProvider) plain = true
	        var instanceNeeded = activity || service || broadcastReceiver || contentProvider
	        var plainMethods: Map[String, ResourceUri] = Map()
	        if(!instanceNeeded || plain){
	          item._2.foreach{
	            method =>
	              if(!androidLibInfoTables.isStaticMethod(method)) instanceNeeded = true
	          }
	        }
	        if(instanceNeeded){
	          val newExp = template.getInstanceOf("NewExp")
					  newExp.add("name", item._1)
					  val va = varGen.generate(item._1)
					  localVarsForClasses += (rUri -> va)
					  val variable = template.getInstanceOf("LocalVar")
					  variable.add("typ", item._1)
					  variable.add("name", va)
					  localVars.add(variable.render())
					  val asmt = template.getInstanceOf("AssignmentStmt")
					  asmt.add("lhs", va)
					  asmt.add("rhs", newExp)
					  classStartFragment.setCode(asmt)
	          generateRecordConstructor(rUri, msetEmpty, classStartFragment)
	        }
	        val classLocalVar = localVarsForClasses(rUri)
	        
	        //now start to generate lifecycle for the four kinds of component
	        if(activity){
	          activityLifeCycleGenerator(item._2, rUri, endClassFragment, classLocalVar, classStartFragment)
	        }
	        if(service){
	          serviceLifeCycleGenerator(item._2, rUri, endClassFragment, classLocalVar, classStartFragment)
	        }
	        if(broadcastReceiver){
	          broadcastReceiverLifeCycleGenerator(item._2, rUri, endClassFragment, classLocalVar, classStartFragment)
	        }
	        if(contentProvider){
	          contentProviderLifeCycleGenerator(item._2, rUri, endClassFragment, classLocalVar, classStartFragment)
	        }
	        if(plain){
	          
	        }
	      } finally {
	        endClassFragment.addLabel
	      	codeFragments.add(endClassFragment)
	      	entryExitFragment.addLabel
	      	codeFragments.add(entryExitFragment)
	      }
	  }
	  val outerExitFragment = new CodeFragmentGenerator
    outerExitFragment.addLabel
    createIfStmt(outerStartFragment, outerExitFragment)
	  createReturnStmt("@void", outerExitFragment)
	  codeFragments.add(outerExitFragment)
	  localVarsTemplate.add("locals", localVars)
	  bodyTemplate.add("codeFragments", generateBody)
	  procDeclTemplate.add("localVars", localVarsTemplate.render())
	  procDeclTemplate.add("body", bodyTemplate.render())
	  procDeclTemplate.render()
	}
	
	private def generateBody() : ArrayList[String] = {
	  val body : ArrayList[String] = new ArrayList[String]
	  for(i <- 0 to codeFragments.size() - 1){
	    body.add(i, codeFragments.get(i).generate)
	  }
	  body
	}
	
	def generateRecordConstructor(rUri : ResourceUri, constructionStack : MSet[ResourceUri], codefg : CodeFragmentGenerator) : String = {
	  constructionStack.add(rUri)
	  val sigs = androidLibInfoTables.getProcedureSigsByRecordUri(rUri)
	  var cons : String = null
	  sigs.foreach{
	    sig =>
	      val pUri = androidLibInfoTables.getProcedureUriBySignature(sig)
	      if(androidLibInfoTables.isConstructor(pUri)) cons = sig
	  }
	  if(cons != null){
	    generateProcedureCall(cons, "direct", localVarsForClasses(rUri), constructionStack, codefg)
	  } else {
	    System.err.println("Warning, cannot find constructor for " + rUri)
	  }
	  cons
	}
	
	private def generateProcedureCall(pSig : ResourceUri, typ : String, localClassVar : String, constructionStack : MSet[ResourceUri], codefg : CodeFragmentGenerator) : Unit = {
	  val sigParser = new SignatureParser(pSig).getParamSig
    val paramNum = sigParser.getParameterNum
    val params = sigParser.getObjectParameters
    val retTyp = sigParser.getReturnTypeSignature
    var paramVars : Map[Int, ResourceUri] = Map()
    params.foreach{
      param =>
        val paramUri = androidLibInfoTables.getRecordUri(param._2)
        // to protect go into dead constructor create loop
        if(!constructionStack.contains(paramUri)){
          val newExp = template.getInstanceOf("NewExp")
				  newExp.add("name", param._2)
				  val va = varGen.generate(param._2)
				  localVarsForClasses += (paramUri -> va)
				  val variable = template.getInstanceOf("LocalVar")
				  variable.add("typ", param._2)
				  variable.add("name", va)
				  localVars.add(variable.render())
				  val asmt = template.getInstanceOf("AssignmentStmt")
				  asmt.add("lhs", va)
				  asmt.add("rhs", newExp.render())
				  codefg.setCode(asmt)
          paramVars += (param._1 -> va)
          generateRecordConstructor(paramUri, constructionStack, codefg)
        }
    }
    val invokeStmt = template.getInstanceOf("InvokeStmt")
    invokeStmt.add("funcName", getFuncNameFromProcedureSig(pSig))
    val finalParamVars : ArrayList[String] = new ArrayList[String]
    finalParamVars.add(0, localClassVar)
    for(i <- 0 to paramNum - 1){
      if(paramVars.contains(i)){
        finalParamVars.add(i + 1, paramVars(i))
      } else {
        finalParamVars.add(i + 1, "0")
      }
    }
    invokeStmt.add("params", finalParamVars)
    invokeStmt.add("sig", pSig)
    invokeStmt.add("typ", typ)
    codefg.setCode(invokeStmt)
	}
	
	private def getFuncNameFromProcedureSig(sig : String) : String = {
	  val strs = sig.substring(3, sig.indexOf(":"))
	  "[|" + strs.replaceAll("\\/", ":").replaceAll("&lt;", "<").replaceAll("&gt;", ">").replaceAll(";", "") + "|]"
	}
	
	/**
	 * generate the lifecycle for contentprovider component
	 * @param entryPoints The list of procedures to consider in this record
	 * @param rUri Current record resource uri
	 * @param endClassFragment
	 * @param classLocalVar Current record's local variable
	 * @param codefg Current code fragment
	 */
	private def contentProviderLifeCycleGenerator(entryPoints : MList[ResourceUri], rUri : ResourceUri, endClassFragment : CodeFragmentGenerator, classLocalVar : String, codefg : CodeFragmentGenerator) = {
		createIfStmt(endClassFragment, codefg)
		
		// 1. onCreate:
		val onCreateFragment = new CodeFragmentGenerator
	  onCreateFragment.addLabel
	  codeFragments.add(onCreateFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.CONTENTPROVIDER_ONCREATE, rUri, entryPoints, onCreateFragment)
	  
	  //all other entry points of this class can be called in arbitary order
	  val endWhileFragment = generateAllCallbacks(entryPoints, rUri, classLocalVar)
	  createIfStmt(onCreateFragment, endWhileFragment)
	}
	
	/**
	 * generate the lifecycle for broadcastreceiver component
	 * @param entryPoints The list of procedures to consider in this record
	 * @param rUri Current record resource uri
	 * @param endClassFragment
	 * @param classLocalVar Current record's local variable
	 * @param codefg Current code fragment
	 */
	private def broadcastReceiverLifeCycleGenerator(entryPoints : MList[ResourceUri], rUri : ResourceUri, endClassFragment : CodeFragmentGenerator, classLocalVar : String, codefg : CodeFragmentGenerator) = {
		createIfStmt(endClassFragment, codefg)
		
		// 1. onReceive:
		val onReceiveFragment = new CodeFragmentGenerator
	  onReceiveFragment.addLabel
	  codeFragments.add(onReceiveFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.BROADCAST_ONRECEIVE, rUri, entryPoints, onReceiveFragment)
	  
	  //all other entry points of this class can be called in arbitary order
	  val endWhileFragment = generateAllCallbacks(entryPoints, rUri, classLocalVar)
	  createIfStmt(onReceiveFragment, endWhileFragment)
	}
	
	/**
	 * generate the lifecycle for service component
	 * @param entryPoints The list of procedures to consider in this record
	 * @param rUri Current record resource uri
	 * @param endClassFragment
	 * @param classLocalVar Current record's local variable
	 * @param codefg Current code fragment
	 */
	private def serviceLifeCycleGenerator(entryPoints : MList[ResourceUri], rUri : ResourceUri, endClassFragment : CodeFragmentGenerator, classLocalVar : String, codefg : CodeFragmentGenerator) = {
		createIfStmt(endClassFragment, codefg)
		
		// 1. onCreate:
	  val onCreateFragment = new CodeFragmentGenerator
	  onCreateFragment.addLabel
	  codeFragments.add(onCreateFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.SERVICE_ONCREATE, rUri, entryPoints, onCreateFragment)
	  // service has two different lifecycles:
	  // lifecycle 1:
	  // 2. onStart:
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.SERVICE_ONSTART1, rUri, entryPoints, onCreateFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.SERVICE_ONSTART2, rUri, entryPoints, onCreateFragment)
	  //all other entry points of this class can be called in arbitary order
	  generateAllCallbacks(entryPoints, rUri, classLocalVar)
	  // lifecycle 1 end.
	  
	  // lifecycle 2:
	  // 2. onBind:
	  val onBindFragment = new CodeFragmentGenerator
	  onBindFragment.addLabel
	  codeFragments.add(onBindFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.SERVICE_ONBIND, rUri, entryPoints, onBindFragment)
	  val beforemethodsFragment = new CodeFragmentGenerator
	  beforemethodsFragment.addLabel
	  codeFragments.add(beforemethodsFragment)
	  //all other entry points of this class can be called in arbitary order
	  generateAllCallbacks(entryPoints, rUri, classLocalVar)
	  
	  // 3. onRebind:
	  val onRebindFragment = new CodeFragmentGenerator
	  onRebindFragment.addLabel
	  codeFragments.add(onRebindFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.SERVICE_ONREBIND, rUri, entryPoints, onRebindFragment)
	  createIfStmt(beforemethodsFragment, onRebindFragment)
	  
	  // 4. onUnbind:
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.SERVICE_ONUNBIND, rUri, entryPoints, onRebindFragment)
	  createIfStmt(onBindFragment, onRebindFragment)
	  // lifecycle 2 end.
	  
	  // 3 | 5. onDestory:
	  val onDestoryFragment = new CodeFragmentGenerator
	  onDestoryFragment.addLabel
	  codeFragments.add(onDestoryFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.SERVICE_ONDESTROY, rUri, entryPoints, onDestoryFragment)
	  // either begin or end or next class:
	  createIfStmt(onCreateFragment, onDestoryFragment)
	  createIfStmt(endClassFragment, onDestoryFragment)
	}
	  
	/**
	 * generate the lifecycle for activity component
	 * @param entryPoints The list of procedures to consider in this record
	 * @param rUri Current record resource uri
	 * @param endClassFragment
	 * @param classLocalVar Current record's local variable
	 * @param codefg Current code fragment
	 */
	private def activityLifeCycleGenerator(entryPoints : MList[ResourceUri], rUri : ResourceUri, endClassFragment : CodeFragmentGenerator, classLocalVar : String, codefg : CodeFragmentGenerator) = {
	  createIfStmt(endClassFragment, codefg)
	  
	  // 1. onCreate:
	  val onCreateFragment = new CodeFragmentGenerator
	  onCreateFragment.addLabel
	  codeFragments.add(onCreateFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONCREATE, rUri, entryPoints, onCreateFragment)
	  // 2. onStart:
	  val onStartFragment = new CodeFragmentGenerator
	  onStartFragment.addLabel
	  codeFragments.add(onStartFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONSTART, rUri, entryPoints, onStartFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONRESTOREINSTANCESTATE, rUri, entryPoints, onStartFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONPOSTCREATE, rUri, entryPoints, onStartFragment)
	  // 3. onResume:
	  val onResumeFragment = new CodeFragmentGenerator
	  onResumeFragment.addLabel
	  codeFragments.add(onResumeFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONRESUME, rUri, entryPoints, onResumeFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONPOSTRESUME, rUri, entryPoints, onResumeFragment)
	  
	  // all other entry points of this activity
	  generateAllCallbacks(entryPoints, rUri, classLocalVar)
	  // 4. onPause:
	  val onPauseFragment = new CodeFragmentGenerator
	  onPauseFragment.addLabel
	  codeFragments.add(onPauseFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONPAUSE, rUri, entryPoints, onPauseFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONCREATEDESCRIPTION, rUri, entryPoints, onPauseFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONSAVEINSTANCESTATE, rUri, entryPoints, onPauseFragment)
	  // goto onDestory, onRestart or onCreate:
	  val onStopFragment = new CodeFragmentGenerator
	  createIfStmt(onStopFragment, onPauseFragment)
	  createIfStmt(onResumeFragment, onPauseFragment)
	  createIfStmt(onCreateFragment, onPauseFragment)
	  
	  // 5. onStop:
	  onStopFragment.addLabel
	  codeFragments.add(onStopFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONSTOP, rUri, entryPoints, onStopFragment)
	  //goto onDestory, onRestart or onCreate:
	  val onDestoryFragment = new CodeFragmentGenerator
	  val onRestartFragment = new CodeFragmentGenerator
	  createIfStmt(onDestoryFragment, onStopFragment)
	  createIfStmt(onRestartFragment, onStopFragment)
	  createIfStmt(onCreateFragment, onStopFragment)
	  
	  // 6. onRestart:
	  onRestartFragment.addLabel
	  codeFragments.add(onRestartFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONRESTART, rUri, entryPoints, onRestartFragment)
	  if(onStartFragment != null){
	    createGotoStmt(onStartFragment, onRestartFragment)
	  }
	  
	  // 7. onDestory:
	  onDestoryFragment.addLabel
	  codeFragments.add(onDestoryFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONDESTROY, rUri, entryPoints, onDestoryFragment)
	  
	  createIfStmt(endClassFragment, onDestoryFragment)
	}
	
	private def generateAllCallbacks(entryPoints : MList[ResourceUri], rUri : ResourceUri, classLocalVar : String) : CodeFragmentGenerator = {
	  val startWhileFragment = new CodeFragmentGenerator
	  startWhileFragment.addLabel
	  codeFragments.add(startWhileFragment)
	  val endWhileFragment = new CodeFragmentGenerator
	  val procedureUris = androidLibInfoTables.getProcedureUrisByRecordUri(rUri)
	  import scala.collection.JavaConversions._
	  for(currentProcedureUri <- procedureUris){
	    if(entryPoints.contains(currentProcedureUri)){ 
	      val pSig = androidLibInfoTables.getProcedureSignatureByUri(currentProcedureUri)
	      if(!AndroidEntryPointConstants.getActivityLifecycleMethods.contains(androidLibInfoTables.getSubSignature(pSig))){
		      val thenStmtFragment = new CodeFragmentGenerator
		      createIfStmt(thenStmtFragment, startWhileFragment)
		      val elseStmtFragment = new CodeFragmentGenerator
		      createGotoStmt(elseStmtFragment, startWhileFragment)
		      thenStmtFragment.addLabel
		      codeFragments.add(thenStmtFragment)
		      generateProcedureCall(pSig, "virtual", classLocalVar, msetEmpty + rUri, thenStmtFragment)
		      elseStmtFragment.addLabel
		      codeFragments.add(elseStmtFragment)
	      }
	    }
	  }
	  val callBackFragment = new CodeFragmentGenerator
	  callBackFragment.addLabel
	  codeFragments.add(callBackFragment)
	  addCallbackProcedures(rUri, classLocalVar, callBackFragment)
	  endWhileFragment.addLabel
	  codeFragments.add(endWhileFragment)
	  createIfStmt(startWhileFragment, endWhileFragment)
	  endWhileFragment
	}
	
	private def generateCallToAllCallbacks(callbackRecordUri : ResourceUri, callbackProcedureUris : List[ResourceUri], classLocalVar : String, codefg : CodeFragmentGenerator) = {
	  callbackProcedureUris.foreach{
	    callbackProcedureUri =>
	      val pSig = androidLibInfoTables.getProcedureSignatureByUri(callbackProcedureUri)
	      val thenStmtFragment = new CodeFragmentGenerator
	      createIfStmt(thenStmtFragment, codefg)
	      val elseStmtFragment = new CodeFragmentGenerator
	      createGotoStmt(elseStmtFragment, codefg)
	      thenStmtFragment.addLabel
	      codeFragments.add(thenStmtFragment)
	      generateProcedureCall(pSig, "virtual", classLocalVar, msetEmpty + callbackRecordUri, thenStmtFragment)
	      elseStmtFragment.addLabel
	      codeFragments.add(elseStmtFragment)
	  }
	}
	
	private def searchAndBuildProcedureCall(subsignature : String, rUri : ResourceUri, entryPoints : MList[String], codefg : CodeFragmentGenerator) = {
	  val pUri = androidLibInfoTables.findProcedureUri(rUri, subsignature)
	  if(pUri != null){
	    entryPoints -= pUri
	    assert(androidLibInfoTables.isStaticMethod(pUri) || localVarsForClasses(rUri) != null)
	    generateProcedureCall(androidLibInfoTables.getProcedureSignatureByUri(pUri), "virtual", localVarsForClasses(rUri), msetEmpty + rUri, codefg)
	  } else {
	    System.err.println("Could not find Android entry point procedure: " + subsignature)
	    null
	  }
	}
	
	/**
	 * Generates invocation statements for all callback methods which need to be invoded during the give record's run cycle.
	 * @param rUri Current record resource uri which under process
	 * @param classLocalVar The local variable fro current record
	 */
	private def addCallbackProcedures(rUri : ResourceUri, parentClassLocalVar : String, codefg : CodeFragmentGenerator) : Unit = {
	  if(!this.callbackFunctions.contains(rUri)) return
	  val callbackRecords : Map[ResourceUri, List[ResourceUri]] = 
	    this.callbackFunctions(rUri).map{
		    case (pUri) => 
		      val pSig = androidLibInfoTables.getProcedureSignatureByUri(pUri)
		      val theRecordUri = androidLibInfoTables.getRecordUri(new SignatureParser(pSig).getRecordName)
		      val theProcedureUri = androidLibInfoTables.findProcedureUri(theRecordUri, androidLibInfoTables.getSubSignature(pSig))
		      if(theProcedureUri == null){
		        System.err.println("Could not find callback method " + pSig)
		        (theRecordUri, List())
		      } else {
		        (theRecordUri, List(theProcedureUri))
		      }
		  }.toMap
		callbackRecords.foreach{
		  case(callbackRUri, callbackPUris) =>
		    val classLocalVar : String =
		      if(isCompatible(rUri, callbackRUri)) parentClassLocalVar
		      // create a new instance of this class
		      else generateRecordConstructor(callbackRUri, msetEmpty + rUri, codefg)
		    if(classLocalVar != null){
		      // build the calls to all callback procedures in this record
		      generateCallToAllCallbacks(callbackRUri, callbackPUris, classLocalVar, codefg)
		    } else {
		      System.err.println("Constructor cannot be generated for callbck class " + callbackRUri)
		    }
		}
	}
	
	private def isCompatible(actual : ResourceUri, expected : ResourceUri) : Boolean = {
	  val allPossible = androidLibInfoTables.getAncestors(actual) + actual
	  allPossible.filter(p => if(p.equals(expected))true else false)
	  allPossible.isEmpty
	}
	
	private def createIfStmt(targetfg : CodeFragmentGenerator, codefg : CodeFragmentGenerator) = {
	  val target = targetfg.getLabel
	  if(target != null){
	    val condExp = template.getInstanceOf("CondExp")
      condExp.add("lhs", intCounter)
      condExp.add("rhs", conditionCounter)
//      conditionCounter += 1
      val ifStmt = template.getInstanceOf("IfStmt")
      ifStmt.add("cond", condExp)
      ifStmt.add("label", target)
      codefg.setCode(ifStmt)
	  }
	}
	
	private def createGotoStmt(targetfg : CodeFragmentGenerator, codefg : CodeFragmentGenerator) = {
	  val target = targetfg.getLabel
	  if(target != null){
      val gotoStmt = template.getInstanceOf("GotoStmt")
      gotoStmt.add("label", target)
      codefg.setCode(gotoStmt)
	  }
	}
	
	private def createReturnStmt(variable : String, codefg : CodeFragmentGenerator) = {
	  val returnStmt = template.getInstanceOf("ReturnStmt")
      returnStmt.add("variable", variable)
      codefg.setCode(returnStmt)
	}
	
	private class CodeFragmentGenerator {
	  private val codeFragment = template.getInstanceOf("CodeFragment")
	  private val codes : ArrayList[ST] = new ArrayList[ST]
	  private var label = template.getInstanceOf("Label")
	  
	  def addLabel() = {
	    label.add("num", conditionCounter)
	    codeFragment.add("label", label)
	    conditionCounter += 1
	  }
	  def getLabel() : ST = label
	  def setCode(code : ST) = {
	    codes.add(code)
	  }
	  def generate() : String = {
	    val finalCodes = new ArrayList[ST]
	    for(i <- 0 to codes.size - 1){
	      val code = template.getInstanceOf("Code")
	      code.add("code", codes.get(i))
	      finalCodes.add(i, code)
	    }
	    codeFragment.add("codes", finalCodes)
	    codeFragment.render()
	  }
	}
	
}
