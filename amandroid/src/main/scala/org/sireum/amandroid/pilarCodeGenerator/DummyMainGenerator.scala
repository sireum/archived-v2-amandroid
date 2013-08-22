package org.sireum.amandroid.dummyMainGen

import org.sireum.util._
import org.stringtemplate.v4.STGroupFile
import org.stringtemplate.v4.ST
import java.util.ArrayList
import org.sireum.amandroid.util.SignatureParser
import java.util.Arrays
import org.sireum.amandroid.pilarCodeGenerator.VariableGenerator
import org.sireum.amandroid.symbolResolver.AndroidLibInfoTables
import org.sireum.amandroid.pilarCodeGenerator.AndroidEntryPointConstants

class DummyMainGenerator {
  
  private var currentComponent : String = null
  private var androidClasses : Set[ResourceUri] = Set()
  /**
   * Map from record uri to list of callback procedure uri
   */
  private var callbackFunctions : Map[ResourceUri, MSet[ResourceUri]] = Map()
  private var intCounter : String = null
  private var conditionCounter : Int = 0
  private var codeCounter : Int = 0
  private val template = new STGroupFile("org/sireum/amandroid/pilarCodeTemplate/PilarCode.stg")
  private val procDeclTemplate = template.getInstanceOf("ProcedureDecl")
  private val localVarsTemplate = template.getInstanceOf("LocalVars")
  private val bodyTemplate = template.getInstanceOf("Body")
  private val varGen = new VariableGenerator()
  private val localVars = new ArrayList[String]
  private val codeFragments = new ArrayList[CodeFragmentGenerator]
  private var androidLibInfoTables : AndroidLibInfoTables = null
  /**
   * Map from record name to it's local variable
   */
  private var localVarsForClasses : Map[String, String] = Map()
  /**
   * Set of param's record uri
   */
  private var paramRecordUris : Set[ResourceUri] = Set()
  
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
  
  def setCurrentComponent(clazzName : String) = {
    this.currentComponent = clazzName
  }
  
    
  def setCodeCounter(codeCtr : Int) = {
    this.codeCounter = codeCtr
  }
  
   def getCodeCounter():Int = {
    this.codeCounter
  }
  
  /**
	 * Sets the list of callback functions to be integrated into the Android
	 * lifecycle
	 * @param callbackFunctions The list of callback functions to be integrated
	 * into the Android lifecycle. This is a mapping from the Android element
	 * class (activity, service, etc.) to the list of callback methods for that
	 * element.
	 */
	def setCallbackFunctions(callbackFunctions : Map[String, MSet[ResourceUri]]) {
		this.callbackFunctions = callbackFunctions
	}
  
	def generate() : (String, String) = {
	  generate(List())
	}
  
	/**
	 * generate dummy main with predefined methods list
	 */
  def generate(methods : List[ResourceUri]) : (String, String) = {
    val procedureName = this.currentComponent.substring(0, this.currentComponent.length() - 2) + ".dummyMain|]"
	  val annotations = new ArrayList[ST]
	  val signature = procedureName.replaceAll("\\[\\|", "[|L").replaceAll("\\:", "/").replaceAll("\\.dummyMain", ";.dummyMain:()V")
	  initProcedureHead("[|void|]", procedureName, this.currentComponent, signature, "STATIC")
    (signature, generateInternal(methods))
  }
  
  def generateWithParam(params : List[String]) : (String, String) = {
    val procedureName = this.currentComponent.substring(0, this.currentComponent.length() - 2) + ".dummyMain|]"
	  val annotations = new ArrayList[ST]
    var parSigStr : String = ""
    params.indices.foreach{i => parSigStr += params(i).replaceAll("\\[\\|", "L").replaceAll("\\:", "/").replaceAll("\\|\\]", ";")}
	  val signature = procedureName.replaceAll("\\[\\|", "[|L").replaceAll("\\:", "/").replaceAll("\\.dummyMain", ";.dummyMain:("+parSigStr+")V")
	  initProcedureHead("[|void|]", procedureName, this.currentComponent, signature, "STATIC")
    val paramArray = new ArrayList[ST]
    params.indices.foreach{
      i =>
        val paramVar = template.getInstanceOf("ParamVar")
			  val p = varGen.generate(params(i))
			  localVarsForClasses += (androidLibInfoTables.getRecordUri(params(i)) -> p)
			  this.paramRecordUris += androidLibInfoTables.getRecordUri(params(i))
			  paramVar.add("typ", params(i))
			  paramVar.add("name", p)
			  val annot = generateParamAnnotation("type", List("object"))
			  paramVar.add("annotations", new ArrayList[ST](Arrays.asList(annot)))
			  paramArray.add(i, paramVar)
    }
    procDeclTemplate.add("params", paramArray)
    (signature, generateInternal(List()))
  }
  
  private def generateParamAnnotation(flag : String, params : List[String]) : ST = {
    val paramArray = new ArrayList[String]
    params.foreach(param => paramArray.add(param))
    val annot = template.getInstanceOf("annotationWithParam")
	  annot.add("flag", flag)
	  annot.add("params", paramArray)
  }
  
  private def generateExpAnnotation(flag : String, exps : List[String]) : ST = {
    val expArray = new ArrayList[String]
    exps.foreach(exp => expArray.add(exp))
    val annot = template.getInstanceOf("annotationWithExp")
	  annot.add("flag", flag)
	  annot.add("exps", expArray)
  }
  
  private def initProcedureHead(retTyp : String, procedureName : String, owner : String, signature : String, access : String) = {
	  procDeclTemplate.add("retTyp", retTyp)
	  procDeclTemplate.add("procedureName", procedureName)
	  val annotations = new ArrayList[ST]
	  annotations.add(generateExpAnnotation("owner", List(owner)))
	  annotations.add(generateExpAnnotation("signature", List(signature)))
	  annotations.add(generateExpAnnotation("Access", List(access)))
	  procDeclTemplate.add("annotations", annotations)
  }
	
	def generateInternal(procedureSigs : List[String]) : String = {
	  var classMap : Map[String, MList[String]] = Map()
	  procedureSigs.map{
	    case(procedureSig) => 
	      val recordName = androidLibInfoTables.getRecordNameFromProcedureSig(procedureSig)
	      if(!classMap.contains(recordName))
	          classMap += (recordName -> mlistEmpty)
	      classMap(recordName) += classMap(recordName) + procedureSig
	  }
	  if(!classMap.contains(this.currentComponent))
	  	classMap += (this.currentComponent -> mlistEmpty)
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
	        //How this part work? item._2 always empty!
	        var plainMethods: Map[String, ResourceUri] = Map()
	        if(!instanceNeeded || plain){
	          item._2.foreach{
	            methodSig =>
	              val methodUri = androidLibInfoTables.getProcedureUriBySignature(methodSig)
	              plainMethods += (methodSig -> methodUri)
	              if(!androidLibInfoTables.isStaticProcedure(methodUri)) instanceNeeded = true
	          }
	        }
	        if(instanceNeeded){
	          val va = generateInstanceCreation(item._1, classStartFragment)
	          this.localVarsForClasses += (rUri -> va)
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
	          plainRecordGenerator(plainMethods, endClassFragment, classLocalVar, classStartFragment)
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
	
	private def generateInstanceCreation(recordName : String, codefg : CodeFragmentGenerator) : String = {
	  val newExp = template.getInstanceOf("NewExp")
	  newExp.add("name", recordName)
	  val va = varGen.generate(recordName)
	  val variable = template.getInstanceOf("LocalVar")
	  variable.add("typ", recordName)
	  variable.add("name", va)
	  localVars.add(variable.render())
	  val asmt = template.getInstanceOf("AssignmentStmt")
	  asmt.add("lhs", va)
	  asmt.add("rhs", newExp)
	  codefg.setCode(asmt)
	  va
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
				  val va = generateInstanceCreation(param._2, codefg)
				  localVarsForClasses += (paramUri -> va)
          paramVars += (param._1 -> va)
          generateRecordConstructor(paramUri, constructionStack, codefg)
        } else {
          paramVars += (param._1 -> localVarsForClasses(paramUri))
        }
    }
    val invokeStmt = template.getInstanceOf("InvokeStmt")
    invokeStmt.add("funcName", androidLibInfoTables.getProcedureNameFromProcedureSig(pSig))
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
    val annotations = new ArrayList[ST]
	  annotations.add(generateExpAnnotation("signature", List(pSig)))
	  annotations.add(generateExpAnnotation("type", List(typ)))
    invokeStmt.add("annotations", annotations)
    codefg.setCode(invokeStmt)
	}
	
	/**
	 * Generate plain record methods calling sequence. Because we don't know
	 * the order of the custom statements, we assume that it can loop arbitrarily.
	 * 
	 */
	private def plainRecordGenerator(plainMethods: Map[String, ResourceUri], endClassFragment : CodeFragmentGenerator, classLocalVar : String, codefg : CodeFragmentGenerator) = {
	  val beforeClassFragment = new CodeFragmentGenerator
	  beforeClassFragment.addLabel
	  codeFragments.add(beforeClassFragment)
	  plainMethods.foreach{
	    case (currentProcedureSig, currentProcedureUri) =>
	      if(androidLibInfoTables.isStaticProcedure(currentProcedureUri) || classLocalVar != null){
	        val ifFragment = new CodeFragmentGenerator
	        val thenFragment = new CodeFragmentGenerator
	        ifFragment.addLabel
	        codeFragments.add(ifFragment)
	        createIfStmt(thenFragment, ifFragment)
	        val elseFragment = new CodeFragmentGenerator
	        val elseGotoFragment = new CodeFragmentGenerator
	        elseGotoFragment.addLabel
	        createGotoStmt(elseFragment, elseGotoFragment)
	        codeFragments.add(elseGotoFragment)
	        thenFragment.addLabel
	        codeFragments.add(thenFragment)
	        generateProcedureCall(currentProcedureSig, "virtual", classLocalVar, msetEmpty + currentProcedureUri, thenFragment)
	        createIfStmt(endClassFragment, thenFragment)
	        elseFragment.addLabel
	        codeFragments.add(elseFragment)
	        createIfStmt(beforeClassFragment, elseFragment)
	      } else {
	        System.err.println("Skipping procedure " + currentProcedureUri + " because we have no instance")
	      }
	  }
	}
	
	/**
	 * generate the lifecycle for AsyncTask component
	 * @param entryPoints The list of procedures to consider in this record
	 * @param rUri Current record resource uri
	 * @param endClassFragment
	 * @param classLocalVar Current record's local variable
	 * @param codefg Current code fragment
	 */
	private def asyncTaskLifeCycleGenerator(entryPoints : MList[ResourceUri], rUri : ResourceUri, endClassFragment : CodeFragmentGenerator, classLocalVar : String, codefg : CodeFragmentGenerator) = {
	  val constructionStack : MSet[ResourceUri] = msetEmpty ++ this.paramRecordUris
		createIfStmt(endClassFragment, codefg)
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
	  val constructionStack : MSet[ResourceUri] = msetEmpty ++ this.paramRecordUris
		createIfStmt(endClassFragment, codefg)
		
		// 1. onCreate:
		val onCreateFragment = new CodeFragmentGenerator
	  onCreateFragment.addLabel
	  codeFragments.add(onCreateFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.CONTENTPROVIDER_ONCREATE, rUri, entryPoints, constructionStack, onCreateFragment)
	  
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
	  val constructionStack : MSet[ResourceUri] = msetEmpty ++ this.paramRecordUris
		createIfStmt(endClassFragment, codefg)
		
		// 1. onReceive:
		val onReceiveFragment = new CodeFragmentGenerator
	  onReceiveFragment.addLabel
	  codeFragments.add(onReceiveFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.BROADCAST_ONRECEIVE, rUri, entryPoints, constructionStack, onReceiveFragment)
	  
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
	  val constructionStack : MSet[ResourceUri] = msetEmpty ++ this.paramRecordUris
		createIfStmt(endClassFragment, codefg)
		
		// 1. onCreate:
	  val onCreateFragment = new CodeFragmentGenerator
	  onCreateFragment.addLabel
	  codeFragments.add(onCreateFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.SERVICE_ONCREATE, rUri, entryPoints, constructionStack, onCreateFragment)
	  // service has two different lifecycles:
	  // lifecycle 1:
	  // 2. onStart:
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.SERVICE_ONSTART1, rUri, entryPoints, constructionStack, onCreateFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.SERVICE_ONSTART2, rUri, entryPoints, constructionStack, onCreateFragment)
	  //all other entry points of this class can be called in arbitary order
	  generateAllCallbacks(entryPoints, rUri, classLocalVar)
	  // lifecycle 1 end.
	  
	  // lifecycle 2:
	  // 2. onBind:
	  val onBindFragment = new CodeFragmentGenerator
	  onBindFragment.addLabel
	  codeFragments.add(onBindFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.SERVICE_ONBIND, rUri, entryPoints, constructionStack, onBindFragment)
	  val beforemethodsFragment = new CodeFragmentGenerator
	  beforemethodsFragment.addLabel
	  codeFragments.add(beforemethodsFragment)
	  //all other entry points of this class can be called in arbitary order
	  generateAllCallbacks(entryPoints, rUri, classLocalVar)
	  
	  // 3. onRebind:
	  val onRebindFragment = new CodeFragmentGenerator
	  onRebindFragment.addLabel
	  codeFragments.add(onRebindFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.SERVICE_ONREBIND, rUri, entryPoints, constructionStack, onRebindFragment)
	  createIfStmt(beforemethodsFragment, onRebindFragment)
	  
	  // 4. onUnbind:
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.SERVICE_ONUNBIND, rUri, entryPoints, constructionStack, onRebindFragment)
	  createIfStmt(onBindFragment, onRebindFragment)
	  // lifecycle 2 end.
	  
	  // 3 | 5. onDestory:
	  val onDestoryFragment = new CodeFragmentGenerator
	  onDestoryFragment.addLabel
	  codeFragments.add(onDestoryFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.SERVICE_ONDESTROY, rUri, entryPoints, constructionStack, onDestoryFragment)
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
	  val constructionStack : MSet[ResourceUri] = msetEmpty ++ this.paramRecordUris
	  createIfStmt(endClassFragment, codefg)
	  generateProcedureCall(AndroidEntryPointConstants.ACTIVITY_SETINTENT_SIG, "virtual", localVarsForClasses(rUri), constructionStack, codefg)
	  
	  // 1. onCreate:
	  val onCreateFragment = new CodeFragmentGenerator
	  onCreateFragment.addLabel
	  codeFragments.add(onCreateFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONCREATE, rUri, entryPoints, constructionStack, onCreateFragment)
	  // 2. onStart:
	  val onStartFragment = new CodeFragmentGenerator
	  onStartFragment.addLabel
	  codeFragments.add(onStartFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONSTART, rUri, entryPoints, constructionStack, onStartFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONRESTOREINSTANCESTATE, rUri, entryPoints, constructionStack, onStartFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONPOSTCREATE, rUri, entryPoints, constructionStack, onStartFragment)
	  // 3. onResume:
	  val onResumeFragment = new CodeFragmentGenerator
	  onResumeFragment.addLabel
	  codeFragments.add(onResumeFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONRESUME, rUri, entryPoints, constructionStack, onResumeFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONPOSTRESUME, rUri, entryPoints, constructionStack, onResumeFragment)
	  
	  // all other entry points of this activity
	  generateAllCallbacks(entryPoints, rUri, classLocalVar)
	  // 4. onPause:
	  val onPauseFragment = new CodeFragmentGenerator
	  onPauseFragment.addLabel
	  codeFragments.add(onPauseFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONPAUSE, rUri, entryPoints, constructionStack, onPauseFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONCREATEDESCRIPTION, rUri, entryPoints, constructionStack, onPauseFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONSAVEINSTANCESTATE, rUri, entryPoints, constructionStack, onPauseFragment)
	  // goto onDestory, onRestart or onCreate:
	  val onStopFragment = new CodeFragmentGenerator
	  createIfStmt(onStopFragment, onPauseFragment)
	  createIfStmt(onResumeFragment, onPauseFragment)
	  createIfStmt(onCreateFragment, onPauseFragment)
	  
	  // 5. onStop:
	  onStopFragment.addLabel
	  codeFragments.add(onStopFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONSTOP, rUri, entryPoints, constructionStack, onStopFragment)
	  //goto onDestory, onRestart or onCreate:
	  val onDestoryFragment = new CodeFragmentGenerator
	  val onRestartFragment = new CodeFragmentGenerator
	  createIfStmt(onDestoryFragment, onStopFragment)
	  createIfStmt(onRestartFragment, onStopFragment)
	  createIfStmt(onCreateFragment, onStopFragment)
	  
	  // 6. onRestart:
	  onRestartFragment.addLabel
	  codeFragments.add(onRestartFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONRESTART, rUri, entryPoints, constructionStack, onRestartFragment)
	  if(onStartFragment != null){
	    createGotoStmt(onStartFragment, onRestartFragment)
	  }
	  
	  // 7. onDestory:
	  onDestoryFragment.addLabel
	  codeFragments.add(onDestoryFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONDESTROY, rUri, entryPoints, constructionStack, onDestoryFragment)
	  
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
	
	private def generateCallToAllCallbacks(callbackRecordUri : ResourceUri, callbackProcedureUris : Set[ResourceUri], classLocalVar : String, codefg : CodeFragmentGenerator) = {
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
	
	private def searchAndBuildProcedureCall(subsignature : String, rUri : ResourceUri, entryPoints : MList[String], constructionStack : MSet[ResourceUri], codefg : CodeFragmentGenerator) = {
	  val pUri = androidLibInfoTables.findProcedureUri(rUri, subsignature)
	  if(pUri != null){
	    entryPoints -= pUri
	    assert(androidLibInfoTables.isStaticProcedure(pUri) || localVarsForClasses(rUri) != null)
	    generateProcedureCall(androidLibInfoTables.getProcedureSignatureByUri(pUri), "virtual", localVarsForClasses(rUri), constructionStack, codefg)
	  } else {
	    System.err.println("Could not find Android entry point procedure: " + subsignature)
	    null
	  }
	}
	
	/**
	 * Generates invocation statements for all callback methods which need to be invoked during the give record's run cycle.
	 * @param rUri Current record resource uri which under process
	 * @param classLocalVar The local variable fro current record
	 */
	private def addCallbackProcedures(rUri : ResourceUri, parentClassLocalVar : String, codefg : CodeFragmentGenerator) : Unit = {
	  if(!this.callbackFunctions.contains(rUri)) return
	  var callbackRecords : Map[ResourceUri, MSet[ResourceUri]] = Map()
    this.callbackFunctions(rUri).map{
	    case (pUri) => 
	      val pSig = androidLibInfoTables.getProcedureSignatureByUri(pUri)
	      val theRecordUri = androidLibInfoTables.getRecordUri(new SignatureParser(pSig).getRecordName)
	      if(!callbackRecords.contains(theRecordUri))
	      	callbackRecords += (theRecordUri -> msetEmpty)
	      val theProcedureUri = androidLibInfoTables.findProcedureUri(theRecordUri, androidLibInfoTables.getSubSignature(pSig))
	      if(theProcedureUri == null){
	        System.err.println("Could not find callback method " + pSig)
	      } else {
	        callbackRecords(theRecordUri) += theProcedureUri
	      }
	  }
		callbackRecords.foreach{
		  case(callbackRUri, callbackPUris) =>
		    val classLocalVar : String =
		      if(isCompatible(rUri, callbackRUri)) parentClassLocalVar
		      // create a new instance of this class
		      else{
		        val recordName = androidLibInfoTables.getRecordName(callbackRUri)
			      val va = generateInstanceCreation(recordName, codefg)
		        this.localVarsForClasses += (callbackRUri -> va)
		        generateRecordConstructor(callbackRUri, msetEmpty + rUri, codefg)
		        va
		      }
		    if(classLocalVar != null){
		      // build the calls to all callback procedures in this record
		      generateCallToAllCallbacks(callbackRUri, callbackPUris.toSet, classLocalVar, codefg)
		    } else {
		      System.err.println("Constructor cannot be generated for callback class " + callbackRUri)
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
	      code.add("num", codeCounter)
	      codeCounter += 1
	      code.add("code", codes.get(i))
	      finalCodes.add(i, code)
	    }
	    codeFragment.add("codes", finalCodes)
	    codeFragment.render()
	  }
	}
	
}
