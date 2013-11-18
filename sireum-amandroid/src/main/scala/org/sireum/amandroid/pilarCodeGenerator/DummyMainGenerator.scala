package org.sireum.amandroid.pilarCodeGenerator

import org.sireum.util._
import org.stringtemplate.v4.STGroupFile
import org.stringtemplate.v4.ST
import java.util.ArrayList
import org.sireum.amandroid.util.SignatureParser
import java.util.Arrays
import org.sireum.amandroid.AmandroidRecord
import org.sireum.amandroid.AmandroidProcedure
import org.sireum.amandroid.util.StringFormConverter
import org.sireum.amandroid.Center
import org.sireum.amandroid.AmandroidResolver
import org.sireum.amandroid.util.SignatureParser
import org.sireum.amandroid.MessageCenter._
import org.sireum.amandroid.NormalType
import com.google.common.io.CharStreams
import java.io.InputStreamReader
import org.sireum.amandroid.util.MyFileUtil

class DummyMainGenerator {
  private var currentComponent : String = null
  private var androidClasses : Set[String] = Set()
  /**
   * Map from record to list of callback procedure
   */
  private var callbackFunctions : Map[String, Set[String]] = Map()
  private var conditionCounter : Int = 0
  private var codeCounter : Int = 0
  private val template = new STGroupFile("org/sireum/amandroid/resources/pilarCodeGenerator/PilarCode.stg")
  private val procDeclTemplate = template.getInstanceOf("ProcedureDecl")
  private val localVarsTemplate = template.getInstanceOf("LocalVars")
  private val bodyTemplate = template.getInstanceOf("Body")
  private val varGen = new VariableGenerator()
  private val localVars = new ArrayList[String]
  private val codeFragments = new ArrayList[CodeFragmentGenerator]
  
  /**
   * map from a record to it's substitute record
   */
  private var substituteRecordMap : IMap[String, String] = imapEmpty
  
  /**
   * Map from record to it's local variable
   */
  private var localVarsForClasses : Map[String, String] = Map()
  
  /**
   * Set of param's record name
   */
  private var paramRecords : Set[AmandroidRecord] = Set()

  /**
   * set the substituteRecordMap
   */
  def setSubstituteRecordMap(map : IMap[String, String]) = this.substituteRecordMap = map
  
  /**
	 * Registers a list of classes to be automatically scanned for Android
	 * lifecycle methods
	 * @param androidClasses The list of classes to be automatically scanned for
	 * Android lifecycle methods
	 */
  def setEntryPointClasses(androidClasses : Set[String]) = {
    this.androidClasses = androidClasses
  }
  
  def setCurrentComponent(clazz : String) = {
    this.currentComponent = clazz
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
	def setCallbackFunctions(callbackFunctions : Map[String, Set[String]]) {
		this.callbackFunctions = callbackFunctions
	}
  
	def generate() : AmandroidProcedure = {
	  generate(List())
	}
  
	/**
	 * generate dummy main with predefined methods list
	 */
  def generate(methods : List[String]) : AmandroidProcedure = {
    val recordName = this.currentComponent
    val procedureName = recordName.substring(0, recordName.length() - 2) + ".dummyMain|]"
	  val annotations = new ArrayList[ST]
	  val signature = procedureName.replaceAll("\\[\\|", "[|L").replaceAll("\\:", "/").replaceAll("\\.dummyMain", ";.dummyMain:()V")
	  initProcedureHead("[|void|]", procedureName, recordName, signature, "STATIC")
	  val code = generateInternal(List())
    msg_normal("dummyMain code:\n" + code)
    AmandroidResolver.resolveProcedureCode(signature, code)
  }
  
  def generateWithParam(params : List[String]) : AmandroidProcedure = {
    val recordName = this.currentComponent
    val procedureName = recordName.substring(0, recordName.length() - 2) + ".dummyMain|]"
	  val annotations = new ArrayList[ST]
    var parSigStr : String = ""
    params.indices.foreach{i => parSigStr += params(i).replaceAll("\\[\\|", "L").replaceAll("\\:", "/").replaceAll("\\|\\]", ";")}
	  val signature = procedureName.replaceAll("\\[\\|", "[|L").replaceAll("\\:", "/").replaceAll("\\.dummyMain", ";.dummyMain:("+parSigStr+")V")
	  initProcedureHead("[|void|]", procedureName, recordName, signature, "STATIC")
    val paramArray = new ArrayList[ST]
    params.indices.foreach{
      i =>
        val paramVar = template.getInstanceOf("ParamVar")
			  val p = varGen.generate(params(i))
			  localVarsForClasses += (params(i) -> p)
			  this.paramRecords += Center.resolveRecord(params(i), Center.ResolveLevel.BODIES)
			  paramVar.add("typ", params(i))
			  paramVar.add("name", p)
			  val annot = generateExpAnnotation("type", List("object"))
			  paramVar.add("annotations", new ArrayList[ST](Arrays.asList(annot)))
			  paramArray.add(i, paramVar)
    }
    procDeclTemplate.add("params", paramArray)
    val code = generateInternal(List())
    msg_critical("dummyMain code:\n" + code)
    AmandroidResolver.resolveProcedureCode(signature, code)
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
	
	def generateInternal(procedures : List[String]) : String = {
	  val classMap : MMap[String, MList[String]] = mmapEmpty
	  procedures.map{
	    procedure => 
	      val record = StringFormConverter.getRecordNameFromProcedureSignature(procedure)
	      classMap.getOrElseUpdate(record, mlistEmpty) += procedure
	  }
	  if(!classMap.contains(this.currentComponent))
	  	classMap.getOrElseUpdate(this.currentComponent, mlistEmpty)
	  val intVar = template.getInstanceOf("LocalVar")
	  val outerStartFragment = new CodeFragmentGenerator
	  outerStartFragment.addLabel
	  codeFragments.add(outerStartFragment)
	  classMap.foreach{
	    item =>
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
	        val currentRecord = Center.resolveRecord(item._1, Center.ResolveLevel.BODIES)
	        val ancestors = Center.getRecordHierarchy.getAllSuperClassesOfIncluding(currentRecord)
	        ancestors.foreach{
	          ancestor =>
	            val recName = ancestor.getName
              if(recName.equals(AndroidEntryPointConstants.ACTIVITY_CLASS)) activity = true
              if(recName.equals(AndroidEntryPointConstants.SERVICE_CLASS)) service = true
              if(recName.equals(AndroidEntryPointConstants.BROADCAST_RECEIVER_CLASS)) broadcastReceiver = true
              if(recName.equals(AndroidEntryPointConstants.CONTENT_PROVIDER_CLASS)) contentProvider = true
	        }
	        if(!activity && !service && !broadcastReceiver && !contentProvider) plain = true
	        var instanceNeeded = activity || service || broadcastReceiver || contentProvider
	        //How this part work? item._2 always empty!
	        var plainMethods: Map[String, AmandroidProcedure] = Map()
	        if(!instanceNeeded || plain){
	          item._2.foreach{
	            procSig =>
	              Center.getProcedure(procSig) match{
	                case Some(p) =>
		                plainMethods += (procSig -> p)
		                if(!p.isStatic) instanceNeeded = true
	                case None =>
	                  val recordName = StringFormConverter.getRecordNameFromProcedureSignature(procSig)
	                  if(!Center.containsRecord(recordName)) err_msg_normal("Record for entry point " + recordName + " not found, skipping")
	                  else{
	                    Center.getProcedure(procSig) match{
	                      case Some(p) => 
	                        plainMethods += (procSig -> p)
	                        if(!p.isStatic) instanceNeeded = true
	                      case None => err_msg_normal("Procedure for entry point " + procSig + " not found, skipping")
	                    }
	                  }
	              }
	          }
	        }
	        if(instanceNeeded){
	          val va = generateInstanceCreation(currentRecord.getName, classStartFragment)
	          this.localVarsForClasses += (currentRecord.getName -> va)
	          generateRecordConstructor(currentRecord, msetEmpty, classStartFragment)
	        }
	        val classLocalVar = localVarsForClasses.getOrElse(currentRecord.getName, null)
	        
	        //now start to generate lifecycle for the four kinds of component
	        if(activity){
	          activityLifeCycleGenerator(item._2, currentRecord, endClassFragment, classLocalVar, classStartFragment)
	        }
	        if(service){
	          serviceLifeCycleGenerator(item._2, currentRecord, endClassFragment, classLocalVar, classStartFragment)
	        }
	        if(broadcastReceiver){
	          broadcastReceiverLifeCycleGenerator(item._2, currentRecord, endClassFragment, classLocalVar, classStartFragment)
	        }
	        if(contentProvider){
	          contentProviderLifeCycleGenerator(item._2, currentRecord, endClassFragment, classLocalVar, classStartFragment)
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
	  val rhs =
		  if(recordName == "[|java:lang:String|]"){
		    val stringAnnot = generateExpAnnotation("type", List("object"))
		    "\"\" " + stringAnnot.render() 
		  } else {
			  val newExp = template.getInstanceOf("NewExp")
			  newExp.add("name", recordName)
			  newExp.render()
		  }
	  val va = varGen.generate(recordName)
	  val variable = template.getInstanceOf("LocalVar")
	  variable.add("typ", recordName)
	  variable.add("name", va)
	  localVars.add(variable.render())
	  val asmt = template.getInstanceOf("AssignmentStmt")
	  asmt.add("lhs", va)
	  asmt.add("rhs", rhs)
	  codefg.setCode(asmt)
	  va
	}

	
	def generateRecordConstructor(r : AmandroidRecord, constructionStack : MSet[AmandroidRecord], codefg : CodeFragmentGenerator) : String = {
	  constructionStack.add(r)
	  val ps = r.getProcedures
	  var cons : String = null
	  val conProcs = ps.filter(p => p.isConstructor && !p.isStatic && !p.getParamTypes.contains(NormalType("[|java:lang:Class|]", 0)))
	  if(!conProcs.isEmpty){
	    val p = conProcs.minBy(_.getParamTypes.size)
	  	cons = p.getSignature
	  }
	  if(cons != null){
	    generateProcedureCall(cons, "direct", localVarsForClasses(r.getName), constructionStack, codefg)
	  } else {
	    err_msg_normal("Warning, cannot find constructor for " + r)
	  }
	  cons
	}
	
	
	private def generateProcedureCall(pSig : String, typ : String, localClassVar : String, constructionStack : MSet[AmandroidRecord], codefg : CodeFragmentGenerator) : Unit = {
	  val sigParser = new SignatureParser(pSig).getParamSig
    val paramNum = sigParser.getParameterNum
    val params = sigParser.getObjectParameters
    var paramVars : Map[Int, String] = Map()
    params.foreach{
	    case(i, param) =>
        var r = Center.resolveRecord(param.typ, Center.ResolveLevel.BODIES)
        val outterClassOpt = if(r.isInnerClass) Some(r.getOuterClass) else None
        if(!r.isConcrete){
          var substRecordName = this.substituteRecordMap.getOrElse(r.getName, null)
          if(substRecordName != null) r = Center.resolveRecord(substRecordName, Center.ResolveLevel.BODIES)
        }
        // to protect from going into dead constructor create loop
        if(!r.isConcrete){
          val va = varGen.generate(r.getName)
          localVarsForClasses += (r.getName -> va)
          paramVars += (i -> va)
          err_msg_normal("Cannot create valid constructer for " + r + ", because it is " + r.getAccessFlagString + " and cannot find substitute.")
        } else if(!constructionStack.contains(r)){
				  val va = generateInstanceCreation(r.getName, codefg)
				  localVarsForClasses += (r.getName -> va)
          paramVars += (i -> va)
          generateRecordConstructor(r, constructionStack, codefg)
        } else {
          paramVars += (i -> localVarsForClasses(r.getName))
        }
    }
    val invokeStmt = template.getInstanceOf("InvokeStmt")
    invokeStmt.add("funcName", StringFormConverter.getProcedureNameFromProcedureSignature(pSig))
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
	private def plainRecordGenerator(plainMethods: Map[String, AmandroidProcedure], endClassFragment : CodeFragmentGenerator, classLocalVar : String, codefg : CodeFragmentGenerator) = {
	  val beforeClassFragment = new CodeFragmentGenerator
	  beforeClassFragment.addLabel
	  codeFragments.add(beforeClassFragment)
	  plainMethods.foreach{
	    case (currentProcedureSig, currentProcedure) =>
	      if(currentProcedure.isStatic || classLocalVar != null){
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
	        generateProcedureCall(currentProcedureSig, "virtual", classLocalVar, msetEmpty, thenFragment)
	        createIfStmt(endClassFragment, thenFragment)
	        elseFragment.addLabel
	        codeFragments.add(elseFragment)
	        createIfStmt(beforeClassFragment, elseFragment)
	      } else {
	        err_msg_normal("Skipping procedure " + currentProcedure + " because we have no instance")
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
	private def asyncTaskLifeCycleGenerator(entryPoints : MList[ResourceUri], record : AmandroidRecord, endClassFragment : CodeFragmentGenerator, classLocalVar : String, codefg : CodeFragmentGenerator) = {
	  val constructionStack : MSet[AmandroidRecord] = msetEmpty ++ this.paramRecords
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
	private def contentProviderLifeCycleGenerator(entryPoints : MList[ResourceUri], record : AmandroidRecord, endClassFragment : CodeFragmentGenerator, classLocalVar : String, codefg : CodeFragmentGenerator) = {
	  val constructionStack : MSet[AmandroidRecord] = msetEmpty ++ this.paramRecords
		createIfStmt(endClassFragment, codefg)
		
		// 1. onCreate:
		val onCreateFragment = new CodeFragmentGenerator
	  onCreateFragment.addLabel
	  codeFragments.add(onCreateFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.CONTENTPROVIDER_ONCREATE, record, entryPoints, constructionStack, onCreateFragment)
	  
	  //all other entry points of this class can be called in arbitary order
	  val endWhileFragment = generateAllCallbacks(entryPoints, record, classLocalVar)
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
	private def broadcastReceiverLifeCycleGenerator(entryPoints : MList[ResourceUri], record : AmandroidRecord, endClassFragment : CodeFragmentGenerator, classLocalVar : String, codefg : CodeFragmentGenerator) = {
	  val constructionStack : MSet[AmandroidRecord] = msetEmpty ++ this.paramRecords
		createIfStmt(endClassFragment, codefg)
		
		// 1. onReceive:
		val onReceiveFragment = new CodeFragmentGenerator
	  onReceiveFragment.addLabel
	  codeFragments.add(onReceiveFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.BROADCAST_ONRECEIVE, record, entryPoints, constructionStack, onReceiveFragment)
	  
	  //all other entry points of this class can be called in arbitary order
	  val endWhileFragment = generateAllCallbacks(entryPoints, record, classLocalVar)
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
	private def serviceLifeCycleGenerator(entryPoints : MList[ResourceUri], record : AmandroidRecord, endClassFragment : CodeFragmentGenerator, classLocalVar : String, codefg : CodeFragmentGenerator) = {
	  val constructionStack : MSet[AmandroidRecord] = msetEmpty ++ this.paramRecords
		createIfStmt(endClassFragment, codefg)
		
	  val r = Center.resolveRecord("[|android:app:ContextImpl|]", Center.ResolveLevel.BODIES)
	  val va = generateInstanceCreation(r.getName, codefg)
	  localVarsForClasses += (r.getName -> va)
    generateRecordConstructor(r, constructionStack, codefg)
    createFieldSetStmt(localVarsForClasses(record.getName), "[|android:content:ContextWrapper.mBase|]", va, List("object"), codefg)
	  
	  
		// 1. onCreate:
	  val onCreateFragment = new CodeFragmentGenerator
	  onCreateFragment.addLabel
	  codeFragments.add(onCreateFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.SERVICE_ONCREATE, record, entryPoints, constructionStack, onCreateFragment)
	  // service has two different lifecycles:
	  // lifecycle 1:
	  // 2. onStart:
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.SERVICE_ONSTART1, record, entryPoints, constructionStack, onCreateFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.SERVICE_ONSTART2, record, entryPoints, constructionStack, onCreateFragment)
	  //all other entry points of this class can be called in arbitary order
	  generateAllCallbacks(entryPoints, record, classLocalVar)
	  // lifecycle 1 end.
	  
	  // lifecycle 2:
	  // 2. onBind:
	  val onBindFragment = new CodeFragmentGenerator
	  onBindFragment.addLabel
	  codeFragments.add(onBindFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.SERVICE_ONBIND, record, entryPoints, constructionStack, onBindFragment)
	  val beforemethodsFragment = new CodeFragmentGenerator
	  beforemethodsFragment.addLabel
	  codeFragments.add(beforemethodsFragment)
	  //all other entry points of this class can be called in arbitary order
	  generateAllCallbacks(entryPoints, record, classLocalVar)
	  
	  // 3. onRebind:
	  val onRebindFragment = new CodeFragmentGenerator
	  onRebindFragment.addLabel
	  codeFragments.add(onRebindFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.SERVICE_ONREBIND, record, entryPoints, constructionStack, onRebindFragment)
	  createIfStmt(beforemethodsFragment, onRebindFragment)
	  
	  // 4. onUnbind:
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.SERVICE_ONUNBIND, record, entryPoints, constructionStack, onRebindFragment)
	  createIfStmt(onBindFragment, onRebindFragment)
	  // lifecycle 2 end.
	  
	  // 3 | 5. onDestory:
	  val onDestoryFragment = new CodeFragmentGenerator
	  onDestoryFragment.addLabel
	  codeFragments.add(onDestoryFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.SERVICE_ONDESTROY, record, entryPoints, constructionStack, onDestoryFragment)
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
	private def activityLifeCycleGenerator(entryPoints : MList[ResourceUri], record : AmandroidRecord, endClassFragment : CodeFragmentGenerator, classLocalVar : String, codefg : CodeFragmentGenerator) = {
	  val constructionStack : MSet[AmandroidRecord] = msetEmpty ++ this.paramRecords
	  createIfStmt(endClassFragment, codefg)
	  val r = Center.resolveRecord("[|android:app:ContextImpl|]", Center.ResolveLevel.BODIES)
	  val va = generateInstanceCreation(r.getName, codefg)
	  localVarsForClasses += (r.getName -> va)
    generateRecordConstructor(r, constructionStack, codefg)
    createFieldSetStmt(localVarsForClasses(record.getName), "[|android:view:ContextThemeWrapper.mBase|]", va, List("object"), codefg)
    
	  generateProcedureCall(AndroidEntryPointConstants.ACTIVITY_SETINTENT_SIG, "virtual", localVarsForClasses(record.getName), constructionStack, codefg)
	  
	  // 1. onCreate:
	  val onCreateFragment = new CodeFragmentGenerator
	  onCreateFragment.addLabel
	  codeFragments.add(onCreateFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONCREATE, record, entryPoints, constructionStack, onCreateFragment)
	  // 2. onStart:
	  val onStartFragment = new CodeFragmentGenerator
	  onStartFragment.addLabel
	  codeFragments.add(onStartFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONSTART, record, entryPoints, constructionStack, onStartFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONRESTOREINSTANCESTATE, record, entryPoints, constructionStack, onStartFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONPOSTCREATE, record, entryPoints, constructionStack, onStartFragment)
	  // 3. onResume:
	  val onResumeFragment = new CodeFragmentGenerator
	  onResumeFragment.addLabel
	  codeFragments.add(onResumeFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONRESUME, record, entryPoints, constructionStack, onResumeFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONPOSTRESUME, record, entryPoints, constructionStack, onResumeFragment)
	  
	  // all other entry points of this activity
	  generateAllCallbacks(entryPoints, record, classLocalVar)
	  // 4. onPause:
	  val onPauseFragment = new CodeFragmentGenerator
	  onPauseFragment.addLabel
	  codeFragments.add(onPauseFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONPAUSE, record, entryPoints, constructionStack, onPauseFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONCREATEDESCRIPTION, record, entryPoints, constructionStack, onPauseFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONSAVEINSTANCESTATE, record, entryPoints, constructionStack, onPauseFragment)
	  // goto onDestory, onRestart or onCreate:
	  val onStopFragment = new CodeFragmentGenerator
	  createIfStmt(onStopFragment, onPauseFragment)
	  createIfStmt(onResumeFragment, onPauseFragment)
	  createIfStmt(onCreateFragment, onPauseFragment)
	  
	  // 5. onStop:
	  onStopFragment.addLabel
	  codeFragments.add(onStopFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONSTOP, record, entryPoints, constructionStack, onStopFragment)
	  //goto onDestory, onRestart or onCreate:
	  val onDestoryFragment = new CodeFragmentGenerator
	  val onRestartFragment = new CodeFragmentGenerator
	  createIfStmt(onDestoryFragment, onStopFragment)
	  createIfStmt(onRestartFragment, onStopFragment)
	  createIfStmt(onCreateFragment, onStopFragment)
	  
	  // 6. onRestart:
	  onRestartFragment.addLabel
	  codeFragments.add(onRestartFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONRESTART, record, entryPoints, constructionStack, onRestartFragment)
	  if(onStartFragment != null){
	    createGotoStmt(onStartFragment, onRestartFragment)
	  }
	  
	  // 7. onDestory:
	  onDestoryFragment.addLabel
	  codeFragments.add(onDestoryFragment)
	  searchAndBuildProcedureCall(AndroidEntryPointConstants.ACTIVITY_ONDESTROY, record, entryPoints, constructionStack, onDestoryFragment)
	  
	  createIfStmt(endClassFragment, onDestoryFragment)
	}
	
	private def generateAllCallbacks(entryPoints : MList[String], record : AmandroidRecord, classLocalVar : String) : CodeFragmentGenerator = {
	  val startWhileFragment = new CodeFragmentGenerator
	  startWhileFragment.addLabel
	  codeFragments.add(startWhileFragment)
	  val endWhileFragment = new CodeFragmentGenerator
	  val procedures = record.getProcedures
	  import scala.collection.JavaConversions._
	  for(currentProcedure <- procedures){
	    if(entryPoints.contains(currentProcedure.getSignature)){ 
	      val pSig = currentProcedure.getSignature
	      if(!AndroidEntryPointConstants.getActivityLifecycleMethods.contains(currentProcedure.getSubSignature)){
		      val thenStmtFragment = new CodeFragmentGenerator
		      createIfStmt(thenStmtFragment, startWhileFragment)
		      val elseStmtFragment = new CodeFragmentGenerator
		      createGotoStmt(elseStmtFragment, startWhileFragment)
		      thenStmtFragment.addLabel
		      codeFragments.add(thenStmtFragment)
		      generateProcedureCall(pSig, "virtual", classLocalVar, msetEmpty + record, thenStmtFragment)
		      elseStmtFragment.addLabel
		      codeFragments.add(elseStmtFragment)
	      }
	    }
	  }
	  val callBackFragment = new CodeFragmentGenerator
	  callBackFragment.addLabel
	  codeFragments.add(callBackFragment)
	  addCallbackProcedures(record, classLocalVar, callBackFragment)
	  endWhileFragment.addLabel
	  codeFragments.add(endWhileFragment)
	  createIfStmt(startWhileFragment, endWhileFragment)
	  endWhileFragment
	}
	
	private def generateCallToAllCallbacks(callbackRecord : AmandroidRecord, callbackProcedures : Set[AmandroidProcedure], classLocalVar : String, codefg : CodeFragmentGenerator) = {
	  var oneCallBackFragment = codefg
	  callbackProcedures.foreach{
	    callbackProcedure =>
	      val pSig = callbackProcedure.getSignature
	      val thenStmtFragment = new CodeFragmentGenerator
	      createIfStmt(thenStmtFragment, oneCallBackFragment)
	      val elseStmtFragment = new CodeFragmentGenerator
	      createGotoStmt(elseStmtFragment, oneCallBackFragment)
	      thenStmtFragment.addLabel
	      codeFragments.add(thenStmtFragment)
	      generateProcedureCall(pSig, "virtual", classLocalVar, msetEmpty + callbackRecord, thenStmtFragment)
	      elseStmtFragment.addLabel
	      codeFragments.add(elseStmtFragment)
	      oneCallBackFragment = new CodeFragmentGenerator
		    oneCallBackFragment.addLabel
		    codeFragments.add(oneCallBackFragment)
	  }
	}
	
	private def searchAndBuildProcedureCall(subsignature : String, record : AmandroidRecord, entryPoints : MList[String], constructionStack : MSet[AmandroidRecord], codefg : CodeFragmentGenerator) = {
	  val apopt = findProcedure(record, subsignature)
	  apopt match{
	    case Some(ap) =>
	      entryPoints -= ap.getSignature
		    assert(ap.isStatic || localVarsForClasses(record.getName) != null)
		    generateProcedureCall(ap.getSignature, "virtual", localVarsForClasses(record.getName), constructionStack, codefg)
	    case None =>
	      err_msg_normal("Could not find Android entry point procedure: " + subsignature)
	      null
	  }
	}
	
	/**
	 * Generates invocation statements for all callback methods which need to be invoked during the give record's run cycle.
	 * @param rUri Current record resource uri which under process
	 * @param classLocalVar The local variable fro current record
	 */
	private def addCallbackProcedures(record : AmandroidRecord, parentClassLocalVar : String, codefg : CodeFragmentGenerator) : Unit = {
	  if(!this.callbackFunctions.contains(record.getName)) return
	  var callbackRecords : Map[AmandroidRecord, ISet[AmandroidProcedure]] = Map()
    this.callbackFunctions(record.getName).map{
	    case (pSig) => 
	      val theRecord = Center.resolveRecord(StringFormConverter.getRecordNameFromProcedureSignature(pSig), Center.ResolveLevel.BODIES)
	      val theProcedure = findProcedure(theRecord, Center.getSubSigFromProcSig(pSig))
	      theProcedure match {
	        case Some(proc) =>
			      callbackRecords += (theRecord -> (callbackRecords.getOrElse(theRecord, isetEmpty) + proc))
	        case None =>
	          err_msg_normal("Could not find callback method " + pSig)
	      }
	      
	  }
	  var oneCallBackFragment = codefg
		callbackRecords.foreach{
		  case(callbackRecord, callbackProcedures) =>
		    val classLocalVar : String =
		      if(isCompatible(record, callbackRecord)) parentClassLocalVar
		      // create a new instance of this class
		      else{
			      val va = generateInstanceCreation(callbackRecord.getName, oneCallBackFragment)
		        this.localVarsForClasses += (callbackRecord.getName -> va)
		        generateRecordConstructor(callbackRecord, msetEmpty + record, oneCallBackFragment)
		        va
		      }
		    if(classLocalVar != null){
		      // build the calls to all callback procedures in this record
		      generateCallToAllCallbacks(callbackRecord, callbackProcedures, classLocalVar, oneCallBackFragment)
		    } else {
		      err_msg_normal("Constructor cannot be generated for callback class " + callbackRecord)
		    }
		    oneCallBackFragment = new CodeFragmentGenerator
		    oneCallBackFragment.addLabel
		    codeFragments.add(oneCallBackFragment)
		}
	}
	
	private def isCompatible(actual : AmandroidRecord, expected : AmandroidRecord) : Boolean = {
	  var act : AmandroidRecord = actual
	  while(act != null){
	    if(act.getName.equals(expected.getName))
	      return true
	    if(expected.isInterface)
	      act.getInterfaces.foreach{int => if(int.getName.equals(expected.getName)) return true}
	    if(!act.hasSuperClass)
	      act = null
	    else act = act.getSuperClass
	  }
	  false
	}
	
	private def createIfStmt(targetfg : CodeFragmentGenerator, codefg : CodeFragmentGenerator) = {
	  val target = targetfg.getLabel
	  if(target != null){
	    val condExp = template.getInstanceOf("CondExp")
      condExp.add("lhs", "RandomCoinToss")
      condExp.add("rhs", "head")
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
	
	private def createFieldSetStmt(base : String, field : String, rhs : String, annoTyps : List[String], codefg : CodeFragmentGenerator) = {
    val mBaseField = template.getInstanceOf("FieldAccessExp")
	  mBaseField.add("base", base)
	  mBaseField.add("field", field)
	  val asmt = template.getInstanceOf("AssignmentStmt")
	  asmt.add("lhs", mBaseField)
	  asmt.add("rhs", rhs)
	  val annos = generateExpAnnotation("type", annoTyps)
	  asmt.add("annotations", annos)
	  codefg.setCode(asmt)
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
	
	protected def findProcedure(currentRecord : AmandroidRecord, subSig : String) : Option[AmandroidProcedure] = {
	  if(currentRecord.declaresProcedure(subSig)) Some(currentRecord.getProcedure(subSig))
	  else if(currentRecord.hasSuperClass) findProcedure(currentRecord.getSuperClass, subSig)
	  else None
	}
	
}
