package org.sireum.amandroid.android.pilarCodeGenerator

import org.sireum.util._
import org.stringtemplate.v4.STGroupFile
import org.stringtemplate.v4.ST
import java.util.ArrayList
import org.sireum.jawa.util.SignatureParser
import java.util.Arrays
import org.sireum.jawa.JawaRecord
import org.sireum.jawa.JawaProcedure
import org.sireum.jawa.util.StringFormConverter
import org.sireum.jawa.Center
import org.sireum.jawa.JawaResolver
import org.sireum.jawa.util.SignatureParser
import org.sireum.jawa.MessageCenter._
import org.sireum.jawa.NormalType
import org.sireum.jawa.pilarCodeGenerator.VariableGenerator
import org.sireum.jawa.pilarCodeGenerator.ProcedureGenerator

class AndroidEnvironmentGenerator extends ProcedureGenerator {
	
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
	        var plainMethods: Map[String, JawaProcedure] = Map()
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
	
	
	
	/**
	 * Generate plain record methods calling sequence. Because we don't know
	 * the order of the custom statements, we assume that it can loop arbitrarily.
	 * 
	 */
	private def plainRecordGenerator(plainMethods: Map[String, JawaProcedure], endClassFragment : CodeFragmentGenerator, classLocalVar : String, codefg : CodeFragmentGenerator) = {
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
	private def asyncTaskLifeCycleGenerator(entryPoints : MList[ResourceUri], record : JawaRecord, endClassFragment : CodeFragmentGenerator, classLocalVar : String, codefg : CodeFragmentGenerator) = {
	  val constructionStack : MSet[JawaRecord] = msetEmpty ++ this.paramRecords
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
	private def contentProviderLifeCycleGenerator(entryPoints : MList[ResourceUri], record : JawaRecord, endClassFragment : CodeFragmentGenerator, classLocalVar : String, codefg : CodeFragmentGenerator) = {
	  val constructionStack : MSet[JawaRecord] = msetEmpty ++ this.paramRecords
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
	private def broadcastReceiverLifeCycleGenerator(entryPoints : MList[ResourceUri], record : JawaRecord, endClassFragment : CodeFragmentGenerator, classLocalVar : String, codefg : CodeFragmentGenerator) = {
	  val constructionStack : MSet[JawaRecord] = msetEmpty ++ this.paramRecords
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
	private def serviceLifeCycleGenerator(entryPoints : MList[ResourceUri], record : JawaRecord, endClassFragment : CodeFragmentGenerator, classLocalVar : String, codefg : CodeFragmentGenerator) = {
	  val constructionStack : MSet[JawaRecord] = msetEmpty ++ this.paramRecords
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
	private def activityLifeCycleGenerator(entryPoints : MList[ResourceUri], record : JawaRecord, endClassFragment : CodeFragmentGenerator, classLocalVar : String, codefg : CodeFragmentGenerator) = {
	  val constructionStack : MSet[JawaRecord] = msetEmpty ++ this.paramRecords
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
	
	private def generateAllCallbacks(entryPoints : MList[String], record : JawaRecord, classLocalVar : String) : CodeFragmentGenerator = {
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

}
