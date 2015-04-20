/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.pilarCodeGenerator

import org.sireum.util._
import org.stringtemplate.v4.STGroupFile
import org.stringtemplate.v4.ST
import java.util.ArrayList
import org.sireum.jawa.util.SignatureParser
import java.util.Arrays
import org.sireum.jawa.JawaClass
import org.sireum.jawa.JawaMethod
import org.sireum.jawa.util.StringFormConverter
import org.sireum.jawa.Center
import org.sireum.jawa.JawaResolver
import org.sireum.jawa.util.SignatureParser
import org.sireum.jawa.MessageCenter._
import org.sireum.jawa.NormalType
import org.sireum.jawa.pilarCodeGenerator.VariableGenerator
import org.sireum.jawa.pilarCodeGenerator.MethodGenerator
import org.sireum.amandroid.parser.ComponentInfo

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
class AndroidEnvironmentGenerator extends MethodGenerator {
	private final val TITLE = "AndroidEnvironmentGenerator"
  private var componentInfos : Set[ComponentInfo] = Set()
  
  def setComponentInfos(componentInfos : Set[ComponentInfo]) = this.componentInfos = componentInfos
  
	def generateInternal(methods : List[String]) : String = {
	  val classMap : MMap[String, MList[String]] = mmapEmpty
	  methods.map{
	    method => 
	      val clazz = StringFormConverter.getClassNameFromMethodSignature(method)
	      classMap.getOrElseUpdate(clazz, mlistEmpty) += method
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
	        val currentClass = Center.resolveClass(item._1, Center.ResolveLevel.HIERARCHY)
	        val ancestors = Center.getClassHierarchy.getAllSuperClassesOfIncluding(currentClass)
	        ancestors.foreach{
	          ancestor =>
	            val recName = ancestor.getName
              if(recName.equals(AndroidEntryPointConstants.ACTIVITY_CLASS)) activity = true
              if(recName.equals(AndroidEntryPointConstants.SERVICE_CLASS)) service = true
              if(recName.equals(AndroidEntryPointConstants.BROADCAST_RECEIVER_CLASS)) broadcastReceiver = true
              if(recName.equals(AndroidEntryPointConstants.CONTENT_PROVIDER_CLASS)) contentProvider = true
	        }
	        componentInfos.foreach{
	          ci =>
	            if(ci.name == currentClass.getName){
	              ci.typ match{
	                case "activity" => activity = true
	                case "service" => service = true
	                case "receiver" => broadcastReceiver = true
	                case "provider" => contentProvider = true
	                case _ =>
	              }
	            }
	        }
	        if(!activity && !service && !broadcastReceiver && !contentProvider) plain = true
	        var instanceNeeded = activity || service || broadcastReceiver || contentProvider
	        //How this part work? item._2 always empty!
	        var plainMethods: Map[String, JawaMethod] = Map()
	        if(!instanceNeeded || plain){
	          item._2.foreach{
	            procSig =>
	              Center.getMethod(procSig) match{
	                case Some(p) =>
		                plainMethods += (procSig -> p)
		                if(!p.isStatic) instanceNeeded = true
	                case None =>
	                  val className = StringFormConverter.getClassNameFromMethodSignature(procSig)
	                  if(!Center.containsClass(className)) err_msg_normal(TITLE, "Class for entry point " + className + " not found, skipping")
	                  else{
	                    Center.getMethod(procSig) match{
	                      case Some(p) => 
	                        plainMethods += (procSig -> p)
	                        if(!p.isStatic) instanceNeeded = true
	                      case None => err_msg_normal(TITLE, "Method for entry point " + procSig + " not found, skipping")
	                    }
	                  }
	              }
	          }
	        }
	        if(instanceNeeded){
	          val va = generateInstanceCreation(currentClass.getName, classStartFragment)
	          this.localVarsForClasses += (currentClass.getName -> va)
	          generateClassConstructor(currentClass, msetEmpty, classStartFragment)
	        }
	        val classLocalVar = localVarsForClasses.getOrElse(currentClass.getName, null)
	        
	        //now start to generate lifecycle for the four kinds of component
	        if(activity){
	          activityLifeCycleGenerator(item._2, currentClass, endClassFragment, classLocalVar, classStartFragment)
	        }
	        if(service){
	          serviceLifeCycleGenerator(item._2, currentClass, endClassFragment, classLocalVar, classStartFragment)
	        }
	        if(broadcastReceiver){
	          broadcastReceiverLifeCycleGenerator(item._2, currentClass, endClassFragment, classLocalVar, classStartFragment)
	        }
	        if(contentProvider){
	          contentProviderLifeCycleGenerator(item._2, currentClass, endClassFragment, classLocalVar, classStartFragment)
	        }
	        if(plain){
	          plainClassGenerator(plainMethods, endClassFragment, classLocalVar, classStartFragment)
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
	 * Generate plain class methods calling sequence. Because we don't know
	 * the order of the custom statements, we assume that it can loop arbitrarily.
	 * 
	 */
	private def plainClassGenerator(plainMethods: Map[String, JawaMethod], endClassFragment : CodeFragmentGenerator, classLocalVar : String, codefg : CodeFragmentGenerator) = {
	  val beforeClassFragment = new CodeFragmentGenerator
	  beforeClassFragment.addLabel
	  codeFragments.add(beforeClassFragment)
	  plainMethods.foreach{
	    case (currentMethodSig, currentMethod) =>
	      if(currentMethod.isStatic || classLocalVar != null){
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
	        generateMethodCall(currentMethodSig, "virtual", classLocalVar, msetEmpty, thenFragment)
	        createIfStmt(endClassFragment, thenFragment)
	        elseFragment.addLabel
	        codeFragments.add(elseFragment)
	        createIfStmt(beforeClassFragment, elseFragment)
	      } else {
	        err_msg_normal(TITLE, "Skipping method " + currentMethod + " because we have no instance")
	      }
	  }
	}
	
	/**
	 * generate the lifecycle for AsyncTask component
	 * @param entryPoints The list of methods to consider in this class
	 * @param rUri Current class resource uri
	 * @param endClassFragment
	 * @param classLocalVar Current class's local variable
	 * @param codefg Current code fragment
	 */
	private def asyncTaskLifeCycleGenerator(entryPoints : MList[ResourceUri], clazz : JawaClass, endClassFragment : CodeFragmentGenerator, classLocalVar : String, codefg : CodeFragmentGenerator) = {
	  val constructionStack : MSet[JawaClass] = msetEmpty ++ this.paramClasses
		createIfStmt(endClassFragment, codefg)
	}
	
	/**
	 * generate the lifecycle for contentprovider component
	 * @param entryPoints The list of methods to consider in this clazz
	 * @param rUri Current clazz resource uri
	 * @param endClassFragment
	 * @param classLocalVar Current clazz's local variable
	 * @param codefg Current code fragment
	 */
	private def contentProviderLifeCycleGenerator(entryPoints : MList[ResourceUri], clazz : JawaClass, endClassFragment : CodeFragmentGenerator, classLocalVar : String, codefg : CodeFragmentGenerator) = {
	  val constructionStack : MSet[JawaClass] = msetEmpty ++ this.paramClasses
		createIfStmt(endClassFragment, codefg)
		
		// 1. onCreate:
		val onCreateFragment = new CodeFragmentGenerator
	  onCreateFragment.addLabel
	  codeFragments.add(onCreateFragment)
	  searchAndBuildMethodCall(AndroidEntryPointConstants.CONTENTPROVIDER_ONCREATE, clazz, entryPoints, constructionStack, onCreateFragment)
	  
	  //all other entry points of this class can be called in arbitary order
	  val endWhileFragment = generateAllCallbacks(entryPoints, clazz, classLocalVar)
	  createIfStmt(onCreateFragment, endWhileFragment)
	}
	
	/**
	 * generate the lifecycle for broadcastreceiver component
	 * @param entryPoints The list of methods to consider in this clazz
	 * @param rUri Current clazz resource uri
	 * @param endClassFragment
	 * @param classLocalVar Current clazz's local variable
	 * @param codefg Current code fragment
	 */
	private def broadcastReceiverLifeCycleGenerator(entryPoints : MList[ResourceUri], clazz : JawaClass, endClassFragment : CodeFragmentGenerator, classLocalVar : String, codefg : CodeFragmentGenerator) = {
	  val constructionStack : MSet[JawaClass] = msetEmpty ++ this.paramClasses
		createIfStmt(endClassFragment, codefg)
		
		// 1. onReceive:
		val onReceiveFragment = new CodeFragmentGenerator
	  onReceiveFragment.addLabel
	  codeFragments.add(onReceiveFragment)
	  searchAndBuildMethodCall(AndroidEntryPointConstants.BROADCAST_ONRECEIVE, clazz, entryPoints, constructionStack, onReceiveFragment)
	  
	  //all other entry points of this class can be called in arbitary order
	  val endWhileFragment = generateAllCallbacks(entryPoints, clazz, classLocalVar)
	  createIfStmt(onReceiveFragment, endWhileFragment)
	}
	
	/**
	 * generate the lifecycle for service component
	 * @param entryPoints The list of methods to consider in this clazz
	 * @param rUri Current clazz resource uri
	 * @param endClassFragment
	 * @param classLocalVar Current clazz's local variable
	 * @param codefg Current code fragment
	 */
	private def serviceLifeCycleGenerator(entryPoints : MList[ResourceUri], clazz : JawaClass, endClassFragment : CodeFragmentGenerator, classLocalVar : String, codefg : CodeFragmentGenerator) = {
	  val constructionStack : MSet[JawaClass] = msetEmpty ++ this.paramClasses
		createIfStmt(endClassFragment, codefg)
		
	  val r = Center.resolveClass("android.app.ContextImpl", Center.ResolveLevel.HIERARCHY)
	  val va = generateInstanceCreation(r.getName, codefg)
	  localVarsForClasses += (r.getName -> va)
    generateClassConstructor(r, constructionStack, codefg)
    createFieldSetStmt(localVarsForClasses(clazz.getName), "android.content.ContextWrapper.mBase", va, List("object"), codefg)
	  
	  
		// 1. onCreate:
	  val onCreateFragment = new CodeFragmentGenerator
	  onCreateFragment.addLabel
	  codeFragments.add(onCreateFragment)
	  searchAndBuildMethodCall(AndroidEntryPointConstants.SERVICE_ONCREATE, clazz, entryPoints, constructionStack, onCreateFragment)
	  // service has two different lifecycles:
	  // lifecycle 1:
	  // 2. onStart:
	  searchAndBuildMethodCall(AndroidEntryPointConstants.SERVICE_ONSTART1, clazz, entryPoints, constructionStack, onCreateFragment)
	  searchAndBuildMethodCall(AndroidEntryPointConstants.SERVICE_ONSTART2, clazz, entryPoints, constructionStack, onCreateFragment)
	  //all other entry points of this class can be called in arbitary order
	  generateAllCallbacks(entryPoints, clazz, classLocalVar)
	  // lifecycle 1 end.
	  
	  // lifecycle 2:
	  // 2. onBind:
	  val onBindFragment = new CodeFragmentGenerator
	  onBindFragment.addLabel
	  codeFragments.add(onBindFragment)
	  searchAndBuildMethodCall(AndroidEntryPointConstants.SERVICE_ONBIND, clazz, entryPoints, constructionStack, onBindFragment)
	  val beforemethodsFragment = new CodeFragmentGenerator
	  beforemethodsFragment.addLabel
	  codeFragments.add(beforemethodsFragment)
	  //all other entry points of this class can be called in arbitary order
	  generateAllCallbacks(entryPoints, clazz, classLocalVar)
	  
	  // 3. onRebind:
	  val onRebindFragment = new CodeFragmentGenerator
	  onRebindFragment.addLabel
	  codeFragments.add(onRebindFragment)
	  searchAndBuildMethodCall(AndroidEntryPointConstants.SERVICE_ONREBIND, clazz, entryPoints, constructionStack, onRebindFragment)
	  createIfStmt(beforemethodsFragment, onRebindFragment)
	  
	  // 4. onUnbind:
	  searchAndBuildMethodCall(AndroidEntryPointConstants.SERVICE_ONUNBIND, clazz, entryPoints, constructionStack, onRebindFragment)
	  createIfStmt(onBindFragment, onRebindFragment)
	  // lifecycle 2 end.
	  
	  // 3 | 5. onDestory:
	  val onDestoryFragment = new CodeFragmentGenerator
	  onDestoryFragment.addLabel
	  codeFragments.add(onDestoryFragment)
	  searchAndBuildMethodCall(AndroidEntryPointConstants.SERVICE_ONDESTROY, clazz, entryPoints, constructionStack, onDestoryFragment)
	  // either begin or end or next class:
	  createIfStmt(onCreateFragment, onDestoryFragment)
	  createIfStmt(endClassFragment, onDestoryFragment)
	}
	  
	/**
	 * generate the lifecycle for activity component
	 * @param entryPoints The list of methods to consider in this clazz
	 * @param rUri Current clazz resource uri
	 * @param endClassFragment
	 * @param classLocalVar Current clazz's local variable
	 * @param codefg Current code fragment
	 */
	private def activityLifeCycleGenerator(entryPoints : MList[ResourceUri], clazz : JawaClass, endClassFragment : CodeFragmentGenerator, classLocalVar : String, codefg : CodeFragmentGenerator) = {
	  val constructionStack : MSet[JawaClass] = msetEmpty ++ this.paramClasses
	  createIfStmt(endClassFragment, codefg)
	  val r = Center.resolveClass("android.app.ContextImpl", Center.ResolveLevel.HIERARCHY)
	  val va = generateInstanceCreation(r.getName, codefg)
	  localVarsForClasses += (r.getName -> va)
    generateClassConstructor(r, constructionStack, codefg)
    createFieldSetStmt(localVarsForClasses(clazz.getName), "android.view.ContextThemeWrapper.mBase", va, List("object"), codefg)
    
	  generateMethodCall(AndroidEntryPointConstants.ACTIVITY_SETINTENT_SIG, "virtual", localVarsForClasses(clazz.getName), constructionStack, codefg)
	  
	  // 1. onCreate:
	  val onCreateFragment = new CodeFragmentGenerator
	  onCreateFragment.addLabel
	  codeFragments.add(onCreateFragment)
	  searchAndBuildMethodCall(AndroidEntryPointConstants.ACTIVITY_ONCREATE, clazz, entryPoints, constructionStack, onCreateFragment)
	  // 2. onStart:
	  val onStartFragment = new CodeFragmentGenerator
	  onStartFragment.addLabel
	  codeFragments.add(onStartFragment)
	  searchAndBuildMethodCall(AndroidEntryPointConstants.ACTIVITY_ONSTART, clazz, entryPoints, constructionStack, onStartFragment)
	  searchAndBuildMethodCall(AndroidEntryPointConstants.ACTIVITY_ONRESTOREINSTANCESTATE, clazz, entryPoints, constructionStack, onStartFragment)
	  searchAndBuildMethodCall(AndroidEntryPointConstants.ACTIVITY_ONPOSTCREATE, clazz, entryPoints, constructionStack, onStartFragment)
	  // 3. onResume:
	  val onResumeFragment = new CodeFragmentGenerator
	  onResumeFragment.addLabel
	  codeFragments.add(onResumeFragment)
	  searchAndBuildMethodCall(AndroidEntryPointConstants.ACTIVITY_ONRESUME, clazz, entryPoints, constructionStack, onResumeFragment)
	  searchAndBuildMethodCall(AndroidEntryPointConstants.ACTIVITY_ONPOSTRESUME, clazz, entryPoints, constructionStack, onResumeFragment)
	  
	  // all other entry points of this activity
	  generateAllCallbacks(entryPoints, clazz, classLocalVar)
	  // 4. onPause:
	  val onPauseFragment = new CodeFragmentGenerator
	  onPauseFragment.addLabel
	  codeFragments.add(onPauseFragment)
	  searchAndBuildMethodCall(AndroidEntryPointConstants.ACTIVITY_ONPAUSE, clazz, entryPoints, constructionStack, onPauseFragment)
	  searchAndBuildMethodCall(AndroidEntryPointConstants.ACTIVITY_ONCREATEDESCRIPTION, clazz, entryPoints, constructionStack, onPauseFragment)
	  searchAndBuildMethodCall(AndroidEntryPointConstants.ACTIVITY_ONSAVEINSTANCESTATE, clazz, entryPoints, constructionStack, onPauseFragment)
	  // goto onDestory, onRestart or onCreate:
	  val onStopFragment = new CodeFragmentGenerator
	  createIfStmt(onStopFragment, onPauseFragment)
	  createIfStmt(onResumeFragment, onPauseFragment)
	  createIfStmt(onCreateFragment, onPauseFragment)
	  
	  // 5. onStop:
	  onStopFragment.addLabel
	  codeFragments.add(onStopFragment)
	  searchAndBuildMethodCall(AndroidEntryPointConstants.ACTIVITY_ONSTOP, clazz, entryPoints, constructionStack, onStopFragment)
	  //goto onDestory, onRestart or onCreate:
	  val onDestoryFragment = new CodeFragmentGenerator
	  val onRestartFragment = new CodeFragmentGenerator
	  createIfStmt(onDestoryFragment, onStopFragment)
	  createIfStmt(onRestartFragment, onStopFragment)
	  createIfStmt(onCreateFragment, onStopFragment)
	  
	  // 6. onRestart:
	  onRestartFragment.addLabel
	  codeFragments.add(onRestartFragment)
	  searchAndBuildMethodCall(AndroidEntryPointConstants.ACTIVITY_ONRESTART, clazz, entryPoints, constructionStack, onRestartFragment)
	  if(onStartFragment != null){
	    createGotoStmt(onStartFragment, onRestartFragment)
	  }
	  
	  // 7. onDestory:
	  onDestoryFragment.addLabel
	  codeFragments.add(onDestoryFragment)
	  searchAndBuildMethodCall(AndroidEntryPointConstants.ACTIVITY_ONDESTROY, clazz, entryPoints, constructionStack, onDestoryFragment)
	  
	  createIfStmt(endClassFragment, onDestoryFragment)
	}
	
	private def generateAllCallbacks(entryPoints : MList[String], clazz : JawaClass, classLocalVar : String) : CodeFragmentGenerator = {
	  val startWhileFragment = new CodeFragmentGenerator
	  startWhileFragment.addLabel
	  codeFragments.add(startWhileFragment)
	  val endWhileFragment = new CodeFragmentGenerator
	  val methods = clazz.getMethods
	  import scala.collection.JavaConversions._
	  for(currentMethod <- methods){
	    if(entryPoints.contains(currentMethod.getSignature)){ 
	      val pSig = currentMethod.getSignature
	      if(!AndroidEntryPointConstants.getActivityLifecycleMethods.contains(currentMethod.getSubSignature)){
		      val thenStmtFragment = new CodeFragmentGenerator
		      createIfStmt(thenStmtFragment, startWhileFragment)
		      val elseStmtFragment = new CodeFragmentGenerator
		      createGotoStmt(elseStmtFragment, startWhileFragment)
		      thenStmtFragment.addLabel
		      codeFragments.add(thenStmtFragment)
		      generateMethodCall(pSig, "virtual", classLocalVar, msetEmpty + clazz, thenStmtFragment)
		      elseStmtFragment.addLabel
		      codeFragments.add(elseStmtFragment)
	      }
	    }
	  }
	  val callBackFragment = new CodeFragmentGenerator
	  callBackFragment.addLabel
	  codeFragments.add(callBackFragment)
	  addCallbackMethods(clazz, classLocalVar, callBackFragment)
	  endWhileFragment.addLabel
	  codeFragments.add(endWhileFragment)
	  createIfStmt(startWhileFragment, endWhileFragment)
	  endWhileFragment
	}

}
