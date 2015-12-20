/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.alir.pta.reachingFactsAnalysis.model

import org.sireum.amandroid.AndroidConstants
import org.sireum.util._
import org.sireum.jawa.JawaMethod
import org.sireum.jawa.alir.Context
import org.sireum.jawa.alir.pta.reachingFactsAnalysis._
import org.sireum.amandroid.parser.UriData
<<<<<<< HEAD
import org.sireum.amandroid.AppCenter
=======
>>>>>>> upstream/master
import org.sireum.jawa.JawaClass
import org.sireum.jawa.alir.pta.Instance
import java.net.URI
import org.sireum.amandroid.alir.pta.reachingFactsAnalysis.IntentHelper
<<<<<<< HEAD:sireum-amandroid-alir/src/main/scala/org/sireum/amandroid/alir/pta/reachingFactsAnalysis/model/InterComponentCommunicationModel.scala
import org.sireum.jawa.alir.pta.PTAResult
import org.sireum.jawa.alir.pta.VarSlot
<<<<<<< HEAD
=======
>>>>>>> CommunicationLeakage:sireum-amandroid-alir/src/main/scala/org/sireum/amandroid/alir/pta/reachingFactsAnalysis/model/InterComponentCommunicationModel.scala
=======
import org.sireum.jawa.JawaType
import org.sireum.amandroid.Apk
>>>>>>> upstream/master

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object InterComponentCommunicationModel {
  final val TITLE = "InterComponentCommunicationModel"
<<<<<<< HEAD
	def isIccOperation(proc : JawaMethod) : Boolean = {
    var flag = false
    val childClass = proc.getDeclaringClass
    val parentClass = Center.resolveClass(AndroidConstants.CONTEXT, Center.ResolveLevel.HIERARCHY)
    if(!childClass.isInterface && Center.getClassHierarchy.isClassRecursivelySubClassOfIncluding(childClass, parentClass))
	    AndroidConstants.getIccMethods.foreach{
	      item =>
	        if(proc.getSubSignature == item)
	         flag = true
	    }
    flag
  }
	
	def doIccCall(s : PTAResult, calleeMethod : JawaMethod, args : List[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[JawaMethod]) = {
	  require(args.size > 1)
	  val intentSlot = VarSlot(args(1))
	  val intentValues = s.pointsToSet(intentSlot, currentContext)
	  val intentcontents = IntentHelper.getIntentContents(s, intentValues, currentContext)
    val compType: AndroidConstants.CompType.Value = AndroidConstants.getIccCallType(calleeMethod.getSubSignature)
	  val comMap = IntentHelper.mappingIntents(intentcontents, compType)
	  val targets : MSet[JawaMethod] = msetEmpty
	  comMap.foreach{
	    case (_, coms) =>
	      coms.foreach{
	        case (com, _) =>
	          com.tryGetMethod(AndroidConstants.MAINCOMP_ENV_SUBSIG) match{
              case Some(r) => targets += r
              case None => 
                com.tryGetMethod(AndroidConstants.COMP_ENV_SUBSIG) match{
=======
  def isIccOperation(proc : JawaMethod) : Boolean = {
    var flag = false
    val childClass = proc.getDeclaringClass
    val parentClass = childClass.global.getClassOrResolve(new JawaType(AndroidConstants.CONTEXT))
    if(!childClass.isInterface && childClass.global.getClassHierarchy.isClassRecursivelySubClassOfIncluding(childClass, parentClass))
      AndroidConstants.getIccMethods.foreach{
        item =>
          if(proc.getSubSignature == item)
           flag = true
      }
    flag
  }

  def doIccCall(apk: Apk, s : PTAResult, calleeMethod : JawaMethod, args : List[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[JawaMethod]) = {
    require(args.size > 1)
    val intentSlot = VarSlot(args(1), false, true)
    val intentValues = s.pointsToSet(intentSlot, currentContext)
    val intentcontents = IntentHelper.getIntentContents(s, intentValues, currentContext)
    val compType: AndroidConstants.CompType.Value = AndroidConstants.getIccCallType(calleeMethod.getSubSignature)
    val comMap = IntentHelper.mappingIntents(calleeMethod.getDeclaringClass.global, apk, intentcontents, compType)
    val targets : MSet[JawaMethod] = msetEmpty
    comMap.foreach{
      case (_, coms) =>
        coms.foreach{
          case (com, _) =>
            com.getMethod(AndroidConstants.MAINCOMP_ENV_SUBSIG) match{
              case Some(r) => targets += r
              case None => 
                com.getMethod(AndroidConstants.COMP_ENV_SUBSIG) match{
>>>>>>> upstream/master
                  case Some(r) => targets += r
                  case None => 
                }
            }
<<<<<<< HEAD
	      }
	  }
	  (isetEmpty, targets.toSet)
	}
=======
        }
    }
    (isetEmpty, targets.toSet)
  }
>>>>>>> upstream/master

}