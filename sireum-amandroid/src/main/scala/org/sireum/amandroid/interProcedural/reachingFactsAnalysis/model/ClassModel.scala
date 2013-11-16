package org.sireum.amandroid.interProcedural.reachingFactsAnalysis.model

import org.sireum.amandroid._
import org.sireum.util._
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAFact
import org.sireum.amandroid.interProcedural.Context
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.VarSlot
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAConcreteStringInstance
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.FieldSlot
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.RFAPointStringInstance
import org.sireum.amandroid.util.StringFormConverter
import org.sireum.amandroid.MessageCenter._
import org.sireum.amandroid.interProcedural.reachingFactsAnalysis.ReachingFactsAnalysisHelper

object ClassModel {
	def isClass(r : AmandroidRecord) : Boolean = r.getName == "[|java:lang:Class|]"
	  
	def doClassCall(s : ISet[RFAFact], p : AmandroidProcedure, args : List[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact], Boolean) = {
	  var newFacts = isetEmpty[RFAFact]
	  var delFacts = isetEmpty[RFAFact]
	  var byPassFlag = true
	  p.getSignature match{
	    case "[|Ljava/lang/Class;.<init>:()V|]" =>  //private constructor
		  case "[|Ljava/lang/Class;.arraycopy:([Ljava/lang/Object;[Ljava/lang/Object;[Ljava/lang/Object;)[Ljava/lang/Object;|]" =>  //private static
		  case "[|Ljava/lang/Class;.asSubclass:(Ljava/lang/Class;)Ljava/lang/Class;|]" =>  //public
		    require(retVars.size == 1)
		    classAsSubClass(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "[|Ljava/lang/Class;.cast:(Ljava/lang/Object;)Ljava/lang/Object;|]" =>  //public
		    require(retVars.size == 1)
		    classAsSubClass(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "[|Ljava/lang/Class;.classForName:(Ljava/lang/String;ZLjava/lang/ClassLoader;)Ljava/lang/Class;|]" =>  //private static native
		    require(retVars.size == 1)
		    classForName(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "[|Ljava/lang/Class;.desiredAssertionStatus:()Z|]" =>  //public native
		  case "[|Ljava/lang/Class;.forName:(Ljava/lang/String;)Ljava/lang/Class;|]" =>  //public static
		    require(retVars.size == 1)
		    classForName(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "[|Ljava/lang/Class;.forName:(Ljava/lang/String;ZLjava/lang/ClassLoader;)Ljava/lang/Class;|]" =>  //public static
		    require(retVars.size == 1)
		    classForName(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "[|Ljava/lang/Class;.getAnnotation:(Ljava/lang/Class;)Ljava/lang/annotation/Annotation;|]" =>  //public
		  case "[|Ljava/lang/Class;.getAnnotations:()[Ljava/lang/annotation/Annotation;|]" =>  //public
		  case "[|Ljava/lang/Class;.getCanonicalName:()Ljava/lang/String;|]" =>  //public
		  case "[|Ljava/lang/Class;.getClassLoader:()Ljava/lang/ClassLoader;|]" =>  //public
		  case "[|Ljava/lang/Class;.getClassLoader:(Ljava/lang/Class;)Ljava/lang/ClassLoader;|]" =>  //private static native
		  case "[|Ljava/lang/Class;.getClassLoaderImpl:()Ljava/lang/ClassLoader;|]" =>  //
		  case "[|Ljava/lang/Class;.getClasses:()[Ljava/lang/Class;|]" =>  //public
		  case "[|Ljava/lang/Class;.getComponentType:()Ljava/lang/Class;|]" =>  //public native
		    
		  case "[|Ljava/lang/Class;.getConstructor:([Ljava/lang/Class;)Ljava/lang/reflect/Constructor;|]" =>  //public
		  case "[|Ljava/lang/Class;.getConstructorOrMethod:(Ljava/lang/String;ZZ[Ljava/lang/Class;)Ljava/lang/reflect/Member;|]" =>  //private
		  case "[|Ljava/lang/Class;.getConstructors:()[Ljava/lang/reflect/Constructor;|]" =>  //public
		  case "[|Ljava/lang/Class;.getDeclaredAnnotation:(Ljava/lang/Class;)Ljava/lang/annotation/Annotation;|]" =>  //private native
		  case "[|Ljava/lang/Class;.getDeclaredAnnotations:()[Ljava/lang/annotation/Annotation;|]" =>  //public native
		  case "[|Ljava/lang/Class;.getDeclaredClasses:()[Ljava/lang/Class;|]" =>  //public
		  case "[|Ljava/lang/Class;.getDeclaredClasses:(Ljava/lang/Class;Z)[Ljava/lang/Class;|]" =>  //private static native
		  case "[|Ljava/lang/Class;.getDeclaredConstructor:([Ljava/lang/Class;)Ljava/lang/reflect/Constructor;|]" =>  //public
		  case "[|Ljava/lang/Class;.getDeclaredConstructorOrMethod:(Ljava/lang/Class;Ljava/lang/String;[Ljava/lang/Class;)Ljava/lang/reflect/Member;|]" =>  //static native
		  case "[|Ljava/lang/Class;.getDeclaredConstructors:()[Ljava/lang/reflect/Constructor;|]" =>  //public
		  case "[|Ljava/lang/Class;.getDeclaredConstructors:(Ljava/lang/Class;Z)[Ljava/lang/reflect/Constructor;|]" =>  //private static native
		  case "[|Ljava/lang/Class;.getDeclaredField:(Ljava/lang/Class;Ljava/lang/String;)Ljava/lang/reflect/Field;|]" =>  //static native
		  case "[|Ljava/lang/Class;.getDeclaredField:(Ljava/lang/String;)Ljava/lang/reflect/Field;|]" =>  //public
		  case "[|Ljava/lang/Class;.getDeclaredFields:()[Ljava/lang/reflect/Field;|]" =>  //public
		  case "[|Ljava/lang/Class;.getDeclaredFields:(Ljava/lang/Class;Z)[Ljava/lang/reflect/Field;|]" =>  //static native
		  case "[|Ljava/lang/Class;.getDeclaredMethod:(Ljava/lang/String;[Ljava/lang/Class;)Ljava/lang/reflect/Method;|]" =>  //public
		  case "[|Ljava/lang/Class;.getDeclaredMethods:()[Ljava/lang/reflect/Method;|]" =>  //public
		  case "[|Ljava/lang/Class;.getDeclaredMethods:(Ljava/lang/Class;Z)[Ljava/lang/reflect/Method;|]" =>  //static native
		  case "[|Ljava/lang/Class;.getDeclaringClass:()Ljava/lang/Class;|]" =>  //public native
		  case "[|Ljava/lang/Class;.getEnclosingClass:()Ljava/lang/Class;|]" =>  //public native
		  case "[|Ljava/lang/Class;.getEnclosingConstructor:()Ljava/lang/reflect/Constructor;|]" =>  //public native
		  case "[|Ljava/lang/Class;.getEnclosingMethod:()Ljava/lang/reflect/Method;|]" =>  //public native
		  case "[|Ljava/lang/Class;.getEnumConstants:()[Ljava/lang/Object;|]" =>  //public
		  case "[|Ljava/lang/Class;.getField:(Ljava/lang/String;)Ljava/lang/reflect/Field;|]" =>  //public
		  case "[|Ljava/lang/Class;.getFields:()[Ljava/lang/reflect/Field;|]" =>  //public
		  case "[|Ljava/lang/Class;.getGenericInterfaces:()[Ljava/lang/reflect/Type;|]" =>  //public
		  case "[|Ljava/lang/Class;.getGenericSuperclass:()Ljava/lang/reflect/Type;|]" =>  //public
		  case "[|Ljava/lang/Class;.getInnerClassName:()Ljava/lang/String;|]" =>  //private native
		  case "[|Ljava/lang/Class;.getInterfaces:()[Ljava/lang/Class;|]" =>  //public native
		  case "[|Ljava/lang/Class;.getMethod:(Ljava/lang/String;[Ljava/lang/Class;)Ljava/lang/reflect/Method;|]" =>  //public
		  case "[|Ljava/lang/Class;.getMethods:()[Ljava/lang/reflect/Method;|]" =>  //public
		  case "[|Ljava/lang/Class;.getModifiers:()I|]" =>  //public
		  case "[|Ljava/lang/Class;.getModifiers:(Ljava/lang/Class;Z)I|]" =>  //private static native
		  case "[|Ljava/lang/Class;.getName:()Ljava/lang/String;|]" =>  //public
		    require(retVars.size == 1)
		    classGetName(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "[|Ljava/lang/Class;.getNameNative:()Ljava/lang/String;|]" =>  //private native
		    require(retVars.size == 1)
		    classGetName(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
		    byPassFlag = false
		  case "[|Ljava/lang/Class;.getPackage:()Ljava/lang/Package;|]" =>  //public
		  case "[|Ljava/lang/Class;.getProtectionDomain:()Ljava/security/ProtectionDomain;|]" =>  //public
		  case "[|Ljava/lang/Class;.getPublicConstructorOrMethodRecursive:(Ljava/lang/String;[Ljava/lang/Class;)Ljava/lang/reflect/Member;|]" =>  //private
		  case "[|Ljava/lang/Class;.getPublicFieldRecursive:(Ljava/lang/String;)Ljava/lang/reflect/Field;|]" =>  //private
		  case "[|Ljava/lang/Class;.getPublicFieldsRecursive:(Ljava/util/List;)V|]" =>  //private
		  case "[|Ljava/lang/Class;.getPublicMethodsRecursive:(Ljava/util/List;)V|]" =>  //private
		  case "[|Ljava/lang/Class;.getResource:(Ljava/lang/String;)Ljava/net/URL;|]" =>  //public
		  case "[|Ljava/lang/Class;.getResourceAsStream:(Ljava/lang/String;)Ljava/io/InputStream;|]" =>  //public
		  case "[|Ljava/lang/Class;.getSignatureAnnotation:()[Ljava/lang/Object;|]" =>  //private native
		  case "[|Ljava/lang/Class;.getSignatureAttribute:()Ljava/lang/String;|]" =>  //private
		  case "[|Ljava/lang/Class;.getSigners:()[Ljava/lang/Object;|]" =>  //public
		  case "[|Ljava/lang/Class;.getSimpleName:()Ljava/lang/String;|]" =>  //public
		  case "[|Ljava/lang/Class;.getSuperclass:()Ljava/lang/Class;|]" =>  //public native
		  case "[|Ljava/lang/Class;.getTypeParameters:()[Ljava/lang/reflect/TypeVariable;|]" =>  //public declared_synchronized
		  case "[|Ljava/lang/Class;.isAnnotation:()Z|]" =>  //public
		  case "[|Ljava/lang/Class;.isAnnotationPresent:(Ljava/lang/Class;)Z|]" =>  //public
		  case "[|Ljava/lang/Class;.isAnonymousClass:()Z|]" =>  //public native
		  case "[|Ljava/lang/Class;.isArray:()Z|]" =>  //public
		  case "[|Ljava/lang/Class;.isAssignableFrom:(Ljava/lang/Class;)Z|]" =>  //public native
		  case "[|Ljava/lang/Class;.isDeclaredAnnotationPresent:(Ljava/lang/Class;)Z|]" =>  //private native
		  case "[|Ljava/lang/Class;.isEnum:()Z|]" =>  //public
		  case "[|Ljava/lang/Class;.isInstance:(Ljava/lang/Object;)Z|]" =>  //public native
		  case "[|Ljava/lang/Class;.isInterface:()Z|]" =>  //public native
		  case "[|Ljava/lang/Class;.isLocalClass:()Z|]" =>  //public
		  case "[|Ljava/lang/Class;.isMemberClass:()Z|]" =>  //public
		  case "[|Ljava/lang/Class;.isPrimitive:()Z|]" =>  //public native
		  case "[|Ljava/lang/Class;.isSynthetic:()Z|]" =>  //public
		  case "[|Ljava/lang/Class;.newInstance:()Ljava/lang/Object;|]" =>  //public
		  case "[|Ljava/lang/Class;.newInstanceImpl:()Ljava/lang/Object;|]" =>  //private native
		  case "[|Ljava/lang/Class;.toString:()Ljava/lang/String;|]" =>  //public
	  }
	  (newFacts, delFacts, byPassFlag)
	}
	
	
	/**
   * [|Ljava/lang/Class;.asSubclass:(Ljava/lang/Class;)Ljava/lang/Class;|]
   */
  private def classAsSubClass(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >1)
    val thisSlot = VarSlot(args(0))
	  val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	  var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  thisValue.foreach{
	    tv =>
	      newfacts += RFAFact(VarSlot(retVar), tv)
	  }
    (newfacts, delfacts)
  }
  
  /**
   * [|Ljava/lang/Class;.asSubclass:(Ljava/lang/Class;)Ljava/lang/Class;|]
   */
  private def classCast(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
    require(args.size >1)
    val paramSlot = VarSlot(args(1))
	  val paramValue = factMap.getOrElse(paramSlot, isetEmpty)
	  var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
	  paramValue.foreach{
	    pv =>
	      newfacts += RFAFact(VarSlot(retVar), pv)
	  }
    (newfacts, delfacts)
  }
  
  /**
	 * [|Ljava/lang/Class;.forName:(Ljava/lang/String;)Ljava/lang/Class;|]   static
	 */
	private def classForName(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
	  // algo:thisValue.foreach.{ cIns => get value of (cIns.name|]") and create fact (retVar, value)}
    require(args.size > 0)
    val clazzNameSlot = VarSlot(args(0))
    val clazzNameValue = factMap.getOrElse(clazzNameSlot, isetEmpty)
    var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
    clazzNameValue.foreach{
      cIns =>
        cIns match{
          case cstr @ RFAConcreteStringInstance(text, c) =>
            val recordName = StringFormConverter.formatClassNameToRecordName(text)
            val recordOpt = Center.tryLoadRecord(recordName, Center.ResolveLevel.BODIES)
            recordOpt match{
              case Some(record) =>
            		newfacts += RFAFact(VarSlot(retVar), record.getClassObj)
              case None =>
                System.err.println("Given class name probably come from another app: " + cIns)
            }
          case pstr @ RFAPointStringInstance(c) => 
            err_msg_normal("Get class use point string: " + pstr)
          case _ =>
            err_msg_normal("Get class use unknown instance: " + cIns)
        }
    }
    (newfacts, delfacts)
	}
	
	/**
	 * [|Ljava/lang/Class;.getName:()Ljava/lang/String;|]
	 */
	private def classGetName(s : ISet[RFAFact], args : List[String], retVar : String, currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
    val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
	  // algo:thisValue.foreach.{ cIns => get value of (cIns.name|]") and create fact (retVar, value)}
    require(args.size > 0)
    val thisSlot = VarSlot(args(0))
    val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
    var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
    thisValue.foreach{
      cIns =>
        require(cIns.isInstanceOf[ClassInstance])
        val name = cIns.asInstanceOf[ClassInstance].getName
        val strIns = RFAConcreteStringInstance(name, cIns.getDefSite)
        newfacts += (RFAFact(VarSlot(retVar), strIns))
    }
    (newfacts, delfacts)
	}
	
}