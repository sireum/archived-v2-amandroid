package org.sireum.amandroid

/**
 * This object provides constants which represent pilar access flag; Some helper methods are also present.
 *
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object AccessFlag{
  final private val ABSTRACT = 							0x0001
  final private val FINAL =									0x0002
  final private val INTERFACE =							0x0004
  final private val NATIVE =								0x0008
  final private val PRIVATE =								0x0010
  final private val PROTECTED =							0x0020
  final private val PUBLIC =								0x0040
  final private val STATIC =								0x0080
  final private val SYNCHRONIZED =					0x0100
  final private val TRANSIENT =							0x0200
  final private val VOLATILE =							0x0400
  final private val STRICTFP =							0x0800
  final private val ANNOTATION =						0x1000
  final private val ENUM =									0x2000
  final private val SYNTHETIC =							0x4000
  final private val CONSTRUCTOR =						0x8000
  final private val DECLARED_SYNCHRONIZED = 0x10000
  
  def getAccessFlags(str : String) : Int = {
    var af : Int = 0
    if(str.contains("ABSTRACT")) af = af | ABSTRACT
    if(str.contains("FINAL")) af = af | FINAL
    if(str.contains("INTERFACE")) af = af | INTERFACE
    if(str.contains("NATIVE")) af = af | NATIVE
    if(str.contains("PRIVATE")) af = af | PRIVATE
    else if(str.contains("PROTECTED")) af = af | PROTECTED
    else if(str.contains("PUBLIC")) af = af | PUBLIC
    if(str.contains("STATIC")) af = af | STATIC
    if(str.contains("TRANSIENT")) af = af | TRANSIENT
    if(str.contains("VOLATILE")) af = af | VOLATILE
    if(str.contains("STRICTFP")) af = af | STRICTFP
    if(str.contains("ANNOTATION")) af = af | ANNOTATION
    if(str.contains("ENUM")) af = af | ENUM
    if(str.contains("SYNTHETIC")) af = af | SYNTHETIC
    if(str.contains("CONSTRUCTOR")) af = af | CONSTRUCTOR
    if(str.contains("DECLARED_SYNCHRONIZED")) af = af | DECLARED_SYNCHRONIZED
    else if(str.contains("SYNCHRONIZED")) af = af | SYNCHRONIZED
    af
  }
  
  def isAbstract(af : Int) : Boolean = (af & ABSTRACT) != 0
  def isFinal(af : Int) : Boolean = (af & FINAL) != 0
  def isInterface(af : Int) : Boolean = (af & INTERFACE) != 0
  def isNative(af : Int) : Boolean = (af & NATIVE) != 0
  def isPrivate(af : Int) : Boolean = (af & PRIVATE) != 0
  def isProtected(af : Int) : Boolean = (af & PROTECTED) != 0
  def isPublic(af : Int) : Boolean = (af & PUBLIC) != 0
  def isStatic(af : Int) : Boolean = (af & STATIC) != 0
  def isSynchronized(af : Int) : Boolean = (af & SYNCHRONIZED) != 0
  def isTransient(af : Int) : Boolean = (af & TRANSIENT) != 0
  def isVolatile(af : Int) : Boolean = (af & VOLATILE) != 0
  def isStrictFP(af : Int) : Boolean = (af & STRICTFP) != 0
  def isAnnotation(af : Int) : Boolean = (af & ANNOTATION) != 0
  def isEnum(af : Int) : Boolean = (af & ENUM) != 0
  def isSynthetic(af : Int) : Boolean = (af & SYNTHETIC) != 0
  def isConstructor(af : Int) : Boolean = (af & CONSTRUCTOR) != 0
  def isDeclaredSynchronized(af : Int) : Boolean = (af & DECLARED_SYNCHRONIZED) != 0
  
  def toString(af : Int) : String = {
    val sb = new StringBuffer
    if(isPublic(af)) sb.append("public ")
    else if(isPrivate(af)) sb.append("private ")
    else if(isProtected(af)) sb.append("protected ")
    if(isAbstract(af)) sb.append("abstract ")
    if(isStatic(af)) sb.append("static ")
    if(isFinal(af)) sb.append("final ")
    if(isSynchronized(af)) sb.append("synchronized ")
    if(isNative(af)) sb.append("native ")
    if(isTransient(af)) sb.append("transient ")
    if(isVolatile(af)) sb.append("volatile ")
    if(isStrictFP(af)) sb.append("strictfp ")
    if(isAnnotation(af)) sb.append("annotation ")
    if(isEnum(af)) sb.append("enum ")
    if(isSynthetic(af)) sb.append("synthetic ")
    if(isConstructor(af)) sb.append("constructor ")
    if(isDeclaredSynchronized(af)) sb.append("declared_synchronized ")
    if(isInterface(af)) sb.append("interface ")
    sb.toString().trim()
  }
}