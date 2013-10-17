package org.sireum.amandroid.util

import org.sireum.amandroid._


/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object StringFormConverter {
  
  /**
	 * get record name from procedure signature. e.g. [|Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z|] -> [|java:lang:Object|]
	 */
  
  def getRecordNameFromProcedureSignature(sig : String) : String = {
    val typ = getRecordTypeFromProcedureSignature(sig)
    typ.name
  }
  
  /**
	 * get record type from procedure signature. e.g. [|Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z|] -> [|java:lang:Object|]
	 */
  
  def getRecordTypeFromProcedureSignature(sig : String) : Type = {
    if(!isValidProcSig(sig)) throw new RuntimeException("wrong sig: " + sig)
    formatSigToTypeForm(sig.substring(2, sig.indexOf('.')))
  }
  
  /**
	 * get proc name from procedure signature. e.g. [|Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z|] -> [|java:lang:Object.equals|]
	 */
  
  def getProcedureNameFromProcedureSignature(sig : String) : String = {
	  val strs = sig.substring(3, sig.indexOf(":"))
	  "[|" + strs.replaceAll("\\/", ":").replaceAll("&lt;", "<").replaceAll("&gt;", ">").replaceAll(";", "") + "|]"
	}
  
  /**
	 * convert type string from signature style to type style. [Ljava/lang/Object; -> [|java:lang:Object|][]
	 */
	
	def formatSigToTypeForm(sig : String) : Type = {
	  if(!isValidTypeSig(sig)) throw new RuntimeException("wrong type sig: " + sig)
	  val (tmp, d) = getDimensionsAndRemoveArrayFromSig(sig)
    tmp match{
      case "B" => 	getType("[|byte|]", d)
      case "C" => 	getType("[|char|]", d)
      case "D" => 	getType("[|double|]", d)
      case "F" => 	getType("[|float|]", d)
      case "I" => 	getType("[|int|]", d)
      case "J" => 	getType("[|long|]", d)
      case "S" =>		getType("[|short|]", d)
      case "Z" =>		getType("[|boolean|]", d)
      case "V" =>		getType("[|void|]", d)
      case _ =>
        getType("[|" + tmp.substring(1, tmp.length() - 1).replaceAll("\\/", ":") + "|]", d)
    }
	}
	
	/**
   * input looks like [|java:lang:String|], output is Ljava/lang/String;
   * or 'B' | 'C' | 'D' | 'F' | 'I' | 'J' | 'S' | 'Z'
   */
  
  def formatTypeToSigForm(typ : String) : String = {
    if(!isValidType(typ)) throw new RuntimeException("wrong type: " + typ)
    val t = getTypeFromName(typ)
    val d = t.dimensions
    t.typ match{
      case "[|byte|]" => 		getTypeSig("B", d)
      case "[|char|]" => 		getTypeSig("C", d)
      case "[|double|]" => 	getTypeSig("D", d)
      case "[|float|]" => 	getTypeSig("F", d)
      case "[|int|]" => 		getTypeSig("I", d)
      case "[|long|]" => 		getTypeSig("J", d)
      case "[|short|]" =>		getTypeSig("S", d)
      case "[|boolean|]" =>	getTypeSig("Z", d)
      case "[|void|]" =>		"V"
      case _ =>
        getTypeSig("L" + t.typ.substring(2, t.typ.length() - 2).replaceAll(":", "/") + ";", d)
    }
  }
  
  /**
   * check whether it is a valid pilar type e.g. [|java:lang:String|] or [|java:lang:String|][]
   */
  
  def isValidType(typ : String) : Boolean = typ.startsWith("[|") && (typ.endsWith("|]") || typ.endsWith("[]"))
  
  /**
   * check whether it is a valid pilar proc signature e.g. [|Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z|]
   */
  
  def isValidProcSig(sig : String) : Boolean = sig.startsWith("[|") && sig.endsWith("|]") && sig.lastIndexOf('.') > 0
  
  /**
   * check whether it is a valid type signature e.g. Ljava/lang/Object
   */
  
  def isValidTypeSig(sig : String) : Boolean = !sig.startsWith("[|") && !sig.endsWith("|]")
  
  /**
   * input ("Ljava/lang/String;", 1) output "[Ljava/lang/String;"
   */
  
  protected def getTypeSig(sig : String, dimension : Int) : String = {
    val sb = new StringBuffer
    for(d <- 1 to dimension) sb.append("[")
    sb.append(sig)
    sb.toString().intern()
  }
  
  
  /**
   * input ("java.lang.Class", 1) output "[Ljava.lang.Class"
   */
  
  protected def getClassName(name : String, dimension : Int) : String = {
    val sb = new StringBuffer
    for(d <- 1 to dimension) sb.append("[")
    sb.append(name)
    sb.toString().intern()
  }
  
  /**
   * input ("[|java:lang:String|]", 1) output Type
   */
  
  protected def getType(typ : String, dimension : Int) : Type = new NormalType(typ, dimension)
  
  /**
   * input: "[|java:lang:String|][]"  output: ("[|java:lang:String|]", 1)
   */
  
  def getTypeFromName(name : String) : Type = {
    if(!isValidType(name)) throw new RuntimeException("wrong type: " + name)
    var d : Int = 0
    var tmp = name
    while(tmp.endsWith("[]")){
      d += 1
      tmp = tmp.substring(0, tmp.length() - 2)
    }
    new NormalType(tmp, d)
  }
  
  /**
   * get outer class name from inner class name. e.g. [|android:os:Handler$Callback|] -> [|android:os:Handler|]
   */
  
  def getOuterNameFrom(innerName : String) = {
    if(!isValidType(innerName) && innerName.lastIndexOf("$") <= 0) throw new RuntimeException("wrong innerName: " + innerName)
    innerName.substring(0, innerName.lastIndexOf("$")) + "|]"
  }
  
  /**
   * input: "[Ljava/lang/String;"  output: ("Ljava/lang/String;", 1)
   */
  
  def getDimensionsAndRemoveArrayFromSig(sig : String) : (String, Int) = {
    val d =
      if(sig.startsWith("["))
      	sig.lastIndexOf('[') - sig.indexOf('[') + 1
      else 0
    val tmp = sig.substring(sig.lastIndexOf('[') + 1)
    (tmp, d)
  }
  
  /**
	 * get sub-signature from signature. e.g. [|Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z|] -> equals:(Ljava/lang/Object;)Z
	 */
  
  def getSubSigFromProcSig(sig : String) : String = {
    if(!isValidProcSig(sig)) throw new RuntimeException("wrong procedure sig: " + sig)
    sig.substring(sig.lastIndexOf('.') + 1, sig.length() - 2)
  }
  
   /**
	 * get procedure signature from the owner record name and the procedure sub-signature. e.g. ( [|java:lang:Object|], equals:(Ljava/lang/Object;)Z ) -> [|Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z|] 
	 */
  
  def getSigFromOwnerAndProcSubSig(recordName: String, subSig : String) : String = {
      if(!isValidType(recordName)) throw new RuntimeException("given type is not a valid form: " + recordName)
      val recSig =  formatTypeToSigForm(recordName)
	  val sb = new StringBuffer
	  sb.append("[|" + recSig + "." + subSig + "|]")
	  sb.toString().intern()
  }
  
  
  /**
	 * signature of the field. e.g. [|java:lang:Throwable.stackState|] or @@[|java:lang:Enum.sharedConstantsCache|]
	 */
  
  def isValidFieldSig(sig : String) : Boolean = (sig.startsWith("[|") || sig.startsWith("@@[|")) && sig.endsWith("|]") && sig.lastIndexOf('.') > 0
  
  /**
   * get field name from field signature. e.g. [|java:lang:Throwable.stackState|] -> stackState
   */
  
  def getFieldNameFromFieldSignature(sig : String) : String = {
    if(!isValidFieldSig(sig)) throw new RuntimeException("given field signature is not a valid form: " + sig)
    else sig.substring(sig.lastIndexOf('.') + 1, sig.lastIndexOf("|]"))
  }
  
  /**
   * get record name from field signature. e.g. [|java:lang:Throwable.stackState|] -> [|java:lang:Throwable|]
   */
  
  def getRecordNameFromFieldSignature(sig : String) : String = {
    if(!isValidFieldSig(sig)) throw new RuntimeException("given field signature is not a valid form: " + sig)
    else{
      if(sig.indexOf("[]") > 0){
        sig.substring(sig.indexOf("[|"), sig.indexOf("[]")) + "|]" + sig.substring(sig.indexOf("[]"), sig.lastIndexOf('.'))
      } else {
      	sig.substring(sig.indexOf("[|"), sig.lastIndexOf('.')) + "|]"
      }
    }
  }
  
  /**
   * get record name from field signature. e.g. [|java:lang:Throwable.stackState|] -> [|java:lang:Throwable|]
   */
  
  def getRecordTypeFromFieldSignature(sig : String) : Type = {
    val recName = getRecordNameFromFieldSignature(sig)
    getTypeFromName(recName)
  }
  
  /**
	 * generate signature of this field. input: ("[|java:lang:Throwable|]", "stackState") output: "[|java:lang:Throwable.stackState|]"
	 */
	
	def generateFieldSignature(recordName : String, name : String, isStatic : Boolean) : String = {
	  if(!isValidType(recordName)) throw new RuntimeException("given type is not a valid form: " + recordName)
	  val sb = new StringBuffer
	  if(isStatic) sb.append("@@")
	  sb.append(recordName.substring(0, recordName.length() - 2) + "." + name + "|]")
	  sb.toString().intern()
	}
  
	
	  /**
	 * generate a procedure full name from a record name and proc short name. input: ("[|java:lang:Throwable|]", "foo") output: "[|java:lang:Throwable.foo|]"
	 */
	
	def generateProcName(recordName : String, name : String) : String = {
	  if(!isValidType(recordName)) throw new RuntimeException("given type is not a valid form: " + recordName)
	  val sb = new StringBuffer
	  sb.append(recordName.substring(0, recordName.length() - 2) + "." + name + "|]")
	  sb.toString().intern()
	}
	
	def isValidClassName(str : String) : Boolean = !str.startsWith("[|") && !str.contains(":")
	
	/**
   * input: "[Ljava.lang.String;"  output: ("[|java:lang:String|]", 1)
   * input: "[[I" output: ("[|int|]", 2)
   */
  
  def getDimensionsAndTypeFromClassName(name : String) : (String, Int) = {
    val d =
      if(name.startsWith("["))
      	name.lastIndexOf('[') - name.indexOf('[') + 1
      else 0
    val tmp =
	    if(d>0){
	      if(name.endsWith(";")){
	        "[|" + name.substring(name.lastIndexOf('[') + 2, name.lastIndexOf(';')).replaceAll("\\.", ":") + "|]"
	      }
	      else{
	        name.substring(name.lastIndexOf('[') + 1) match{
	          case "B" => 	"[|byte|]"
			      case "C" => 	"[|char|]"
			      case "D" => 	"[|double|]"
			      case "F" => 	"[|float|]"
			      case "I" => 	"[|int|]"
			      case "J" => 	"[|long|]"
			      case "S" =>		"[|short|]"
			      case "Z" =>		"[|boolean|]"
			      case "V" =>		"[|void|]"
	        }
	      }
	    } else {
	      "[|" + name.replaceAll("\\.", ":") + "|]"
	    }
    (tmp, d)
  }
	
	
	/**
	 * format java class name to amandriod type. 
	 * input: "com.example.activity.XActivity" 
	 * output: "[|com:example:acitivity:XActivity|]"
	 * input: "[[Lwfg.test.A;"
	 * output: "[|wfg:test:A|][][]"
	 */
	def formatClassNameToType(cName : String) : Type = {
	  if(!isValidClassName(cName)) throw new RuntimeException("given class name is not a valid form: " + cName)
	  val (tmp, d) = getDimensionsAndTypeFromClassName(cName)
    getType(tmp, d)
	}
	
	def formatClassNameToRecordName(cName : String) : String = {
	  formatClassNameToType(cName).name
	}
	
	/**
   * convert amandroid record name to java class name.
   *  e.g. [|java:lang:String|][] -> [Ljava.lang.String;   [|java:lang:String|] -> java.lang.String
   */
  def formatRecordNameToClassName(name : String) : String = {
    require(isValidType(name))
    val t = getTypeFromName(name)
    val d = t.dimensions
    if(d > 0){
	    t.typ match{
	      case "[|byte|]" => 		getClassName("B", d)
	      case "[|char|]" => 		getClassName("C", d)
	      case "[|double|]" => 	getClassName("D", d)
	      case "[|float|]" => 	getClassName("F", d)
	      case "[|int|]" => 		getClassName("I", d)
	      case "[|long|]" => 		getClassName("J", d)
	      case "[|short|]" =>		getClassName("S", d)
	      case "[|boolean|]" =>	getClassName("Z", d)
	      case _ =>
	        getClassName("L" + t.typ.substring(2, t.typ.length() - 2).replaceAll(":", ".") + ";", d)
	    }
    } else {
      t.typ match{
	      case "[|byte|]" => 		"byte"
	      case "[|char|]" => 		"char"
	      case "[|double|]" => 	"double"
	      case "[|float|]" => 	"float"
	      case "[|int|]" => 		"int"
	      case "[|long|]" => 		"long"
	      case "[|short|]" =>		"short"
	      case "[|boolean|]" =>	"boolean"
	      case "[|void|]" =>		"void"
	      case _ =>
	        t.typ.substring(2, t.typ.length() - 2).replaceAll(":", ".")
	    }
    }
  }
  
}