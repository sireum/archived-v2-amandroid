package org.sireum.amandroid.util

object StringFormConverter {

  /**
	 * get record name from procedure signature. e.g. [|Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z|] -> [|java:lang:Object|]
	 */
  
  def getRecordNameFromProcedureSignature(sig : String) : String = {
    if(!isValidProcSig(sig)) throw new RuntimeException("wrong sig: " + sig)
    formatSigToTypeForm(sig.substring(2, sig.indexOf('.')))
  }
  
  /**
	 * convert type string from signature style to type style. [Ljava/lang/Object; -> [|java:lang:Object|][]
	 */
	
	def formatSigToTypeForm(sig : String) : String = {
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
      case "V" =>		"[|void|]"
      case _ =>
        getTypeSig("[|" + tmp.substring(1, tmp.length() - 1).replaceAll("\\/", ":") + "|]", d)
    }
	}
	
	/**
   * input looks like [|java:lang:String|], output is Ljava/lang/String;
   * or 'B' | 'C' | 'D' | 'F' | 'I' | 'J' | 'S' | 'Z'
   */
  
  def formatTypeToSigForm(typ : String) : String = {
    if(!isValidType(typ)) throw new RuntimeException("wrong type: " + typ)
    val (tmp, d) = getDimensionsAndRemoveArrayFromType(typ)
    tmp match{
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
        getTypeSig("L" + tmp.substring(2, tmp.length() - 2).replaceAll(":", "/") + ";", d)
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
   * check whether it is a valid pilar proc signature e.g. [|Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z|]
   */
  
  def isValidTypeSig(sig : String) : Boolean = !sig.startsWith("[|") && !sig.endsWith("|]")
  
  /**
   * input ("Ljava/lang/String;", 1) output "[Ljava/lang/String;"
   */
  
  protected def getTypeSig(sig : String, dimension : Int) : String = {
    val sb = new StringBuffer
    for(d <- 0 to dimension) sb.append("[")
    sb.append(sig)
    sb.toString().intern()
  }
  
  /**
   * input ("[|java:lang:String|]", 1) output "[|java:lang:String|][]"
   */
  
  protected def getType(typ : String, dimension : Int) : String = {
    val sb = new StringBuffer
    sb.append(typ)
    for(d <- 0 to dimension) sb.append("[]")
    sb.toString().intern()
  }
  
  /**
   * input: "[|java:lang:String|][]"  output: ("[|java:lang:String|]", 1)
   */
  
  def getDimensionsAndRemoveArrayFromType(typ : String) : (String, Int) = {
    if(!isValidType(typ)) throw new RuntimeException("wrong type: " + typ)
    var d : Int = 0
    var tmp = typ
    while(tmp.endsWith("[]")){
      d += 1
      tmp = tmp.substring(0, tmp.length() - 2)
    }
    (tmp, d)
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
	 * signature of the field. e.g. [|java:lang:Throwable.stackState|]
	 */
  
  def isValidFieldSig(sig : String) : Boolean = sig.startsWith("[|") && (sig.endsWith("|]") || sig.endsWith("[]")) && sig.lastIndexOf('.') > 0
  
  /**
   * get field name from field signature. e.g. [|java:lang:Throwable.stackState|] -> stackState
   */
  
  def getFieldNameFromFieldSignature(sig : String) : String = {
    if(!isValidFieldSig(sig)) throw new RuntimeException("given field signature is not a valid form: " + sig)
    else sig.substring(sig.lastIndexOf('.') + 1, sig.length() - 2)
  }
  
  /**
   * get record name from field signature. e.g. [|java:lang:Throwable.stackState|] -> [|java:lang:Throwable|]
   */
  
  def getRecordNameFromFieldSignature(sig : String) : String = {
    if(!isValidFieldSig(sig)) throw new RuntimeException("given field signature is not a valid form: " + sig)
    else sig.substring(0, sig.lastIndexOf('.')) + "|]"
  }
  
  /**
	 * generate signature of this field. input: ("[|java:lang:Throwable|]", "stackState") output: "[|java:lang:Throwable.stackState|]"
	 */
	
	def generateFieldSignature(recordName : String, name : String) : String = {
	  if(!isValidType(recordName)) throw new RuntimeException("given type is not a valid form: " + recordName)
	  val sb = new StringBuffer
	  sb.append(recordName.substring(0, recordName.length() - 2) + "." + name + "|]")
	  sb.toString().intern()
	}
  
}