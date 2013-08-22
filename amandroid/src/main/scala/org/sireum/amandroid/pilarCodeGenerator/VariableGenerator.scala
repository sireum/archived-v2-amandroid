package org.sireum.amandroid.pilarCodeGenerator

import org.sireum.util._

class VariableGenerator {
	private var varMap : MMap[String, Int] = mmapEmpty
	def generate(typ : String) : String = {
	  var variable : String = ""
	  typ match {
	    case "int" => 
	      if(varMap.contains("int")) varMap("int") += 1
	      else varMap.put("int", 0)
	      variable = "i" + varMap("int")
	    case "boolean" => 
	      if(varMap.contains("boolean")) varMap("boolean") += 1
	      else varMap.put("boolean", 0)
	      variable = "z" + varMap("boolean")
	    case _ => 
	      if(varMap.contains("object")) varMap("object") += 1
	      else varMap.put("object", 0)
	      variable = "r" + varMap("object")
	  }
	  variable
	}
}