package org.sireum.amandroid

object MessageCenter {
	/**
	 * SIMPLE: Only print critical message
	 * NORMAL: Print some useful debug message
	 * DETAIL: Print all message
	 */
	object MSG_LEVEL extends Enumeration {
	  val SIMPLE, NORMAL, DETAIL = Value
	}
	var msglevel : MSG_LEVEL.Value = MSG_LEVEL.NORMAL
	implicit def msg_simple(msg : String) = {
	  if(msglevel >= MSG_LEVEL.SIMPLE)
	  	println("[SIMPLE]" + msg)
	}
	implicit def err_msg_simple(msg : String) = {
	  if(msglevel >= MSG_LEVEL.SIMPLE)
	  	System.err.println("[ERROR_SIMPLE]" + msg)
	}
	implicit def msg_normal(msg : String) = {
	  if(msglevel >= MSG_LEVEL.NORMAL)
	  	println("[NORMAL]" + msg)
	}
	implicit def err_msg_normal(msg : String) = {
	  if(msglevel >= MSG_LEVEL.NORMAL)
	  	System.err.println("[ERROR_NORMAL]" + msg)
	}
	implicit def msg_detail(msg : String) = {
	  if(msglevel >= MSG_LEVEL.DETAIL)
	  	println("[DETAIL]" + msg)
	}
	implicit def err_msg_detail(msg : String) = {
	  if(msglevel >= MSG_LEVEL.DETAIL)
	  	System.err.println("[ERROR_DETAIL]" + msg)
	}
}