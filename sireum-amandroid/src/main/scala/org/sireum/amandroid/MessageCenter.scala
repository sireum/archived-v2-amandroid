package org.sireum.amandroid

object MessageCenter {
	/**
	 * CRITICAL: Only print critical message
	 * NORMAL: Print some useful debug message
	 * DETAIL: Print all message
	 */
	object MSG_LEVEL extends Enumeration {
	  val NO, CRITICAL, NORMAL, DETAIL = Value
	}
	var msglevel : MSG_LEVEL.Value = MSG_LEVEL.CRITICAL

	implicit def msg_critical(msg : String) = {
	  if(msglevel >= MSG_LEVEL.CRITICAL)
	  	println("[CRITICAL]" + msg)
	}
	implicit def err_msg_critical(msg : String) = {
	  if(msglevel >= MSG_LEVEL.CRITICAL)
	  	System.err.println("[ERROR_CRITICAL]" + msg)
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