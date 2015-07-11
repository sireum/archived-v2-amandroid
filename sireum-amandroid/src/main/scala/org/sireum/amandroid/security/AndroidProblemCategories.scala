/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.security

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object AndroidProblemCategories {
  
  /**
   * following are vulnerability categories
   */
  final val VUL_INFOMATION_LEAK = "vulnerability:infomation_leak"
  final val VUL_CAPABILITY_LEAK = "vulnerability:capability_leak"
  final val VUL_CONFUSED_DEPUTY = "vulnerability:confused_deputy"
  
  /**
   * following are maliciousness categories
   */
  final val MAL_INFOMATION_LEAK = "maliciousness:infomation_theft"
}