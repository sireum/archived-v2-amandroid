/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Main Contributors:
 *    Fengguo Wei - Argus Lab @ University of South Florida
 *    Sankardas Roy - Bowling Green State University
 *    
 * Contributors:
 *    Robby - Santos Lab @ Kansas State University
 *    Wu Zhou - Fireeye
 *    Fengchi Lin - Chinese People's Public Security University
 ******************************************************************************/
package org.sireum.amandroid.security

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object AndroidProblemCategories {
  
  /**
   * following are vulnerability categories
   */
	final val VUL_INFORMATION_LEAK = "vulnerability:information_leak"
	final val VUL_CAPABILITY_LEAK = "vulnerability:capability_leak"
	final val VUL_CONFUSED_DEPUTY = "vulnerability:confused_deputy"
	  
	/**
	 * following are maliciousness categories
	 */
	final val MAL_INFORMATION_LEAK = "maliciousness:information_theft"
}
