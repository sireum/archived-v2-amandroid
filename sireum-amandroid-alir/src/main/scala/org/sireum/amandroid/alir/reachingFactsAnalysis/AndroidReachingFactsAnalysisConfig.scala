/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.alir.reachingFactsAnalysis

import org.sireum.jawa.util.Timer

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object AndroidReachingFactsAnalysisConfig {
	final var k_context = 1
	final var resolve_icc = true
	final var resolve_static_init = true
	
	/**
	 * Time out period. 0 means never. Other positive number means how many mins.
	 */
	final var timeout = 0
	final var parallel : Boolean = false
}