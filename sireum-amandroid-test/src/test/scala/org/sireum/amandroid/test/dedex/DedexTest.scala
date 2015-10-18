/*
Copyright (c) 2015-2016 Fengguo Wei, University of South Florida.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.test.dedex

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.sireum.amandroid.example.dex.DexExamples
import org.sireum.amandroid.test.framework.dedex.DedexTestFramework

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
@RunWith(classOf[JUnitRunner])
class DedexTest extends DedexTestFramework {
  DexExamples.goodModelFiles.
//  filter { s => s.endsWith("test1.dex") }.
  foreach { resfile =>
    Analyzing title resfile file resfile
  }
}