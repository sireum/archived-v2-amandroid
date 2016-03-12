/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
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
//  filter { s => s.endsWith("exceptiontest.dex") }.
  foreach { resfile =>
    Analyzing title resfile file resfile
  }
}
