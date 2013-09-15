package org.sireum.amandroid.test.amandroidResolver

import org.sireum.amandroid.example.amandroidResolver.AmandroidResolverExamples
import org.sireum.amandroid.test.framework.amandroidResolver.AmandroidResolverTestFramework
import org.junit.runner._
import org.scalatest.junit.JUnitRunner
import org.sireum.amandroid.AmandroidCodeSource


/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
@RunWith(classOf[JUnitRunner])
class AmandroidResolverTest extends AmandroidResolverTestFramework {
  AmandroidCodeSource.preLoad
	AmandroidResolverExamples.pilarModelFiles.
//  filter { s => s.endsWith("/bigWfgNp.pilar") }.
  foreach { fileUri =>
    Analyzing title fileUri file fileUri
  }
	
}