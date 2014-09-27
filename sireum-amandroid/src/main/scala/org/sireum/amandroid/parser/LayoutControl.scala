/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.parser

import org.sireum.jawa.JawaRecord

/**
 * Data class representing a layout control on the android screen
 * 
 * adapted from Steven Arzt
 * modified by: Fenguo Wei
 */
final case class LayoutControl(id : Int, viewClass : JawaRecord, isSensitive : Boolean = false) {
}