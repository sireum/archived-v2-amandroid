package org.sireum.amandroid.android.parser

import org.sireum.jawa.JawaRecord

/**
 * Data class representing a layout control on the android screen
 * 
 * adapted from Steven Arzt
 * modified by: Fenguo Wei
 */
final case class LayoutControl(id : Int, viewClass : JawaRecord, isSensitive : Boolean = false) {
}