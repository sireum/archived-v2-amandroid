package org.sireum.amandroid.android.parser

import org.sireum.amandroid.AmandroidRecord

/**
 * Data class representing a layout control on the android screen
 * 
 * adapted from Steven Arzt
 * modified by: Fenguo Wei
 */
final case class LayoutControl(id : Int, viewClass : AmandroidRecord, isSensitive : Boolean = false) {
}