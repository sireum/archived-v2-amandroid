package org.sireum.amandroid.parser

/**
 * Data class representing a layout control on the android screen
 * 
 * adapted from Steven Arzt
 * modified by: Fenguo Wei
 */
final case class LayoutControl(id : Int, viewClass : String, isSensitive : Boolean = false) {
}