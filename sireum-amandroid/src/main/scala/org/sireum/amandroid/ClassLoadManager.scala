package org.sireum.amandroid

import org.sireum.util._
import scala.collection.immutable.BitSet

object ClassLoadManager {
  /**
   * set of classes can be loaded by the program
   */
	private var classes : IList[AmandroidRecord] = ilistEmpty
	
	def reset = classes = ilistEmpty
	
	protected def addClass(clazz : AmandroidRecord) = {
	  this.classes :+= clazz
	}
	
	def getClassPosition(clazz : AmandroidRecord) : Int = {
	  if(!this.classes.contains(clazz)) addClass(clazz)
	  this.classes.indexOf(clazz)
	}
	
	def loadClass(clazz : AmandroidRecord) : BitSet = {
	  val position = getClassPosition(clazz)
	  BitSet(position)
	}
	
	def loadClass(clazz : AmandroidRecord, bitset : BitSet) : BitSet = {
	  require(!isLoaded(clazz, bitset))
	  val position = getClassPosition(clazz)
	  bitset + position
	}
	
	def isLoaded(clazz : AmandroidRecord, bitset : BitSet) : Boolean = {
	  val position = getClassPosition(clazz)
	  bitset(position)
	}
}