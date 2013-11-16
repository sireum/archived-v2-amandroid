package org.sireum.amandroid.util

final case class ResourceRetriever(clazz : Class[_], path : String, name : String) {
	def getResourceStream = clazz.getResourceAsStream(path + name)
}