package org.sireum.amandroid.serialization

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.{read, write}
import org.sireum.util._
import org.sireum.jawa.JawaType
import org.sireum.jawa.JavaKnowledge

object JawaTypeSerializer extends CustomSerializer[JawaType](format => (
    {
      case jv: JValue =>
        implicit val formats = DefaultFormats
//        val pkg = (jv \ "pkg").extract[String]
//        val name = (jv \ "name").extract[String]
//        val unknown = (jv \ "unknown").extract[Boolean]
//        val d = (jv \ "dim").extract[Int]
//        val j = new JawaType(pkg + "." + name, d)
//        if(unknown) j.toUnknown
//        j
        val str = (jv \ "typ").extract[String]
        JavaKnowledge.getTypeFromName(str)
    },
    {
      case typ: JawaType =>
//        val bt = typ.baseType
//        val d = typ.dimensions
//        ("pkg" -> bt.packageName) ~
//        ("name" -> bt.name) ~
//        ("unknown" -> bt.unknown) ~
//        ("dim" -> d)
      ("typ" -> typ.jawaName)
    }
))

object JawaTypeKeySerializer extends CustomKeySerializer[JawaType](format => (
    {
      case str: String =>
        JavaKnowledge.getTypeFromName(str)
    }, {
      case typ: JawaType =>
        typ.jawaName
    }
))

object JawaTypeSerializerTest extends App {
  implicit val formats = Serialization.formats(NoTypeHints) + JawaTypeSerializer
  var typold = new JawaType("java.lang.Object", 2)
  typold = typold.toUnknown
  val ser = write(typold)
  println(ser)
  val typnew = read[JawaType](ser)
  println(typnew.baseType.name)
}