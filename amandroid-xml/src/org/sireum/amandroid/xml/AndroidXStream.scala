package org.sireum.amandroid.xml

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.xml.Xpp3Driver
import com.google.common.collect.HashBiMap
import org.sireum.util._
import java.io._

object AndroidXStream {
  
  val xstream = {
    import org.sireum.util.converter.java._

    val r = new XStream(new Xpp3Driver())
    r.setMode(XStream.ID_REFERENCES)
    r.alias("collection", classOf[ScalaCollection])
    r.alias("option", classOf[ScalaOption])
    r.alias("pair", classOf[ScalaPair])
    r.alias("product", classOf[ScalaProduct])
    r.alias("productp", classOf[ScalaProductWithProperty])

    r.alias("single", classOf[Tuple1[_]])
    r.alias("double", classOf[Tuple2[_, _]])
    r.alias("triple", classOf[Tuple3[_, _, _]])
    r.alias("quadruple", classOf[Tuple4[_, _, _, _]])
    r.alias("quintuple", classOf[Tuple5[_, _, _, _, _]])
    r.alias("sextuple", classOf[Tuple6[_, _, _, _, _, _]])
    r.alias("septuple", classOf[Tuple7[_, _, _, _, _, _, _]])
    r.alias("octuple", classOf[Tuple8[_, _, _, _, _, _, _, _]])
    r.alias("nonuple", classOf[Tuple9[_, _, _, _, _, _, _, _, _]])
    r.alias("decuple", classOf[Tuple10[_, _, _, _, _, _, _, _, _, _]])
    r.alias("undecuple", classOf[Tuple11[_, _, _, _, _, _, _, _, _, _, _]])
    r.alias("duodecuple", classOf[Tuple12[_, _, _, _, _, _, _, _, _, _, _, _]])
    
    r.useAttributeFor(classOf[Class[_]])

//    r.registerConverter(new SymbolTableConverter())

    r
  }

  def toXml(o : Any, w : Writer) = xstream.toXML(o, w)

  def fromXml(o : File) = xstream.fromXML(o)

  private def javafy(o : Any) = Converter.javafy(o)(idmapEmpty)
  private def scalafy(o : Object) = Converter.scalafy(o)(idmapEmpty)

}