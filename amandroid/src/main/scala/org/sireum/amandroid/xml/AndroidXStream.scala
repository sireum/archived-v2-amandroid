package org.sireum.amandroid.xml

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.xml.Xpp3Driver
import org.sireum.util._
import java.io._

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object AndroidXStream {
  
  val xstream = {
    val r = new XStream(new Xpp3Driver())
    r.setMode(XStream.ID_REFERENCES)
//    r.alias("collection", classOf[ScalaCollection])
//    r.alias("option", classOf[ScalaOption])
//    r.alias("pair", classOf[ScalaPair])
//    r.alias("product", classOf[ScalaProduct])
//    r.alias("productp", classOf[ScalaProductWithProperty])
//
//    r.alias("single", classOf[Tuple1[_]])
//    r.alias("double", classOf[Tuple2[_, _]])
//    r.alias("triple", classOf[Tuple3[_, _, _]])
//    r.alias("quadruple", classOf[Tuple4[_, _, _, _]])
//    r.alias("quintuple", classOf[Tuple5[_, _, _, _, _]])
//    r.alias("sextuple", classOf[Tuple6[_, _, _, _, _, _]])
//    r.alias("septuple", classOf[Tuple7[_, _, _, _, _, _, _]])
//    r.alias("octuple", classOf[Tuple8[_, _, _, _, _, _, _, _]])
//    r.alias("nonuple", classOf[Tuple9[_, _, _, _, _, _, _, _, _]])
//    r.alias("decuple", classOf[Tuple10[_, _, _, _, _, _, _, _, _, _]])
//    r.alias("undecuple", classOf[Tuple11[_, _, _, _, _, _, _, _, _, _, _]])
//    r.alias("duodecuple", classOf[Tuple12[_, _, _, _, _, _, _, _, _, _, _, _]])
    
    r.useAttributeFor(classOf[Class[_]])
//    r.alias("list", classOf[::[_]])
//    r.alias("mmap", classOf[HashMap[Any, Any]])
    r.registerConverter(new ArrayUnenforcedSetConverter(r.getMapper()))
    r.registerConverter(new MMapConverter(r.getMapper()))
    r.registerConverter(new LinkedHashMapConverter(r.getMapper()))
    r.registerConverter(new HashBiMapConverter(r.getMapper()))
    
    r
  }

  def toXml(o : Any) = xstream.toXML(o)
  def toXml(o : Any, w : OutputStream) = xstream.toXML(o, w)

  def fromXml(o : File) = xstream.fromXML(o)
  def fromXml(o : InputStream) = xstream.fromXML(o)

  private def javafy(o : Any) = Converter.javafy(o)(idmapEmpty)
  private def scalafy(o : Object) = Converter.scalafy(o)(idmapEmpty)

}