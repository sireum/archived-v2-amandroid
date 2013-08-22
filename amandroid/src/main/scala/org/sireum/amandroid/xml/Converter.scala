package org.sireum.amandroid.xml

import scala.collection.JavaConversions._
import org.sireum.util.converter.java._
import org.sireum.util._
import com.thoughtworks.xstream.mapper.Mapper
import com.thoughtworks.xstream.converters.collections.AbstractCollectionConverter
import com.thoughtworks.xstream.io.HierarchicalStreamWriter
import com.thoughtworks.xstream.converters.MarshallingContext
import com.thoughtworks.xstream.io.HierarchicalStreamReader
import com.thoughtworks.xstream.converters.UnmarshallingContext
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashMap
import org.jgrapht.util.ArrayUnenforcedSet
import com.google.common.collect.HashBiMap

object Converter {
  def javafy(o : Any)(
    implicit seen : MIdMap[AnyRef, Object]) : Object =
    o match {
      case b : Boolean                       => boolean2Boolean(b)
      case b : Byte                          => byte2Byte(b)
      case c : Char                          => char2Character(c)
      case s : Short                         => short2Short(s)
      case i : Int                           => int2Integer(i)
      case l : Long                          => long2Long(l)
      case f : Float                         => float2Float(f)
      case d : Double                        => double2Double(d)
      case ii : org.sireum.util.math.Integer => ii.toBigInt.bigInteger
      case s : String                        => s
      case e : java.lang.Enum[_]             => e
      case null                              => null
      case None                              => ScalaOption.None
      case Some(o)                           => new ScalaOption(javafy(o))
      case o : AnyRef =>
        if (seen.contains(o)) seen(o)
        else {
          o match {
            case m : scala.collection.Map[_, _] =>
              val size = m.size
              val elements = new Array[Object](size)
              import AndroidScalaCollectionType._
              val result =
                m match {
                  case m : MLinkedMap[_, _] =>
                    new AndroidScalaCollection(MLinkMap, elements)
                  case m : MMap[_, _] =>
                    new AndroidScalaCollection(MMap, elements)
                }
              seen(o) = result
              var i = 0
              for (e <- m) {
                elements(i) = new ScalaPair(javafy(e._1), javafy(e._2))
                i += 1
              }
              result
            case t : scala.collection.Traversable[_] =>
              val size = t.size
              val elements = new Array[Object](size)
              import AndroidScalaCollectionType._
              val result =
                t match {
                  case t : MList[_] => 
                    new AndroidScalaCollection(MList, elements)
                  case t : MBuffer[_] => 
                    new AndroidScalaCollection(MBuffer, elements)
                  case t : MSet[_] => 
                    new AndroidScalaCollection(MSet, elements)
                  case t : ISeq[_] => 
                    new AndroidScalaCollection(ISeq, elements)
                }
              var i = 0
              for (e <- t) {
                elements(i) = javafy(e)
                i += 1
              }
              result
            case p : Product with PropertyProvider =>
              val elementSize = p.productArity
              val elements = new Array[Object](elementSize)
              val result = new ScalaProductWithProperty(p.getClass,
                elements, null)
              seen(o) = result
              for (i <- 0 until elementSize) {
                elements(i) = javafy(p.productElement(i))
              }
              result
            case p : Product =>
              val elementSize = p.productArity
              val elements = new Array[Object](elementSize)
              val result = new ScalaProduct(p.getClass, elements)
              seen(o) = result
              for (i <- 0 until elementSize) {
                elements(i) = javafy(p.productElement(i))
              }
              result
//            case _ => o
          }
        }
    }

  def scalafy(o : Object)(implicit seen : MIdMap[Object, AnyRef]) : Any =
    o match {
      case b : java.lang.Boolean     => b
      case b : java.lang.Byte        => b
      case c : java.lang.Character   => c
      case s : java.lang.Short       => s
      case i : java.lang.Integer     => i
      case l : java.lang.Long        => l
      case f : java.lang.Float       => f
      case d : java.lang.Double      => d
      case ii : java.math.BigInteger => org.sireum.util.math.SireumNumber(ii)
      case s : String                => s
      case e : java.lang.Enum[_]     => e
      case null                      => null
      case o : ScalaOption =>
        if (o.obj == null) None
        else Some(scalafy(o.obj))
      case o : AnyRef =>
        if (seen.contains(o)) seen(o)
        else
          o match {
            case c : AndroidScalaCollection =>
              val es = c.elements.map(scalafy)
              import AndroidScalaCollectionType._
              import scala.collection.mutable._
              c.typ match {
                case MList  => ListBuffer(es : _*)
                case MBuffer   => Buffer(es : _*)
                case MSet => Set(es : _*)
                case ISeq => scala.collection.immutable.Seq(es : _*)
                case MLinkMap =>
                  Map(es.map { o =>
                    val sp = o.asInstanceOf[ScalaPair]
                    (sp.first, sp.second)
                  } : _*)
                case MMap =>
                  Map(es.map { o =>
                    val sp = o.asInstanceOf[ScalaPair]
                    (sp.first, sp.second)
                  } : _*)
              }
            case p : ScalaProduct =>
              val es = p.elements
              val size = es.size
              val elements = new Array[Object](size)
              for (i <- 0 until size) {
                elements(i) = scalafy(es(i)).asInstanceOf[Object]
              }
              val result = ProductUtil.make(p.pclass, elements : _*)
              seen(o) = result.asInstanceOf[Object]
              p match {
                case p : ScalaProductWithProperty =>
                  val pp = result.asInstanceOf[PropertyProvider]
                  for (sp <- p.properties) {
                    pp(sp.first) = sp.second
                    if (sp.second.isInstanceOf[PropertyProviderContext[_]])
                      sp.second.
                        asInstanceOf[PropertyProviderContext[PropertyProvider]].
                        context(pp)
                  }
                case _ =>
              }
              result
          }
    }
}

class ArrayUnenforcedSetConverter( _mapper : Mapper )  extends AbstractCollectionConverter(_mapper) {
  /** Helper method to use x.getClass
   * 
   */
  def getAnyClass(x: Any) = x.asInstanceOf[AnyRef].getClass

  def canConvert( clazz: Class[_]) = {       
    classOf[ArrayUnenforcedSet[Any]] == clazz
  }

  def marshal( value: Any, writer: HierarchicalStreamWriter, context: MarshallingContext) = {
    val set = value.asInstanceOf[ArrayUnenforcedSet[Any]]
    val it = set.iterator()
    while(it.hasNext())
      writeItem(it.next(), context, writer)
  }

  def unmarshal( reader: HierarchicalStreamReader, context: UnmarshallingContext ) = {
//    println(context.getRequiredType())
    var set = createCollection(context.getRequiredType()).asInstanceOf[ArrayUnenforcedSet[Any]]
    while (reader.hasMoreChildren()) {
      reader.moveDown();
      val item = readItem(reader, context, set);
      set.add(item)
      reader.moveUp();
    }
    set
  }
}

class MMapConverter( _mapper : Mapper )  extends AbstractCollectionConverter(_mapper) {
  /** Helper method to use x.getClass
   * 
   */
  def getAnyClass(x: Any) = x.asInstanceOf[AnyRef].getClass

  def canConvert( clazz: Class[_]) = {       
    classOf[HashMap[Any, Any]] == clazz
  }

  def marshal( value: Any, writer: HierarchicalStreamWriter, context: MarshallingContext) = {
    val mmap = value.asInstanceOf[HashMap[Any, Any]]
    for ( (key, value) <- mmap ) {  
      writer.startNode("item")
      writeItem(key, context, writer)
      writeItem(value, context, writer)
      writer.endNode()
    }
  }

  def unmarshal( reader: HierarchicalStreamReader, context: UnmarshallingContext ) = {
//    println(context.getRequiredType())
    val mmap : MMap[Any, Any] = mmapEmpty
    while (reader.hasMoreChildren()) {
      var key : Any = null
      var value : Any = null
      reader.moveDown()
      reader.moveDown()
      key = readItem(reader, context, key)
      reader.moveUp()
      reader.moveDown()
      value = readItem(reader, context, value)
      reader.moveUp()
      reader.moveUp()
      mmap(key) = value
    }
    mmap
  }
}

class LinkedHashMapConverter( _mapper : Mapper )  extends AbstractCollectionConverter(_mapper) {
  /** Helper method to use x.getClass
   * 
   */
  def getAnyClass(x: Any) = x.asInstanceOf[AnyRef].getClass

  def canConvert( clazz: Class[_]) = {       
    classOf[LinkedHashMap[Any, Any]] == clazz
  }

  def marshal( value: Any, writer: HierarchicalStreamWriter, context: MarshallingContext) = {
    val mmap = value.asInstanceOf[LinkedHashMap[Any, Any]]
    for ( (key, value) <- mmap ) {  
      writer.startNode("item")
      writeItem(key, context, writer)
      writeItem(value, context, writer)
      writer.endNode()
    }
  }

  def unmarshal( reader: HierarchicalStreamReader, context: UnmarshallingContext ) = {
//    println(context.getRequiredType())
    var lmap = mlinkedMapEmpty[Any, Any]
    while (reader.hasMoreChildren()) {
      var key : Any = null
      var value : Any = null
      reader.moveDown()
      reader.moveDown()
      key = readItem(reader, context, key)
      reader.moveUp()
      reader.moveDown()
      value = readItem(reader, context, value)
      reader.moveUp()
      reader.moveUp()
      lmap(key) = value
    }
    lmap
  }
}

class HashBiMapConverter( _mapper : Mapper )  extends AbstractCollectionConverter(_mapper) {
  /** Helper method to use x.getClass
   * 
   */
  def getAnyClass(x: Any) = x.asInstanceOf[AnyRef].getClass

  def canConvert( clazz: Class[_]) = {       
    classOf[HashBiMap[Any, Any]] == clazz
  }

  def marshal( value: Any, writer: HierarchicalStreamWriter, context: MarshallingContext) = {
    val hbm = value.asInstanceOf[HashBiMap[Any, Any]]
    for ( (key, value) <- hbm ) {
      writer.startNode("entry")
      writeItem(key, context, writer)
      writeItem(value, context, writer)
      writer.endNode()
    }
  }

  def unmarshal( reader: HierarchicalStreamReader, context: UnmarshallingContext ) = {
    val hbm = HashBiMap.create[Any, Any]()
    while (reader.hasMoreChildren()) {
      var key : Any = null
      var value : Any = null
      reader.moveDown()
      reader.moveDown()
      key = readItem(reader, context, key)
      reader.moveUp()
      reader.moveDown()
      value = readItem(reader, context, value)
      reader.moveUp()
      reader.moveUp()
      hbm(key) = value
    }
    hbm
  }
}
//
//  def marshal( value: Any, writer: HierarchicalStreamWriter, context: MarshallingContext) = {
//    val rda = value.asInstanceOf[ReachingDefinitionAnalysis.Result]
//    writer.startNode("entrySet")
//    writeItem(rda.entrySet, context, writer)
//    writer.endNode()
//  }
//
//  def unmarshal( reader: HierarchicalStreamReader, context: UnmarshallingContext ) = {
////    println(context.getRequiredType())
//    val rda : ReachingDefinitionAnalysis.Result = new ReachingDefinitionAnalysis.Result
//    while (reader.hasMoreChildren()) {
//      var key : Any = null
//      var value : Any = null
//      reader.moveDown()
//      reader.moveDown()
//      key = readItem(reader, context, key)
//      reader.moveUp()
//      reader.moveDown()
//      value = readItem(reader, context, value)
//      reader.moveUp()
//      reader.moveUp()
//      mmap(key) = value
//    }
//    mmap
//  }
//}