/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
package org.sireum.amandroid.serialization

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.{read, write}
import org.sireum.util._
import org.sireum.jawa.Signature

object SignatureSerializer extends CustomSerializer[Signature](format => (
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
        val str = (jv \ "sig").extract[String]
        new Signature(str)
    },
    {
      case sig: Signature =>
//        val bt = typ.baseType
//        val d = typ.dimensions
//        ("pkg" -> bt.packageName) ~
//        ("name" -> bt.name) ~
//        ("unknown" -> bt.unknown) ~
//        ("dim" -> d)
      ("sig" -> sig.signature)
    }
))
