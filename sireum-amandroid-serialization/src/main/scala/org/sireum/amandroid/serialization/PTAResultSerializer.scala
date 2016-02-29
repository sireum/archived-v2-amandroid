/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Main Contributors:
 *    Fengguo Wei - Argus Lab @ University of South Florida
 *    Sankardas Roy - Bowling Green State University
 *    
 * Contributors:
 *    Robby - Santos Lab @ Kansas State University
 *    Wu Zhou - Fireeye
 *    Fengchi Lin - Chinese People's Public Security University
 ******************************************************************************/
package org.sireum.amandroid.serialization

import org.json4s._
import org.json4s.JsonDSL._
import org.sireum.jawa.alir.pta.PTAResult
import org.sireum.jawa.alir.pta.PTAResult._
import org.sireum.util._
import org.sireum.jawa.alir.Context

object PTAResultSerializer extends CustomSerializer[PTAResult](format => (
    {
      case jv: JValue =>
        implicit val formats = format + PTASlotKeySerializer + InstanceSerializer
        val pointsToMap = (jv \ "pointsToMap").extract[IMap[String, PTSMap]]
        val ptaresult = new PTAResult
        ptaresult.addPointsToMap(pointsToMap)
        ptaresult
    }, {
      case ptaresult: PTAResult =>
        implicit val formats = format + PTASlotKeySerializer + InstanceSerializer
        ("pointsToMap" -> Extraction.decompose(ptaresult.pointsToMap))
    }
))
