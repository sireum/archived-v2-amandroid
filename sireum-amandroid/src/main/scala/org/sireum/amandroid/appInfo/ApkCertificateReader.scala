/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
package org.sireum.amandroid.appInfo

import org.sireum.util._
import java.io.FileInputStream
import org.sireum.amandroid.Apk
import java.util.jar.JarFile
import collection.JavaConversions._
import java.security.cert.X509Certificate
import java.io.InputStream
import java.security.cert.CertificateFactory
import java.security.Principal
import java.math.BigInteger
import java.util.Date
import java.security.cert.Certificate
import java.security.MessageDigest

case class ApkCertificate(
    owner: String, 
    issuer: String, 
    serialNumber: Int, 
    notBefore: Date,
    notAfter: Date,
    md5fp: String,
    sha1fp: String,
    sha256fp: String,
    algName: String,
    version: Int) {
  override def toString: String = {
    val sb: StringBuilder = new StringBuilder
    sb.append("Owner: " + owner + "\n")
    sb.append("Issuer:" + issuer + "\n")
    sb.append("Serial number: " + serialNumber.toHexString + "\n")
    sb.append("Valid from: " + notBefore + " until: " + notAfter + "\n")
    sb.append("Certificate fingerprints:\n")
    sb.append("\tMD5: " + md5fp + "\n")
    sb.append("\tSHA1: " + sha1fp + "\n")
    sb.append("\tSHA256: " + sha256fp + "\n")
    sb.append("Signature algorithm name: " + algName + "\n")
    sb.append("Version: " + version + "\n")
    sb.toString()
  }
}

object ApkCertificateReader {
  
  private def DEBUG = false
  
  def apply(fileUri: FileResourceUri): ISet[ApkCertificate] = {
    val apkcerts: MSet[ApkCertificate] = msetEmpty
    if(Apk.isValidApk(fileUri)) {
      val jf = new JarFile(FileUtil.toFile(fileUri), true)
      for(ent <- jf.entries()) {
        if(ent.getName == "META-INF/CERT.RSA") {
          val is = jf.getInputStream(ent)
          apkcerts ++= getCertFromStream(is)
        }
      }
    } else {
      val file = FileUtil.toFile(fileUri)
      val is = new FileInputStream(file)
      apkcerts ++= getCertFromStream(is)
    }
    apkcerts.toSet
  }
  
  private def getCertFromStream(is: InputStream): ISet[ApkCertificate] = {
    val apkcerts: MSet[ApkCertificate] = msetEmpty
    val cf = CertificateFactory.getInstance("X509")
    try {
      val c = cf.generateCertificates(is)
      for(cert <- c) {
        val x509cert = cert.asInstanceOf[X509Certificate]
        val md5fp = getCertFingerPrint("MD5", cert)
        val sha1fp = getCertFingerPrint("SHA1", cert)
        val sha256fp = getCertFingerPrint("SHA-256", cert)
        apkcerts += ApkCertificate(
            x509cert.getSubjectDN.getName,
            x509cert.getIssuerDN.getName,
            x509cert.getSerialNumber.intValue(),
            x509cert.getNotBefore,
            x509cert.getNotAfter,
            md5fp,
            sha1fp,
            sha256fp,
            x509cert.getSigAlgName,
            x509cert.getVersion)
      }
    } catch {
      case e: Exception =>
        if(DEBUG)
          e.printStackTrace()
    }
    apkcerts.toSet
  }
  
  private def getCertFingerPrint(mdAlg: String, cert: Certificate): String = {
    val encCertInfo = cert.getEncoded
    val md = MessageDigest.getInstance(mdAlg)
    val digest = md.digest(encCertInfo)
    toHexString(digest)
  }
  
  private def toHexString(block: Array[Byte]): String = {
    val buf = new StringBuffer()
    val len = block.length
    for (i <- 0 to len - 1) {
      byte2hex(block(i), buf)
      if (i < len-1) {
        buf.append(":")
      }
    }
    buf.toString()
  }
  
  private def byte2hex(b: Byte, buf: StringBuffer) = {
    val hexChars = Array(
      '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'
    )
    val high = ((b & 0xf0) >> 4)
    val low = (b & 0x0f)
    buf.append(hexChars(high))
    buf.append(hexChars(low))
  }
}