package net.lshift.typesetr
package odt

import net.lshift.typesetr.Differ
import java.io.{ IOException, File }

import org.xml.sax.SAXParseException

import scala.io.Source
import scala.xml.XML

class OdtTestDiffer extends Differ {
  def diff(spec: File, output: File): Option[String] = {
    // Spec is a content.xml file,
    // while the output is an .odt file.
    if (!spec.exists() || !output.exists()) Some("Invalid test resources")
    else
      try {
        (for {
          odtOutFile <- output.unpack().toRight(s"Cannot open file $output").right
          f2 <- odtOutFile._1.content.toRight(s"`content.xml` not found in $output").right
        } yield {
          val s1 = XML.loadFile(spec)
          val s2 = XML.loadFile(f2)

          compareNodes(s1, s2, 0)
        }) match {
          case Left(err) => Some(err)
          case Right(v)  => v
        }
      } catch {
        case ex: IOException =>
          Some("I/O error: " + ex.getMessage)
        case ex: SAXParseException =>
          ex.printStackTrace()
          Some("I/O XML error: " + ex.getMessage + ex)
      }
  }

  private def compareNodes(elem1: scala.xml.Node, elem2: scala.xml.Node, idx: Int): Option[String] = {
    if (elem1.prefix != elem2.prefix)
      Some(s"[$idx] incomparable nodes prefixes: ${elem1.prefix} vs ${elem2.prefix}")
    else if (elem1.label != elem2.label)
      Some(s"[$idx] incomparable nodes labels: ${elem1.label} vs ${elem2.label}")
    else if (elem1.attributes != elem2.attributes)
      Some(s"[$idx] incomparable nodes attributes: ${elem1.attributes} vs ${elem2.attributes}")
    else if (elem1.text != elem2.text)
      Some(s"[$idx] incomparable nodes attributes: ${elem1.text} vs ${elem2.text}")
    else seqToOpt((elem1.child zip elem2.child).flatMap { case (e1, e2) => compareNodes(e1, e2, idx + 1) })
  }

  private def seqToOpt(seq: Seq[String]): Option[String] = seq match {
    case Nil => None
    case _   => Some(seq.take(3).mkString("\n"))
  }
}
