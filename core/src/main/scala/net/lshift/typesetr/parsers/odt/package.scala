package net.lshift.typesetr
package parsers

import odt.styles.StyleId
import styles.MetaFromDocument
import net.lshift.typesetr.xml.NameSpaces
import scala.xml.Node
import xml.{ Tag, Attribute }

package object odt {

  private val ns: NameSpaces = OdtNameSpaces(Map(
    "style" -> "urn:oasis:names:tc:opendocument:xmlns:style:1.0",
    "text" -> "urn:oasis:names:tc:opendocument:xmlns:text:1.0",
    "office" -> "urn:oasis:names:tc:opendocument:xmlns:office:1.0",
    "draw" -> "urn:oasis:names:tc:opendocument:xmlns:drawing:1.0",
    "table" -> "urn:oasis:names:tc:opendocument:xmlns:table:1.0",

    // openoffice crudified standard namespaces
    "fo" -> "urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0",
    "svg" -> "urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0",

    // general standards
    "dc" -> "http://purl.org/dc/elements/1.1/",
    "xlink" -> "http://www.w3.org/1999/xlink",
    "xml" -> "http://www.w3.org/XML/1998/namespace"))

  implicit val defaultNS: NameSpaces = ns

  implicit lazy val nodeEmptyBuilder: ReprNullFactory[scala.xml.Node] =

    new ReprNullFactory[Node] {

      def empty(): Repr.Aux[Node] = new Repr { self =>

        type R = scala.xml.Node

        type BodyTpe = Repr.Aux[Node]

        def source: R = ???

        def body: Seq[self.BodyTpe] = Nil

        def tag: Tag = Tag.nodeTag

        def contents: Option[String] = None

        def attr: List[Attribute] = Nil
      }

    }

}
