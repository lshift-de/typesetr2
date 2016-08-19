package net.lshift.typesetr.parsers.odt.styles

import scala.language.implicitConversions

case class Props(textProps: Option[scala.xml.Node],
                 parProps: Option[scala.xml.Node],
                 tableProps: Option[scala.xml.Node])

object Props {

  type Aux = (Option[scala.xml.Node], Option[scala.xml.Node], Option[scala.xml.Node])

  implicit def toProps(ps: Aux): Props = Props(ps._1, ps._2, ps._3)

}
