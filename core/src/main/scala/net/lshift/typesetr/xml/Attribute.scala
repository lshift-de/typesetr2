package net.lshift.typesetr.xml

import scala.language.implicitConversions

case class Attribute(val name: String)

object Attribute {

  implicit def toString(x: Attribute): String = x.name

}
