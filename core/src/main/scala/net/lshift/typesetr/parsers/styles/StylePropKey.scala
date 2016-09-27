package net.lshift.typesetr.parsers.styles

import net.lshift.typesetr.xml.XmlAttribute
import net.lshift.typesetr.xml.attributes.StyleAttribute

import scala.language.implicitConversions

/**
  * Class representing a type information
  * of a single style property (a style `sheet` may
  * contain many style properties).
  */
abstract class StylePropKey { self =>

  // Type of the property's value
  type Result

  /**
    * The low-level xml attribute name that this style property
    * encapsulates.
    *
    * The attribute has a string value that needs to be
    * validated and translated into a first-class Scala object
    */
  def name: Option[XmlAttribute]

  // A reference to the self type.
  // V and Tag are only needed to define proper
  // heterogeneous maps.
  type V = self.type

  implicit val Tag: V

}

object StylePropKey {

  type Aux[T] = StylePropKey { type Result = T }

  type Of = StylePropKey { type Result <: StyleAttribute }

  // TODO: Figure out why the type inferencer goes
  // nuts when we use With[StyleAttribute] instead
  type With[+T] = StylePropKey { type Result <: T }

}
