package net.lshift.typesetr.xml

import scala.language.implicitConversions

case class AttributeKey(key: String)

object AttributeKey {

  implicit def toKey(key: String): AttributeKey =
    AttributeKey(key)

  implicit def toKeyFromTag(key: XmlTag): AttributeKey =
    AttributeKey(key)

  implicit class AttributeKeyOps(val key: AttributeKey) extends AnyVal {
    def in(keys: List[AttributeKey]): Boolean =
      keys.contains(key)

    def inAttributes(attrs: List[Attribute]): Option[Attribute] =
      attrs.find(_.key == key)
  }

}

sealed abstract class Attribute {

  def key: AttributeKey

  def value: Option[String]

}

case class AttributeWithVal(key: AttributeKey)(v: String) extends Attribute {

  def value: Option[String] = Some(v)

  override def toString: String = s"$key = $value"

}

case class SingletonAttribute(key: AttributeKey) extends Attribute {

  def value: Option[String] = None

}

object Attribute {

  def apply(name: AttributeKey): Attribute = SingletonAttribute(name)

  def apply(name: AttributeKey, v: String): Attribute = AttributeWithVal(name)(v)

  def unapply(x: Attribute): Option[(AttributeKey, String)] =
    for {
      v <- x.value
    } yield (x.key, v)

}
