package net.lshift.typesetr.xml

/*
 * Represent short and full namespace information
 */
case class NameSpace(v: String, short: NameSpaceKey) {

  override def toString: String = short.toString

}

abstract class NameSpaces {

  def apply(key: NameSpaceKey): Option[NameSpace]

  def name: String

  protected def iterator: Iterable[(NameSpaceKey, NameSpace)]

  override def toString: String =
    iterator.map { e => s"$prefix:$name=$e" } mkString (" ")

  private final val prefix = "xmlns"

}
