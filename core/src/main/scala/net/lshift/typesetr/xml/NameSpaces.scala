package net.lshift.typesetr.xml

case class NameSpace(v: String)

abstract class NameSpaces {
  def apply(key: NameSpaceKey): Option[NameSpace]
  def name: String
  protected def iterator: Iterable[(NameSpaceKey, NameSpace)]
  override def toString: String =
    iterator.map { e => s"$prefix:$name=$e" } mkString (" ")

  private final val prefix = "xmlns"
}
