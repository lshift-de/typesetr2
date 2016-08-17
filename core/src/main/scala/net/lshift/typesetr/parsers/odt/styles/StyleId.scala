package net.lshift.typesetr.parsers
package odt
package styles

case class StyleId(family: String, name: String) {

  def withThisFamily(name0: String) = StyleId(family, name0)

  override def toString: String = s"$family:$name"

}