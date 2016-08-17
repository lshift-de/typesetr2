package net.lshift.typesetr.xml.attributes

/*
 * Trait shared by all style attributes that are analyzed in Typesetr.
 */
trait StyleAttribute {

  def name: String

  abstract override def toString: String = name

}