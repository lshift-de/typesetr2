package net.lshift.typesetr.util

import scala.language.implicitConversions

/**
 * A value along with its unit.
 *
 * The values of attributes are accompanied by their units.
 *
 * @param human-readable name of the unit
 */
sealed abstract class ValOfUnit(val name: String) {

  /**
   * The underlying value represented as a double value
   */
  def value: Double

  /**
   * Human-readable value rounded to an integer along with the unit
   */
  def roundValue: String = s"${value.toInt}$name"

  /**
   * Convert value to centimeters
   * @return
   */
  def toCm: Double

  override def toString: String = s"$value$name"

}

object ValOfUnit {

  def default: ValOfUnit = Centimeters(0)

  def parse(x: (Double, String)): Option[ValOfUnit] = x._2 match {
    case Centimeters.name => Some(Centimeters(x._1))
    case Percentage.name  => Some(Percentage(x._1))
    case Inches.name      => Some(Inches(x._1))
    case Pt.name          => Some(Pt(x._1))
    case _                => None
  }

  implicit def toAttributeValue(x: ValOfUnit): String = x.roundValue

}

case class Centimeters(value: Double) extends ValOfUnit(Centimeters.name) {
  def toCm: Double = value
}
object Centimeters {
  val name: String = "cm"
}

case class Milimeters(value: Double) extends ValOfUnit(Centimeters.name) {
  def toCm: Double = value / 100
}
object Milimeters {
  val name: String = "mm"
}

case class Percentage(value: Double) extends ValOfUnit(Percentage.name) {
  def toCm: Double = ???
}
object Percentage {
  val name: String = "%"
}

case class Inches(value: Double) extends ValOfUnit(Inches.name) {
  def toCm: Double = value * 2.54
}
object Inches {
  val name: String = "in"
}

case class Pt(value: Double) extends ValOfUnit(Pt.name) {
  def toCm: Double = ???
}
object Pt {
  val name: String = "pt"
}
