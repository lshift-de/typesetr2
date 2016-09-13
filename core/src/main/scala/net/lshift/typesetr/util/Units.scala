package net.lshift.typesetr.util

sealed abstract class Units(val name: String) {
  def value: Double

  override def toString: String = s"$value$name"
}

object Units {

  def default: Units = Centimeters(0)

  def parse(x: (Double, String)): Option[Units] = x._2 match {
    case Centimeters.name => Some(Centimeters(x._1))
    case Percentage.name  => Some(Percentage(x._1))
    case Inches.name      => Some(Inches(x._1))
    case Pt.name          => Some(Pt(x._1))
    case _                => None
  }

}

case class Centimeters(value: Double) extends Units(Centimeters.name)
object Centimeters {
  val name: String = "cm"
}

case class Milimeters(value: Double) extends Units(Centimeters.name)
object Milimeters {
  val name: String = "mm"
}

case class Percentage(value: Double) extends Units(Percentage.name)
object Percentage {
  val name: String = "%"
}

case class Inches(value: Double) extends Units(Inches.name)
object Inches {
  val name: String = "in"
}

case class Pt(value: Double) extends Units(Pt.name)
object Pt {
  val name: String = "pt"
}
