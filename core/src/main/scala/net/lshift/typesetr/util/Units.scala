package net.lshift.typesetr.util

abstract class Units(val name: String) {
  def v: Int
}

object Units {

  def default: Units = Centimeters(0)

  def parse(x: (Int, String)): Option[Units] = x._2 match {
    case Centimeters.name => Some(Centimeters(x._1))
    case Percentage.name  => Some(Percentage(x._1))
    case Inches.name      => Some(Inches(x._1))
    case Pt.name          => Some(Pt(x._1))
    case _                => None
  }

}

case class Centimeters(v: Int) extends Units(Centimeters.name)
object Centimeters {
  val name: String = "cm"
}

case class Percentage(v: Int) extends Units(Percentage.name)
object Percentage {
  val name: String = "%"
}

case class Inches(v: Int) extends Units(Inches.name)
object Inches {
  val name: String = "in"
}

case class Pt(v: Int) extends Units(Pt.name)
object Pt {
  val name: String = "pt"
}
