package net.lshift.typesetr

import scala.language.implicitConversions

package object parsers {
  implicit def toIntOps(x: Int): IntegerOps = new IntegerOps(x)

  class IntegerOps(val x: Int) extends AnyVal {
    def increment: Int = x + 1
  }
}
