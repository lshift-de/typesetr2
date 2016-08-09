package net.lshift.typesetr
package parsers

import net.lshift.typesetr.parsers.Repr._
import net.lshift.typesetr.util
import cmd.Config

abstract class Writer {

  type N

  def writeToFile(node: Aux[N], out: java.io.File)(
    implicit logger: util.Logger, config: Config): Boolean

}
