package net.lshift.typesetr
package writers

import cmd.Config
import parsers.Repr._

abstract class Writer {

  type N

  def writeToFile(node: Aux[N], out: java.io.File)(
    implicit logger: util.Logger, config: Config): Boolean

}
