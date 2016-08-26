package net.lshift.typesetr
package writers

import cmd.Config
import parsers.Repr._

// TODO: rename
abstract class Writer {

  type N

  def writeToFile(node: Aux[N])(
    implicit logger: util.Logger, config: Config): Option[java.io.File]

}
