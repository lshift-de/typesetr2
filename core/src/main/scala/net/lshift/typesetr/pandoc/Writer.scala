package net.lshift.typesetr
package pandoc

import cmd.Config

abstract class Writer {

  def write(config: Config)(implicit logger: util.Logger): Unit

}
