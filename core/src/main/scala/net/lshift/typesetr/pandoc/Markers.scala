package net.lshift.typesetr.pandoc

import net.lshift.typesetr.parsers.{ NodeFactory, Repr }

object Markers {

  private def env[T](name: String)(txt: => Seq[Repr.Aux[T]])(implicit factory: NodeFactory.Aux[T]): Seq[Repr.Aux[T]] = {
    (Repr.makeTextElem(s"$BeginEnv!$name!", synthetic = false)(factory.textNode(s"$BeginEnv!$name!"), factory) +:
      txt) :+
      Repr.makeTextElem(s"$EndEnv!$name!", synthetic = false)(factory.textNode(s"$EndEnv!$name!"), factory)
  }

  private def cmd[T](name: String)(txt: => Seq[Repr.Aux[T]])(implicit factory: NodeFactory.Aux[T]): Seq[Repr.Aux[T]] = {
    (Repr.makeTextElem(s"$BeginCmd!$name!", synthetic = false)(factory.textNode(s"$BeginCmd!$name!"), factory) +:
      txt) :+
      Repr.makeTextElem(s"$EndCmd!$name!", synthetic = false)(factory.textNode(s"$EndCmd!$name!"), factory)
  }

  def citationInBlockQuotation[T](txt: => Seq[Repr.Aux[T]])(implicit factory: NodeFactory.Aux[T]): Seq[Repr.Aux[T]] =
    cmd(RightAlign)(txt)

  final val RightAlign = "attrib"

  private final val BeginEnv = "TYPESETRENVSTART"

  private final val EndEnv = "TYPESETRENVEND"

  private final val BeginCmd = "TYPESETRCMDSTART"

  private final val EndCmd = "TYPESETRCMDEND"

  final val BeginEnvR = new scala.util.matching.Regex(s"$BeginEnv!(.*)!")

  final val EndEnvR = new scala.util.matching.Regex(s"$EndEnv!(.*)!")

  final val BeginCmdR = new scala.util.matching.Regex(s"$BeginCmd!(.*)!")

  final val EndCmdR = new scala.util.matching.Regex(s"$EndCmd!(.*)!")

}
