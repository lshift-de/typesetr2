package net.lshift.typesetr.pandoc

import net.lshift.typesetr.parsers.{ NodeFactory, Repr }

import scala.xml.Text

object Markers {

  private[this] var counter = 0

  private[this] def increment(): Int = {
    val res = counter
    counter = counter + 1
    res
  }

  private def wrap[T](name: String, beginName: String, endName: String)(txt: => Seq[Repr.Aux[T]])(implicit factory: NodeFactory.Aux[T]): Seq[Repr.Aux[T]] = {
    val name1 = name + "-" + increment()
    (Repr.makeTextElem(s"$beginName!$name1!", synthetic = false)(factory.textNode(s"$beginName!$name1!"), factory) +:
      txt) :+
      Repr.makeTextElem(s"$endName!$name1!", synthetic = false)(factory.textNode(s"$endName!$name1!"), factory)
  }

  private def env[T](name: String)(txt: => Seq[Repr.Aux[T]])(implicit factory: NodeFactory.Aux[T]): Seq[Repr.Aux[T]] =
    wrap(name, BeginEnv, EndEnv)(txt)

  private def cmd[T](name: String)(txt: => Seq[Repr.Aux[T]])(implicit factory: NodeFactory.Aux[T]): Seq[Repr.Aux[T]] =
    wrap(name, BeginCmd, EndCmd)(txt)

  private def verbatim[T](name: String)(txt: => Seq[Repr.Aux[T]])(implicit factory: NodeFactory.Aux[T]): Seq[Repr.Aux[T]] =
    wrap(name, BeginFormat, EndFormat)(txt)

  def citationInBlockQuotation[T](txt: => Seq[Repr.Aux[T]])(implicit factory: NodeFactory.Aux[T]): Seq[Repr.Aux[T]] =
    cmd(RightAlign)(txt)

  def formatBlock[T](txt: => Seq[Repr.Aux[T]])(implicit factory: NodeFactory.Aux[T]): Seq[Repr.Aux[T]] =
    verbatim(Verbatim)(txt)

  final val RightAlign = "attrib"
  final val Verbatim = "verbatim"

  private final val BeginEnv = "TYPESETRENVSTART"

  private final val EndEnv = "TYPESETRENVEND"

  private final val BeginCmd = "TYPESETRCMDSTART"

  private final val EndCmd = "TYPESETRCMDEND"

  private final val BeginFormat = "TYPESETRPRESTART"

  private final val EndFormat = "TYPESETRPREEND"

  final val groupStartName = "group-start"
  final val groupName = "content"
  final val groupEndName = "group-end"

  final val EnvR = new scala.util.matching.Regex(s"(?s)$BeginEnv!(.*)-(\\d*)!(.*)$EndEnv!(.*)-\\2!", groupStartName, "id", groupName, groupEndName)

  final val CmdR = new scala.util.matching.Regex(s"(?s)$BeginCmd!(.*)-(\\d*)!(.*)$EndCmd!(.*)-\\2!", groupStartName, "id", groupName, groupEndName)

  final val PreR = new scala.util.matching.Regex(s"(?s)${BeginFormat}!$Verbatim-(\\d*)!(.*)${EndFormat}!$Verbatim-\\1!", "id", groupName)

}
