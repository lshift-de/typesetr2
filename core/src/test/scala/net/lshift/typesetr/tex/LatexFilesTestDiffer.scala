package net.lshift.typesetr
package tex

import java.io.{ IOException, File }
import scala.io.Source

class LatexFilesTestDiffer extends Differ[File] {

  // spec and output are both .tex files
  def diff(spec: File, output: File): Option[String] = {
    val f1 = Source.fromFile(spec)
    val f2 = Source.fromFile(output)

    val ls1 = f1.getLines()
    val ls2 = f2.getLines()

    val cmp = (ls1 zip ls2) flatMap {
      case (l1, l2) =>
        if (l1 != l2) Some(s"$l1 differs from $l2")
        else None
    } take (2) toList

    cmp match {
      case Nil => None
      case _   => Some(cmp.mkString("\n"))
    }
  }

}

class LatexStringsTestDiffer extends Differ[String] {

  // spec and output are both .tex files
  def diff(spec: String, output: String): Option[String] = {

    val ls1 = spec.split('\n') toList
    val ls2 = output.split('\n') toList

    val cmp = (ls1 zip ls2) flatMap {
      case (l1, l2) =>
        if (l1 != l2) Some(s"$l1 differs from $l2")
        else None
    } take (2) toList

    cmp match {
      case Nil => None
      case _   => Some(cmp.mkString("\n"))
    }
  }

}
