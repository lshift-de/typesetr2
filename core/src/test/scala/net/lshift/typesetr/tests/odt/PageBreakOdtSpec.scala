package net.lshift.typesetr
package tests
package odt

import net.lshift.typesetr.Differ
import net.lshift.typesetr.odt.OdtTestDiffer
import net.lshift.typesetr.tex.{ LatexFilesTestDiffer, LatexStringsTestDiffer }
import org.scalatest._
import java.io.File

import scala.language.postfixOps

class PageBreakOdtSpec extends OdtSpec {

  import OdtSpec._

  lazy val differ: Differ[File] = new LatexFilesTestDiffer()
  lazy val testRunner = new OdtOptimizerWithTexRunner(new File(s"$resources/styles"))

  "A page break in ODT " should "lead \\clearpage commands in .tex" in {
    val name = "page-break"
    val (input, spec) = testInputTex(name)
    val resultF = testRunner.run(input)

    assert(resultF.isRight, "Optimizing of the document failed")

    val result = differ.diff(spec, resultF.right.get)
    assert(result.isEmpty, result.getOrElse(""))
  }

}
