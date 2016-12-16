package net.lshift.typesetr
package tests
package odt

import net.lshift.typesetr.Differ
import net.lshift.typesetr.odt.OdtTestDiffer
import net.lshift.typesetr.tex.LatexFilesTestDiffer
import org.scalatest._
import java.io.File

import scala.language.postfixOps

class TextOdtSpec extends OdtSpec {

  import OdtSpec._

  lazy val differ: Differ[File] = new OdtTestDiffer()
  lazy val texDiffer: Differ[File] = new LatexFilesTestDiffer
  lazy val testRunner = new OdtOptimizerRunner(new File(s"$resources/styles"))
  lazy val testTexRunner = new OdtOptimizerWithTexRunner(new File(s"$resources/styles"))

  "A footnote in a normal paragraph" should "be left without a change" in {
    val name = "basic-footnotes"
    val (input, spec) = testInputOdt(name)
    val resultF = testRunner.run(input)

    assert(resultF.isRight, "Optimizing of the document failed")

    val result = differ.diff(spec, resultF.right.get)
    assert(result.isEmpty, result.getOrElse(""))
  }

  "A mixed style text" should "be correctly parsed and written" in {
    val name = "text-mixed-styles"
    val (input, spec) = testInputTex(name)
    val resultF = testTexRunner.run(input)

    assert(resultF.isRight, "Optimizing of the document failed")

    val result = texDiffer.diff(spec, resultF.right.get)
    assert(result.isEmpty, result.getOrElse(""))
  }

}
