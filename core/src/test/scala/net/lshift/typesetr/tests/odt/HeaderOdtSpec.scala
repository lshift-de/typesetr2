package net.lshift.typesetr
package tests
package odt

import net.lshift.typesetr.Differ
import net.lshift.typesetr.odt.OdtTestDiffer
import org.scalatest._
import java.io.File

import scala.language.postfixOps

class HeaderOdtSpec extends OdtSpec {

  import OdtSpec._

  lazy val differ: Differ = new OdtTestDiffer()
  lazy val testRunner = new OdtOptimizerRunner(new File(s"$resources/styles"))

  "A simple header" should "be formatted correctly" in {
    val name = "simple-header"
    val (input, spec) = testInput(name)
    val resultF = testRunner.run(input)

    assert(resultF.isRight, "Optimizing of the document failed")

    val result = differ.diff(spec, resultF.right.get)
    assert(result.isEmpty, result.getOrElse(""))
  }

  "A (new style) simple header" should "be formatted correctly" in {
    val name = "new-simple-header"
    val (input, spec) = testInput(name)
    val resultF = testRunner.run(input)

    assert(resultF.isRight, "Optimizing of the document failed")

    val result = differ.diff(spec, resultF.right.get)
    assert(result.isEmpty, result.getOrElse(""))
  }

  "A header with a footnote" should "produce a latex-compatible header" in {
    val name = "header-footnotes"
    val (input, spec) = testInput(name)
    val resultF = testRunner.run(input)

    assert(resultF.isRight, "Optimizing of the document failed")

    val result = differ.diff(spec, resultF.right.get)
    assert(result.isEmpty, result.getOrElse(""))
  }

}
