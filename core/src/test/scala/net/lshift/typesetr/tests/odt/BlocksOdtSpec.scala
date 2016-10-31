package net.lshift.typesetr
package tests
package odt

import net.lshift.typesetr.Differ
import net.lshift.typesetr.odt.OdtTestDiffer
import net.lshift.typesetr.pandoc.Markers
import org.scalatest._
import java.io.File

import scala.language.postfixOps

class BlocksOdtSpec extends OdtSpec {

  import OdtSpec._

  lazy val differ: Differ = new OdtTestDiffer()
  lazy val testRunner = new OdtOptimizerRunner(new File(s"$resources/styles"))

  "A simple indented block" should "be formatted into a blockquote" in {
    val name = "simple-block"
    val (input, spec) = testInput(name)
    val resultF = testRunner.run(input)

    assert(resultF.isRight, "Optimizing of the document failed")

    val result = differ.diff(spec, resultF.right.get)
    assert(result.isEmpty, result.getOrElse(""))
  }

  it should "take into account right-aligned text" in {
    val name = "simple-block-with-right-align"
    val (input, spec) = testInput(name)
    val resultF = testRunner.run(input)

    assert(resultF.isRight, "Optimizing of the document failed")

    val result = differ.diff(spec, resultF.right.get)
    assert(result.isEmpty, result.getOrElse(""))
  }

  "A slightly indented block" should "not be formatted into a blockquote" in {
    val name = "simple-block-not"
    val (input, spec) = testInput(name)
    val resultF = testRunner.run(input)

    assert(resultF.isRight, "Optimizing of the document failed")

    val result = differ.diff(spec, resultF.right.get)
    assert(result.isEmpty, result.getOrElse(""))
  }

  "A code block" should "be detected by the font-family and indentation" in {
    val name = "simple-code-block"
    val (input, spec) = testInput(name)
    val resultF = testRunner.run(input)

    assert(resultF.isRight, "Optimizing of the document failed")

    val result = differ.diff(spec, resultF.right.get)
    assert(result.isEmpty, result.getOrElse(""))
  }

  it should "not be detected if there is no indentation" in {
    val name = "simple-code-block-no-indent"
    val (input, spec) = testInput(name)
    val resultF = testRunner.run(input)

    assert(resultF.isRight, "Optimizing of the document failed")

    val result = differ.diff(spec, resultF.right.get)
    assert(result.isEmpty, result.getOrElse(""))
  }

  // For example. Consolas is not considered to be a code font
  // by Pandoc so we have to make it as if it was.
  "An inlined code" should "be detected by the font-family" in {
    val name = "code-block-inlined"
    val (input, spec) = testInput(name)
    val resultF = testRunner.run(input)

    assert(resultF.isRight, "Optimizing of the document failed")

    val result = differ.diff(spec, resultF.right.get)
    assert(result.isEmpty, result.getOrElse(""))
  }

}
