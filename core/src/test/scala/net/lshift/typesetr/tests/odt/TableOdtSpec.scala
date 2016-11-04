package net.lshift.typesetr
package tests
package odt

import net.lshift.typesetr.Differ
import net.lshift.typesetr.odt.OdtTestDiffer
import org.scalatest._
import java.io.File

import scala.language.postfixOps

class TableOdtSpec extends OdtSpec {

  import OdtSpec._

  lazy val differ: Differ = new OdtTestDiffer()
  lazy val testRunner = new OdtOptimizerRunner(new File(s"$resources/styles"))

  "A table" should "display a correct number of rows and columns without headers" in {
    val name = "basic-table"
    val (input, spec) = testInput(name)
    val resultF = testRunner.run(input)

    assert(resultF.isRight, "Optimizing of the document failed")

    val result = differ.diff(spec, resultF.right.get)
    assert(result.isEmpty, result.getOrElse(""))
  }

  "A table" should "have an associated caption, if present" in {
    val name = "tableWithCaption"
    val (input, spec) = testInput(name)
    val resultF = testRunner.run(input)

    assert(resultF.isRight, "Optimizing of the document failed")

    val result = differ.diff(spec, resultF.right.get)
    assert(result.isEmpty, result.getOrElse(""))
  }

}
