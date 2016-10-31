package net.lshift.typesetr
package tests
package odt

import net.lshift.typesetr.Differ
import net.lshift.typesetr.odt.OdtTestDiffer
import org.scalatest._
import java.io.File

import scala.language.postfixOps

class ListsOdtSpec extends OdtSpec {

  import OdtSpec._

  lazy val differ: Differ = new OdtTestDiffer()
  lazy val testRunner = new OdtOptimizerRunner(new File(s"$resources/styles"))

  "A list" should "correctly start, restart sub-start enumerations" in {
    val name = "enumeration"
    val (input, spec) = testInput(name)
    val resultF = testRunner.run(input)

    assert(resultF.isRight, "Optimizing of the document failed")

    val result = differ.diff(spec, resultF.right.get)
    assert(result.isEmpty, result.getOrElse(""))
  }

  "A very deep list" should "produce a regularly formatted list" in {
    val name = "lists-deep"
    val (input, spec) = testInput(name)
    val resultF = testRunner.run(input)

    assert(resultF.isRight, "Optimizing of the document failed")

    val result = differ.diff(spec, resultF.right.get)
    assert(result.isEmpty, result.getOrElse(""))
  }

}
