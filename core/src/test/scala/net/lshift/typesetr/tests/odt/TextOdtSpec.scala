package net.lshift.typesetr
package tests
package odt

import net.lshift.typesetr.Differ
import net.lshift.typesetr.odt.OdtTestDiffer
import org.scalatest._
import java.io.File

import scala.language.postfixOps

class TextOdtSpec extends OdtSpec {

  import OdtSpec._

  lazy val differ: Differ[File] = new OdtTestDiffer()
  lazy val testRunner = new OdtOptimizerRunner(new File(s"$resources/styles"))

  "A footnote in a normal paragraph" should "be left without a change" in {
    val name = "basic-footnotes"
    val (input, spec) = testInputOdt(name)
    val resultF = testRunner.run(input)

    assert(resultF.isRight, "Optimizing of the document failed")

    val result = differ.diff(spec, resultF.right.get)
    assert(result.isEmpty, result.getOrElse(""))
  }

}
