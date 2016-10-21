package net.lshift.typesetr.tests.odt

import net.lshift.typesetr.Differ
import net.lshift.typesetr.odt.OdtTestDiffer
import org.scalatest._

import java.io.File

class ImageOdtSpec extends FlatSpec with Matchers {
  import ImageOdtSpec._

  lazy val differ: Differ = new OdtTestDiffer()
  lazy val testRunner = new OdtOptimizerRunner(new File(s"$resources/styles"))

  "A simple image" should "be wrapped in Typesetr's image blocks and modified width" in {
    val name = "simple-image"
    val (input, spec) = testInput(name)
    val resultF = testRunner.run(input)

    assert(resultF.isRight, "Optimizing of the document failed")

    val result = differ.diff(spec, resultF.right.get)
    assert(result.isEmpty, result.getOrElse(""))
  }

  "A small inlined image" should "be wrapped in Typesetr's inlined image blocks" in {
    val name = "simple-image-inline"
    val (input, spec) = testInput(name)
    val resultF = testRunner.run(input)

    assert(resultF.isRight, "Optimizing of the document failed")

    val result = differ.diff(spec, resultF.right.get)
    assert(result.isEmpty, result.getOrElse(""))
  }

  "A big inlined image" should "be wrapped in Typesetr's block image block" in {
    val name = "simple-image-inline"
    val (input, spec) = testInput(name)
    val resultF = testRunner.run(input)

    assert(resultF.isRight, "Optimizing of the document failed")

    val result = differ.diff(spec, resultF.right.get)
    assert(result.isEmpty, result.getOrElse(""))
  }

  // -- captions

  "An image" should "parse a text-box caption" in {
    // No block wrapping in this case.
    // Except that such nicely formatted images are never
    // created by google docs. GDoc does not provide any
    // mechanism to allow for captions that are not placed
    // directly in the image.
    /*val name = "simple-image-caption-basic"
    val (input, spec) = testInput(name)
    val resultF = testRunner.run(input)

    assert(resultF.isRight, "Optimizing of the document failed")

    val result = differ.diff(spec, resultF.right.get)
    assert(result.isEmpty, result.getOrElse(""))*/

  }

  "An image" should "detect a caption command in the same paragraph" in {

  }

  "An image" should "detect a caption command in the next paragraph" in {

  }

  "And image" should "parse an image and an empty caption command" in {

  }

}

object ImageOdtSpec {
  def resources = "resources"
  def testInput(testName: String): (File, File) =
    (new File(getClass.getResource(s"/tests/$testName.odt").getFile), //(s"$resources/tests/$testName.odt"),
      new File(getClass.getResource(s"/specs/$testName-content.xml").getFile))
}