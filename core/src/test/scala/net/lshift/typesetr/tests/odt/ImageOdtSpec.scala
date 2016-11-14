package net.lshift.typesetr.tests.odt

import net.lshift.typesetr.Differ
import net.lshift.typesetr.odt.OdtTestDiffer
import org.scalatest._
import java.io.File

import scala.language.postfixOps

class ImageOdtSpec extends OdtSpec {
  import OdtSpec._

  lazy val differ: Differ[File] = new OdtTestDiffer()
  lazy val testRunner = new OdtOptimizerRunner(new File(s"$resources/styles"))

  "A simple image" should "be wrapped in Typesetr's image blocks and modified width" in {
    val name = "simple-image"
    val (input, spec) = testInputOdt(name)
    val resultF = testRunner.run(input)

    assert(resultF.isRight, "Optimizing of the document failed")

    val result = differ.diff(spec, resultF.right.get)
    assert(result.isEmpty, result.getOrElse(""))
  }

  "A small inlined image" should "be wrapped in Typesetr's inlined image blocks" in {
    val name = "simple-image-inline"
    val (input, spec) = testInputOdt(name)
    val resultF = testRunner.run(input)

    assert(resultF.isRight, "Optimizing of the document failed")

    val result = differ.diff(spec, resultF.right.get)
    assert(result.isEmpty, result.getOrElse(""))
  }

  "A big inlined image" should "be wrapped in Typesetr's block image block" in {
    val name = "simple-image-inline"
    val (input, spec) = testInputOdt(name)
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
    val name = "simple-image-caption-basic"
    val (input, spec) = testInputOdt(name)
    val resultF = testRunner.run(input)

    assert(resultF.isRight, "Optimizing of the document failed")

    val result = differ.diff(spec, resultF.right.get)
    assert(result.isEmpty, result.getOrElse(""))

  }

  "An image" should "detect a caption command in the same paragraph" in {
    val name = "simple-image-gdoc-caption01"
    val (input, spec) = testInputOdt(name)
    val resultF = testRunner.run(input)

    assert(resultF.isRight, "Optimizing of the document failed")

    val result = differ.diff(spec, resultF.right.get)
    assert(result.isEmpty, result.getOrElse(""))
  }

  "An image" should "detect a caption command in the next paragraph" in {
    val name = "simple-image-gdoc-caption02"
    val (input, spec) = testInputOdt(name)
    val resultF = testRunner.run(input)

    assert(resultF.isRight, "Optimizing of the document failed")

    val result = differ.diff(spec, resultF.right.get)
    assert(result.isEmpty, result.getOrElse(""))
  }

  "And image" should "parse an image and an empty caption command" in {
    val name = "simple-image-gdoc-caption03"
    val (input, spec) = testInputOdt(name)
    val resultF = testRunner.run(input)

    assert(resultF.isRight, "Optimizing of the document failed")

    val result = differ.diff(spec, resultF.right.get)
    assert(result.isEmpty, result.getOrElse(""))
  }

}