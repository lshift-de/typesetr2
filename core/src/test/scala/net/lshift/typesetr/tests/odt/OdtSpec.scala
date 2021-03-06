package net.lshift.typesetr.tests.odt

import java.io.File

import org.scalatest.{ Matchers, FlatSpec }

abstract class OdtSpec extends FlatSpec with Matchers {

}

object OdtSpec {

  def resources = "resources"

  def testInputOdt(testName: String): (File, File) =
    (new File(getClass.getResource(s"/tests/$testName.odt").getFile),
      new File(getClass.getResource(s"/specs/$testName-content.xml").getFile))

  def testInputTex(testName: String): (File, File) =
    (new File(getClass.getResource(s"/tests/$testName.odt").getFile),
      new File(getClass.getResource(s"/specs/$testName-content.tex").getFile))

}