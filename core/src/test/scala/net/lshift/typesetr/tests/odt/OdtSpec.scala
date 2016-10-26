package net.lshift.typesetr.tests.odt

import java.io.File

import org.scalatest.{ Matchers, FlatSpec }

abstract class OdtSpec extends FlatSpec with Matchers {

}

object OdtSpec {
  def resources = "resources"
  def testInput(testName: String): (File, File) =
    (new File(getClass.getResource(s"/tests/$testName.odt").getFile), //(s"$resources/tests/$testName.odt"),
      new File(getClass.getResource(s"/specs/$testName-content.xml").getFile))
}