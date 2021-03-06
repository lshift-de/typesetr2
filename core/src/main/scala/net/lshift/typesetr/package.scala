package net.lshift

import net.lshift.typesetr.parsers.odt.OdtFile
import OdtFile.OdtFileOps
import net.lshift.typesetr.util.IO.FileOps
import net.lshift.typesetr.util.{ UnitsOps, MetaDataOps, NodeOps }

import scala.language.implicitConversions

package object typesetr {

  implicit def toNodeOps(x: scala.xml.Node): NodeOps =
    new NodeOps(x)

  implicit def toAttrOps(x: scala.xml.MetaData): MetaDataOps =
    new MetaDataOps(x)

  implicit def toFileOps(x: java.io.File): OdtFileOps =
    new OdtFileOps(x)

  implicit def toJavaFileOps(x: java.io.File): util.IO.FileOps =
    new FileOps(x)

  implicit def toUnits(x: Double): UnitsOps =
    new UnitsOps(x)

  implicit def toUnits(x: Int): UnitsOps =
    new UnitsOps(x)

}
