package net.lshift

import net.lshift.typesetr.util.NodeOps

import scala.language.implicitConversions

package object typesetr {

  implicit def toNodeOps(x: scala.xml.Node): NodeOps =
    new NodeOps(x)
}
