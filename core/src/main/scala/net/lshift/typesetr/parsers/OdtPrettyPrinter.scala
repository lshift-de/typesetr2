package net.lshift.typesetr.parsers

class OdtPrettyPrinter extends PrettyPrinter[scala.xml.Node] {

  def print(node: Repr.Aux[scala.xml.Node]): Unit =
    printIndent(node, 1)

  private def printIndent(node: Repr.Aux[scala.xml.Node], indent: Int): Unit = {
    println((">" * indent) + node.source.label)
    node.body.foreach(elem => printIndent(elem, indent + 1))
  }

}
