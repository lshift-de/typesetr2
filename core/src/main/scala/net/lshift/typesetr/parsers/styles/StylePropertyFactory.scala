package net.lshift.typesetr.parsers
package styles

abstract class StylePropertyFactory[T] {

  def create(styleId: StyleId)(implicit factory: NodeFactory.Aux[T]): Repr.Aux[T]

  def modifyBody(styleId: StyleId, children: Seq[Repr.Aux[T]])(implicit factory: NodeFactory.Aux[T]): Seq[Repr.Aux[T]]

}
