package net.lshift.typesetr

import net.lshift.typesetr.parsers.{ NodeFactory, TextRepr, Repr }

import scala.xml._

package object xml {
  final def makeElem(label: Tag,
                     children: Seq[Node] = Seq(),
                     attr: Option[MetaData] = None): Elem = {
    Elem(null, label, attr.getOrElse(null), TopScope, false, children: _*)
  }

  final def optMakeElem(label: Tag, elems: Seq[Node]): Option[Node] =
    if (elems.isEmpty) None
    else Some(makeElem(label, elems))

  /*
    Is the element or body `thing` content-free?

    >>> blank(' ') and blank(['\t']) and blank(('a', {'name': 'some link'}, []))
    True
    >>> blank('x') or blank(('img', {'src': 'foo.png'}, []))
    False
    >>> blank(('a', {'href': 'foo.html'}, ['', ('span', {}, '')]))
    True
    >>> blank(('a', {'href': 'some link'}, ['', ('span', {}, 'link text')]))
    False
  */
  final def isBlank(nodes: Seq[Repr]): Boolean =
    nodes.forall(isBlank)

  final def isBlank(node: Repr): Boolean = node match {
    case TextRepr(text) =>
      text.trim == ""
    case elem =>
      elem.body.forall(isBlank)
  }

  // TODO: rename to sth more meaningful
  final def whack[T](node: Repr.Aux[T],
                     filterBy: Repr => Boolean,
                     removeBody: Boolean = false)(
                       implicit builder: NodeFactory.Aux[T]): Seq[Repr.Aux[T]] = {
    node.source match {
      case atom: Atom[_] =>
        Seq(node)
      case elem: Elem =>
        if (filterBy(node)) {
          if (removeBody) Seq()
          else node.body.toList.flatMap(n =>
            whack[T](n, filterBy, removeBody))
        } else {
          // recursively call children
          val children1 = node.body.toList.flatMap(
            n => whack(n, filterBy, removeBody))
          builder.createWithAttributes(
            tag = node.tag,
            elem = node.source,
            attrs = node.attr,
            children = children1) :: Nil
        }
      case _ =>
        Seq(node)
    }
  }

}
