package net.lshift.typesetr

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
  final def isBlank(nodes: Seq[Node]): Boolean =
    nodes.forall(isBlank)

  final def isBlank(node: Node): Boolean = {
    node match {
      case atom: Text =>
        atom.data.trim == ""
      // TODO: other cases
      case elem: Elem =>
        elem.child.forall(isBlank)
    }
  }

  // TODO: rename to sth more meaningful
  final def whack(node: Node,
                  filterBy: Node => Boolean,
                  removeBody: Boolean = false): Seq[Node] = {
    node match {
      case atom: Atom[_] =>
        Seq(atom)
      case elem: Elem =>
        if (filterBy(elem)) {
          if (removeBody) Seq()
          else elem.child.toList.flatMap(n =>
            whack(n, filterBy, removeBody))
        } else {
          // recursively call children
          val children1 = elem.child.toList.flatMap(n => whack(n, filterBy, removeBody))
          Seq(elem.copy(child = children1))
        }
      case node =>
        Seq(node)
    }
  }

}
