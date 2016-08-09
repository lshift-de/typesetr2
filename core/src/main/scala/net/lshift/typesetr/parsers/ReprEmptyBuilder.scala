package net.lshift.typesetr.parsers

import net.lshift.typesetr.xml.{ Attribute, Tag }

import scala.xml.Node

abstract class ReprEmptyBuilder[T] {
  def empty(): Repr.Aux[T]
}

object ReprEmptyBuilder {

  implicit lazy val nodeEmptyBuilder: ReprEmptyBuilder[scala.xml.Node] =
    new ReprEmptyBuilder[Node] {

      def empty(): Repr.Aux[Node] = new Repr { self =>

        type R = scala.xml.Node

        type BodyTpe = Repr.Aux[scala.xml.Node]

        def source: R = ???

        def body: Seq[self.BodyTpe] = Nil

        def tag: Tag = Tag.nodeTag

        def contents: Option[V] = None

        def attr: List[Attribute] = Nil
      }

    }
}
