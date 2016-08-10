package net.lshift.typesetr
package parsers
package odt
package styles

import java.util.regex.Pattern

import scala.util.matching.Regex

sealed abstract class StyleValidator {

  type T

  def validate(v: String): Boolean

  def transform: String => Option[T]

}

object StyleValidator {

  type Aux[V] = StyleValidator { type T = V }

  def list[T](seq: List[T])(implicit conv: String => T): StyleValidator.Aux[T] =
    new TypesafeListValidator(seq, (x => Some(x)))

  def list[T](seq: List[T], fun: String => Option[T])(implicit conv: String => T): StyleValidator.Aux[T] =
    new TypesafeListValidator(seq, fun)

  def r(reg: Regex): StyleValidator.Aux[String] =
    new RegexValidator(reg.pattern, (x => Some(x)))

  def r[T](reg: Regex, fun: String => Option[T]): StyleValidator.Aux[T] =
    new RegexValidator(reg.pattern, fun)

  def all[T](fun: String => Option[T]): StyleValidator.Aux[T] =
    new AcceptAllValidator[T](fun)

}

class ListValidator[V](vs: List[String], val transform: String => Option[V])
  extends StyleValidator {

  type T = V

  def validate(v: String): Boolean =
    vs contains v

}

class TypesafeListValidator[V](vs: List[V], val transform: String => Option[V])(
                                 implicit conv: String => V) extends StyleValidator {

  type T = V

  def validate(v: String): Boolean =
    vs contains conv(v)

}

class RegexValidator[V](reg: Pattern, val transform: String => Option[V])
  extends StyleValidator {

  type T = V

  def validate(v: String): Boolean =
    reg.matcher(v).matches()

}

class AcceptAllValidator[V](val transform: String => Option[V])
  extends StyleValidator {

  type T = V

  def validate(v: String): Boolean = true

}