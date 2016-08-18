package net.lshift.typesetr
package parsers
package odt
package styles

import java.util.regex.Pattern

import scala.util.matching.Regex

sealed abstract class StyleValidator {

  type T

  type From

  def validate(v: Option[From]): Boolean =
    (!isRequired || v.nonEmpty) && v.map(validateValue).getOrElse(true)

  protected def validateValue(v: From): Boolean

  protected def isRequired: Boolean

  def transform: From => Option[T]

}

abstract class StringStyleValidator extends StyleValidator {
  type From = String
}

object StyleValidator {

  type Aux[V] = StyleValidator { type From = String; type T = V }

  def propsList[V](seq: List[V]): StyleValidator { type T = V } =
    new ListPropsValidator[V](seq)

  def list[T](seq: List[T], required: Boolean)(implicit conv: String => Option[T]): StyleValidator.Aux[T] =
    new TypesafeListValidator(seq, x => conv(x), required)

  def list[T](seq: List[T], fun: String => Option[T], required: Boolean)(implicit conv: String => Option[T]): StyleValidator.Aux[T] = //(implicit conv: String => T): StyleValidator.Aux[T] =
    new TypesafeListValidator(seq, fun, required)

  def r(reg: Regex, required: Boolean): StyleValidator.Aux[String] =
    new RegexValidator(reg.pattern, (x => Some(x)), required)

  def r[T](reg: Regex, fun: String => Option[T], required: Boolean): StyleValidator.Aux[T] =
    new RegexValidator(reg.pattern, fun, required)

  def all[T](fun: String => Option[T]): StyleValidator.Aux[T] =
    new AcceptAllValidator[T](fun)

}

class ListPropsValidator[V](vs: List[V]) extends StyleValidator {

  type From = V

  type T = V

  protected def isRequired: Boolean = true

  protected def validateValue(v: From): Boolean = vs contains v

  def transform: From => Option[T] = (x: From) => Some(x)

}

class ListValidator[V](vs: List[String], val transform: String => Option[V],
                       protected val isRequired: Boolean)
  extends StringStyleValidator {

  type T = V

  def validateValue(v: String): Boolean = vs contains v

}

class TypesafeListValidator[V](vs: List[V], val transform: String => Option[V],
                               protected val isRequired: Boolean)(
                                 implicit conv: String => Option[V]) extends StringStyleValidator {

  type T = V

  def validateValue(v: String): Boolean = {
    val v1 = conv(v)
    v1.map(vs.contains).getOrElse(false)
  }

}

class RegexValidator[V](reg: Pattern, val transform: String => Option[V],
                        protected val isRequired: Boolean)
  extends StringStyleValidator {

  type T = V

  def validateValue(v: String): Boolean = reg.matcher(v).matches()

  override def toString: String = s"Regex validator: ${reg.pattern()}"

}

class AcceptAllValidator[V](val transform: String => Option[V])
  extends StringStyleValidator {

  protected def isRequired: Boolean = false

  type T = V

  def validateValue(v: String): Boolean = true

}