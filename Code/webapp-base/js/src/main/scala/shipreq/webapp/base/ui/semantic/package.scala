package shipreq.webapp.base.ui

import japgolly.scalajs.react.vdom.html_<^._
import scala.collection.immutable.ListSet

package object semantic {

  /** Can be empty */
  type ClassName = String

  final val NoClass = ""

  @inline implicit class ClassNameExt(private val self: ClassName) extends AnyVal {

    def maybe(when: Boolean, add: ClassName): ClassName =
      if (when)
        self + " " + add
      else
        self

    def <+(h: HasClass): ClassName =
      // lol simple. extra spaces don't matter.
      self + " " + h.cls

    def <+(m: Multiple[_ <: HasClass]): ClassName =
      m.values.foldLeft(self)(_ <+ _)
  }

  def divCls(c: ClassName) =
    <.div(^.cls := c)

  abstract class HasClass(val cls: ClassName) extends Serializable with Product

//  def makeClassName(init: ClassName, hs: HasClass*): ClassName = {
//    var c = init
//    hs.foreach(c += " " + _.cls)
//    c
//  }

  final class Multiple[A](val values: ListSet[A]) extends AnyVal {
    @inline def +(a: A): Multiple[A] =
      new Multiple(values + a)

    @inline def ++(as: IterableOnce[A]): Multiple[A] =
      new Multiple(values ++ as)
  }

  object Multiple {
    implicit def empty[A: UnivEq]: Multiple[A] =
      new Multiple(ListSet.empty[A])

    //  implicit def single[A, B >: A: UnivEq](a: A): Multiple[B] =
    //    Multiple.empty[B] | a
    implicit def single[A: UnivEq](a: A): Multiple[A] =
      Multiple.empty[A] + a
  }

  @inline implicit class SemExtAny[A](private val self: A) extends AnyVal {
    def +[B >: A: UnivEq](b: B): Multiple[B] =
      Multiple.empty[B] + self + b
  }

  @inline implicit class SemExtReactTag(private val self: VdomTag) extends AnyVal {
    def addClass(c: ClassName) =
      if (c.isEmpty) self else self(^.cls := c)

    def <+(h: HasClass) =
      self addClass h.cls
  }

  final class UsesSemanticUiManually extends scala.annotation.StaticAnnotation
}
