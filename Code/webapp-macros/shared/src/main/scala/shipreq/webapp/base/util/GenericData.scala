package shipreq.webapp.base.util

import monocle.Lens
import shipreq.base.util.{IMap => IM, NonEmptySet, UnivEq}

abstract class GenericData[Data] {

  /**
   * An attribute of [[Data]].
   */
  type Attr

  /**
   * A value and the attribute to which it applies.
   */
  type Value

  type ValueFor[A <: Attr]

  implicit def equality: UnivEq[Attr]

  val attrs: NonEmptySet[Attr]

  type IMap = IM[Attr, Value]

  def emptyIMap: IMap
}

// =====================================================================================================================
import scala.annotation.StaticAnnotation
import scala.annotation.compileTimeOnly

@compileTimeOnly("Enable macro paradise to expand macro annotations")
class GenericDataAttrs(lenses: Lens[_, _]*) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro GenericDataMacros.objectImpl
}

object GenericDataMacros {
  import shipreq.webapp.macros.MacroUtils._
  import shipreq.webapp.macros.WhiteboxMacroUtils._
  import scala.reflect.macros.whitebox.Context

  def objectImpl(c: Context)(annottees: c.Expr[Any]*) = {
    import c.universe._

    val args = extractStaticAnnotationArgs(c)
    if (args.isEmpty)
      fail(c, "No attributes specified.")

    val (attrNames, defns) = args.map { arg =>
      val name = arg match {
        case Select(_, n) => n
        case x => fail(c, s"Unable to extract attribute name from: $x")
      }

      val prefix: String = {
        val n = name.toTermName.decodedName.toString
        n.head.toString.toUpperCase + n.tail
      }

      val attrName   = prefix
      val attrNameT  = TermName(attrName)
      val valueName  = prefix + "Value"
      val valueNameT = TermName(valueName)
      val valueNameY = TypeName(valueName)

      val defn =
        q"""
          case object $attrNameT extends AttrA($arg) {
            override def value(a: A) = $valueNameT(a)
          }
          final case class $valueNameY(value: $attrNameT.A) extends Value {
            val attr: $attrNameT.type = $attrNameT
          }
        """

      (attrNameT, defn)
    }.unzip

    val impl =
      annottees.map(_.tree) match {
        case List(q"object $objName extends $parent[$t] { ..$body }") if body.isEmpty =>
          val T = t

          q"""
            object $objName extends $parent[$T] {
              import shipreq.base.util.{IMap => IM, NonEmptySet, UnivEq}

              sealed trait Attr {
                type A
                val lens: monocle.Lens[$T, A]
                def value(a: A): ValueFor[this.type]
              }

              sealed abstract class AttrA[_A](override val lens: monocle.Lens[$T, _A]) extends Attr {
                final type A = _A
              }

              sealed trait Value {
                val attr: Attr
                val value: attr.A
              }

              override type ValueFor[A <: Attr] = Value {val attr: A}

              ..${flattenBlocks(c)(defns)}

              override implicit def equality: UnivEq[Attr] = UnivEq.force

              override val attrs: NonEmptySet[Attr] = NonEmptySet(..$attrNames)

              override type IMap = IM[Attr, Value]

              override def emptyIMap: IMap = IM.empty(_.attr)
            }
           """

        case _ => fail(c, "You must annotate an object definition with an empty body.")
      }

//    println()
//    println(impl)
//    println()

    c.Expr[Any](impl)
  }
}