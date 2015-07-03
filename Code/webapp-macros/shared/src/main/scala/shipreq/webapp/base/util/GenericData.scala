package shipreq.webapp.base.util

import shipreq.base.util.{NonEmpty, IMap, NonEmptySet, UnivEq}

abstract class GenericData {

  /**
   * A data attribute.
   */
  trait AttrBase {
    this: Attr =>
    type A
    def value(a: A): ValueFor[this.type]
  }

  /**
   * A value and the attribute to which it applies.
   */
  trait ValueBase {
    this: Value =>
    val attr: Attr
    val value: attr.A
  }

  type Attr  <: AttrBase
  type Value <: ValueBase

  type ValueFor[A <: Attr] = Value {val attr: A}
  type Attrs               = NonEmptySet[Attr]
  type Values              = IMap[Attr, Value]
  type NonEmptyValues      = NonEmpty[Values]

  val attrs: Attrs

  implicit def equality: UnivEq[Attr]

  def emptyValues: Values =
    IMap.empty(_.attr)

  protected implicit class FieldDeclarationSyntax(val name: Symbol) {
    def apply[T] = ???
  }
}

// =====================================================================================================================
import scala.annotation.StaticAnnotation
import scala.annotation.compileTimeOnly

@compileTimeOnly("Enable macro paradise to expand macro annotations")
class GenericDataAttrs extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro GenericDataMacros.objectImpl
}

object GenericDataMacros {
  import shipreq.webapp.macros.MacroUtils._
  import shipreq.webapp.macros.WhiteboxMacroUtils._
  import scala.reflect.macros.whitebox.Context

  def objectImpl(c: Context)(annottees: c.Expr[Any]*) = {
    import c.universe._

    var attrNames = Vector.empty[TermName]
    var attrDefns = List.empty[Tree]
    var unused    = Vector.empty[Tree]

    def processBody(body: List[Tree]): Unit = {
      body.foreach {
        case q"scala.Symbol(${n: Literal})[$attrType]" =>

          val prefix     = n.value.value.toString
          val attrName   = prefix
          val attrNameT  = TermName(attrName)
          val valueName  = prefix + "Value"
          val valueNameT = TermName(valueName)
          val valueNameY = TypeName(valueName)

          val defn =
            q"""
              case object $attrNameT extends Attr {
                override type A = $attrType
                override def value(a: A) = $valueNameT(a)
              }
              final case class $valueNameY(value: $attrNameT.A) extends Value {
                override val attr: $attrNameT.type = $attrNameT
              }
            """

          attrNames :+= attrNameT
          attrDefns ::= defn

        case x if x.isEmpty => ()
        case x => unused :+= x
      }
    }

    val impl = annottees.map(_.tree) match {
      case List(q"object $objName extends $parent { ..$body }") =>

        processBody(body)

        if (unused.nonEmpty) {
          unused foreach println
          fail(c, "Unrecognised field declarations found.")
        }

        q"""
          object $objName extends $parent {
            import shipreq.base.util.{NonEmptySet, UnivEq}

            sealed trait Attr extends AttrBase
            sealed trait Value extends ValueBase

            ..${flattenBlocks(c)(attrDefns)}

            override implicit def equality = UnivEq.force[Attr]

            override val attrs = NonEmptySet[Attr](..$attrNames)
          }
         """

      case _ => fail(c, "You must annotate an object definition.")
    }

//    val sep = ("="*120)+"\n"
//    println(sep + impl + "\n" + sep)

    c.Expr[Any](impl)
  }
}