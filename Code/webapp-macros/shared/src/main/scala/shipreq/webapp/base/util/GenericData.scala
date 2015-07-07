package shipreq.webapp.base.util

import shipreq.base.util.{NonEmpty, IMap, NonEmptySet, UnivEq}

abstract class GenericData {

  /**
   * A data attribute.
   */
  trait AttrBase extends Product with Serializable {
    this: Attr =>
    type Data
    def :=(d: Data): ValueFor[this.type]

    final def apply(vs: Values): Option[ValueFor[this.type]] =
      vs.get(this).asInstanceOf[Option[ValueFor[this.type]]]
  }

  /**
   * A value and the attribute to which it applies.
   */
  trait ValueBase extends Product with Serializable {
    this: Value =>
    val attr: Attr
    val value: attr.Data
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

  def values(vs: Value*): Values =
    emptyValues ++ vs

  def nev(v1: Value, vn: Value*): NonEmptyValues =
    NonEmpty.force(emptyValues + v1 ++ vn)

  protected def defAttr[D]: Attr {type Data = D} = ???
}

// =====================================================================================================================
import scala.annotation.StaticAnnotation
import scala.annotation.compileTimeOnly

@compileTimeOnly("Enable macro paradise to expand macro annotations")
class CreateGenericData extends StaticAnnotation {
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
        case q"val ${n: TermName} = defAttr[$attrType]" =>

          val prefix     = n.toString
          val attrName   = prefix
          val attrNameT  = TermName(attrName)
          val valueName  = "ValueFor" + prefix
          val valueNameT = TermName(valueName)
          val valueNameY = TypeName(valueName)

          val defn =
            q"""
              case object $attrNameT extends Attr {
                override type Data = $attrType
                override def :=(data: Data) = $valueNameT(data)
              }
              final case class $valueNameY(value: $attrNameT.Data) extends Value {
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
          for (u <- unused)
            println(showRaw(u))
          val expl = unused.map(" - " + _.toString) mkString "\n"
          fail(c, s"Unrecognised field declarations found.\n$expl")
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