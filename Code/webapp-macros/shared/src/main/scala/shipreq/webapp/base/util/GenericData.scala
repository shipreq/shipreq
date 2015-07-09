package shipreq.webapp.base.util

import shipreq.base.util.{NonEmpty, IMap, NonEmptySet, UnivEq}

abstract class GenericData {

  /**
   * A data attribute.
   */
  trait AttrBase extends Product with Serializable {
    this: Attr =>
    type Data
    def apply(d: Data): ValueFor[this.type]

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

  // This really just so that Intellij doesn't highlight EVERYTHING red.
  protected def defAttr[D]: Attr {type Data = D; def apply(d: D): ValueFor[this.type]} = ???
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
  import scala.reflect.macros.{blackbox, whitebox}

  def objectImpl(c: whitebox.Context)(annottees: c.Expr[Any]*) = {
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
                override def apply(data: Data) = $valueNameT(data)
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

  import upickle._

  def upickler (d: GenericData)(keys: d.Attr => String): ReadWriter[d.NonEmptyValues] = macro quietMPickler
  def _upickler(d: GenericData)(keys: d.Attr => String): ReadWriter[d.NonEmptyValues] = macro debugMPickler

  def debugMPickler(c: blackbox.Context)(d: c.Expr[GenericData])(keys: c.Expr[d.value.Attr => String]) = implMPickler(c, true )(d)(keys)
  def quietMPickler(c: blackbox.Context)(d: c.Expr[GenericData])(keys: c.Expr[d.value.Attr => String]) = implMPickler(c, false)(d)(keys)

  private def sep = ("_" * 120) + "\n"

  def implMPickler(c: blackbox.Context, debug: Boolean)(d: c.Expr[GenericData])(keys: c.Expr[d.value.Attr => String]): c.Expr[ReadWriter[d.value.NonEmptyValues]] = {
    import c.universe._

    if (debug) println(sep)

    val D = d.actualType.asInstanceOf[SingleType]

    val keyLookup: List[(String, Literal)] =
      keys match {
        case Expr(Function(_, Match(_, caseDefs))) =>
          caseDefs map {
            case CaseDef(Select(_, name), _, key@ Literal(Constant(_: String))) => (name.toString, key)
            case x => fail(c, s"Expecting a case like: {case Attr => Literal}\nGot: ${showRaw(x)}")
          }
        case _ => fail(c, s"Expecting a function like: {case Attr => Literal}\nGot: ${showRaw(keys)}")
      }

    if (debug) println(s"Keys: $keyLookup")

    val attrsAndValues: Vector[(ModuleSymbol, ClassSymbol)] = {
      val attrTrait  = D.member(TypeName("Attr")).asType.toType
      val valueTrait = D.member(TypeName("Value")).asType.toType

      var attrs  = Vector.empty[ModuleSymbol]
      var values = Vector.empty[ClassSymbol]
      for (m <- D.members) {
        if (m.isModule) {
          val mm = m.asModule
          if (mm.moduleClass.asType.toType <:< attrTrait)
            attrs :+= mm
        } else if (m.isClass && !m.isAbstract) {
          val mc = m.asClass
          if (mc.asType.toType <:< valueTrait)
            values :+= mc
        }
      }
      attrs = attrs.sortBy(_.name.toString)
      values = values.sortBy(_.name.toString)

      if (debug) {
        println(s"Attrs: $attrs")
        println(s"Values: $values")
      }
      if (attrs.length != values.length)
        fail(c, s"attrs.length != values.length\n$attrs\n$values")

      attrs zip values
    }

    var init     = List.empty[ValDef]
    var wCases   = List.empty[CaseDef]
    var rCases   = List.empty[CaseDef]
    var keysUsed = Set.empty[String]

    for ((attr, value) <- attrsAndValues) {
      val name = attr.name.toString
      val key = keyLookup.find(_._1 == name).map(_._2) getOrElse fail(c, s"Key not found for $name.\nKeys = $keyLookup")
      val rw = TermName(c.freshName("rw"))
      val rwDef = q"val $rw = implicitly[ReadWriter[$attr.Data]]": ValDef
      wCases  ::= cq"v: $value => kvs :+= (($key, $rw write v.value))"
      rCases  ::= cq"$key => $attr apply $rw.read(kv._2)"
      init    ::= rwDef
      keysUsed += key.toString()
    }

    if (keysUsed.size != attrsAndValues.size)
      fail(c, s"Keys must be unique: $keyLookup")

    val DFQN = toSelectFQN(c)(D.typeSymbol.asType)

    val impl = q""" {
      import _root_.upickle.{ReadWriter, Js}
      ..${flattenBlocks(c)(init)}
      val empty = $DFQN.emptyValues
      ReadWriter[$DFQN.NonEmptyValues](
        vs => {
          var kvs = Vector.empty[(String, Js.Value)]
          vs.value.values foreach { case ..$wCases }
          Js.Obj(kvs: _*)
        },
        { case o: Js.Obj =>
          var kvs = empty
          o.value.foreach { kv =>
            val v = kv._1 match {
              case ..$rCases
              case what => sys.error("Unknown key '"+what+"' in "+o)
            }
            kvs += v
          }
          if (kvs.isEmpty)
            sys.error("At least one value required.")
          else
            NonEmpty.force(kvs)
        }
      )
    } """

    if (debug) println("\n" + impl + "\n" + sep)

    c.Expr[ReadWriter[d.value.NonEmptyValues]](impl)
  }
}
