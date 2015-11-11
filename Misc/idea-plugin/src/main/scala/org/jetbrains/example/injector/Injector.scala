package shipreq.idea

import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScValue
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScClass, ScObject, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.{TypeDefinitionMembers, SyntheticMembersInjector}
import org.jetbrains.plugins.scala.lang.psi.types.result.TypingContext

/*
  def objectImpl(annottees: c.Expr[Any]*) = {
    val equal     = c.typeOf[Equal[_]]
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
                val dataEquality: Equal[Data] = implicitly[Equal[$attrType]]
              }
              final case class $valueNameY(value: $attrNameT.Data) extends Value {
                override val attr: $attrNameT.type = $attrNameT
                override def equals(o: Any): Boolean = o match {
                  case $valueNameT(v2) => $attrNameT.dataEquality.equal(value, v2)
                  case _ => false
                }
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
          fail(s"Unrecognised field declarations found.\n$expl")
        }

        q"""
          object $objName extends $parent {
            import scalaz.{Equal, Order}
            import shipreq.base.util.{NonEmptySet, UnivEq}

            sealed abstract class Attr extends AttrBase
            sealed abstract class Value extends ValueBase

            ..${flattenBlocks(attrDefns)}

            override implicit val equalityAttr: Order[Attr] with UnivEq[Attr] =
              UnivEq.withArbitraryOrder(Vector(..$attrNames))

            @inline override implicit def equalityValue: UnivEq[Value] = UnivEq.force

            override val attrs = NonEmptySet[Attr](..$attrNames)
          }
         """

      case _ => fail("You must annotate an object definition.")
    }
 */
class ShipReqMacroExpander extends SyntheticMembersInjector {

  override def injectInners(source: ScTypeDefinition): Seq[String] =
    source match {
      case o: ScObject if o.annotations.exists(_.getQualifiedName == CreateGenericData) => go1(o)
      case _ => Nil
    }

  override def injectFunctions(source: ScTypeDefinition): Seq[String] =
    source match {
      case o: ScObject if o.annotations.exists(_.getQualifiedName == CreateGenericData) => go(o)
      case _ => Nil
    }

  /*
    source.members.flatMap {
      case v: ScValue if v.hasAnnotation(CreateGenericData).isDefined =>
        v.declaredElements.map { td =>
          s"def get${td.name.capitalize} : ${td.getType(TypingContext.empty).getOrAny.canonicalText} = ???"
        }
      case _ => Seq.empty
    }
  */

  /* // https://github.com/JetBrains/intellij-scala/blob/idea15.x/src/org/jetbrains/plugins/scala/lang/psi/impl/toplevel/typedef/MonocleInjector.scala

      source match {
      // Monocle lenses generation
      case obj: ScObject =>
        obj.fakeCompanionClassOrCompanionClass match {
          case clazz: ScClass if clazz.findAnnotationNoAliases("monocle.macros.Lenses") != null => mkLens(obj)
          case _ => Seq.empty
        }
      case _ => Seq.empty
    }
  */

  private[this] val CreateGenericData = "shipreq.webapp.base.util.CreateGenericData"

  private def go1(o: ScObject): Seq[String] = {
    val r = List.newBuilder[String]
    r += "sealed abstract class Attr extends AttrBase"
    r += "sealed abstract class Value extends ValueBase"
    r += """
           |final case class ValueForName(value: Name.Data) extends Value {
           |  override val attr: Name.type = ???
           |}
          """.stripMargin

    r.result()
  }

  private def go(o: ScObject): Seq[String] = {
    val r = List.newBuilder[String]

//    r += "import scalaz.{Equal, Order}"
//    r += "import shipreq.base.util.{NonEmptySet, UnivEq}"
//    r += "override implicit def equalityAttr: scalaz.Order[Attr] with shipreq.base.util.UnivEq[Attr] = ???"
//    r += "override implicit def equalityValue: shipreq.base.util.UnivEq[Value] = ???"
//    r += "override def attrs: shipreq.base.util.NonEmptySet[Attr] = ???"
    r += "override implicit def equalityAttr = ???"
    r += "override implicit def equalityValue = ???"
    r += "override def attrs = ???"

//    if (!o.toString.contains("CustomReqTypeGD"))
if (true)
      return r.result()

    println("-" * 120)
    println(o)

    import org.jetbrains.plugins.scala.lang.psi.impl.base.patterns.ScReferencePatternImpl

    /*
    println("allVals")
    for ((a, b) <- o.allVals) {
      println("============")
      println(s"  $a       \t$b")
      println(s"      ${a.getNode}")
//      println(s"      ${a.getReference}")
      if (a.toString.contains("Mnemonic")) {
        println(a.getClass)
      }
      a match {
        case c: ScReferencePatternImpl =>
          val t = c.getType(TypingContext.empty)
          println("type: " + t)
//          println(c.getReference)
//          println(c.getReference.getClass)
//          println(c.getReference.getElement)
          println(c.getOriginalElement())
        case x =>
          println("???")
      }
    }*/

    println()
    println()
    println("declaredElements")
    for (xx <- o.declaredElements)
      for (x <- xx.declaredElements) {
      println(s"  $x")
      println(s"    ${x.allVals}")
      println(s"    ${x.declaredElements}")
      println(s"    ${x.getFields}")
    }

    /*
    println("getAllFields")
    for (x <- o.getAllFields())
      println(s"  $x")

    println("declaredElements")
    for (x <- o.declaredElements)
      println(s"  $x")

    println(o.getObjectClassOrTraitToken)
    println(o.getObjectToken)
*/
//    println("\n------------------------")
//    val ss = TypeDefinitionMembers.getSignatures(o)
//    println(ss)
//    for {ns <- ss.allFirstSeq(); (s, n) <- ns} {
//      println(s"${s.name}        -  ${n.info.name}")
//    }
//    println()

//    println()
//    println("getCompanionModule...")
//    ScalaPsiUtil.getCompanionModule(o) match {
//      case Some(c) =>
//        println(c)
//
//        println("functions")
//        for (x <- o.functions)
//          println(s"  $x")
//
//
//      case None =>
//        println(None)
//    }

    println()
    r.result()
  }
}