package shipreq.idea

import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScGenericCall, ScReferenceExpression}
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScPatternDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScObject, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.SyntheticMembersInjector
import shipreq.idea.Utils._
import ShipReqInjector._

object ShipReqInjector {
  final val CreateGenericData = "shipreq.webapp.base.util.CreateGenericData"

  def hasCreateGenericData(o: ScObject): Boolean =
    o.annotations.exists(_.getQualifiedName == CreateGenericData)

  val GenericDataFns: List[String] =
    "override implicit def equalityAttr: Nothing = ???"  ::
    "override implicit def equalityValue: Nothing = ???" ::
    "override def attrs: Nothing = ???"                  ::
    Nil

  // "override implicit def equalityAttr: scalaz.Order[Attr] with shipreq.base.util.UnivEq[Attr] = ???" ::
  // "override implicit def equalityValue: shipreq.base.util.UnivEq[Value] = ???"                       ::
  // "override def attrs: shipreq.base.util.NonEmptySet[Attr] = ???"                                    ::
  // Nil
}

class ShipReqInjector extends SyntheticMembersInjector {

  override def injectInners(source: ScTypeDefinition): Seq[String] =
    source match {
      case o: ScObject if hasCreateGenericData(o) => addInners(o)
      case _ => Nil
    }

  override def injectFunctions(source: ScTypeDefinition): Seq[String] =
    source match {
      case o: ScObject if hasCreateGenericData(o) => GenericDataFns
      case _ => Nil
    }

  private def addInners(o: ScObject): Seq[String] = {
    val r = List.newBuilder[String]

    r += s"sealed abstract class Attr extends AttrBase"
    r += s"sealed abstract class Value extends ValueBase"

    for {
      member  <- o.members
      patDef  <- member.tryCast[ScPatternDefinition] if patDef.declaredElements.lengthCompare(1) == 0
      call    <- patDef.expr.flatMap(_.tryCast[ScGenericCall])
      refExpr <- call.referencedExpr.tryCast[ScReferenceExpression] if refExpr.refName == "defAttr"
      args    <- call.typeArgs.map(_.typeArgs) if args.lengthCompare(1) == 0
    } {
      val valName   = patDef.declaredElements.head.name
      val attrName  = valName //.replaceFirst("^_+", "").capitalize
      val attrType  = args.head.getText
      val valueName = "ValueFor" + attrName

      // println(s"$attrName: $attrType")

      val x1 =
        s"""
           |case object $attrName extends Attr {
           |  override type Data = $attrType
           |  override def apply(data: Data): $valueName = ???
           |}
         """.stripMargin.trim

      val x2 = fakeCaseClass1(valueName, "value", attrType, mod = "final", ext = "Value")(
        s"override def attr: $attrName.type = ???")

      r += x1
      r ++= x2

      // if (o.toString.contains("ReqCodeGroupGD")) {
      //   println(x1)
      //   x2 foreach println
      // }
    }

    r.result()
  }
}