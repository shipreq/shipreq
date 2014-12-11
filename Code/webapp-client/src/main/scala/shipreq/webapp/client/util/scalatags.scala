//package shipreq.webapp.client.util.ui.tablespec2

package japgolly.scalajs.react.vdom

import japgolly.scalajs.react._
import scala.scalajs.js

abstract class XxxxPri0 {
  import japgolly.scalajs.react.vdom.ReactVDom._
  import japgolly.scalajs.react.vdom.ReactVDom.{all => A}

  // LowPriUtil
  @inline implicit final def ___SeqFrag   [A <% Frag](xs: Seq[A])    = A.SeqFrag(xs)
  @inline implicit final def ___OptionFrag[A <% Frag](xs: Option[A]) = A.OptionFrag(xs)
  @inline implicit final def ___ArrayFrag [A <% Frag](xs: Array[A])  = A.ArrayFrag(xs)
}

object XxxxConst {
  trait AllHtmlTags extends ReactVDom.Cap with ReactTags with ReactTags2
  trait AllHtmlAttr extends ReactVDom.Cap with ReactVDom.Attrs with ReactVDom.ExtraAttrs with ReactVDom.Styles
  trait AllSvgTags extends ReactVDom.Cap with ReactSvgTags
  trait AllSvgAttr extends ReactVDom.Cap with ReactVDom.SvgAttrs

  object AllHtmlTags extends AllHtmlTags
  object AllHtmlAttr extends AllHtmlAttr
  object AllSvgTags extends AllSvgTags
  object AllSvgAttr extends AllSvgAttr
}

abstract class Xxxx extends XxxxPri0 {
  import ReactVDom.{Modifier => RM}
  import ReactVDom.{all => A}

  // TODO rename Tag & Modifier
  final type Tag = ReactVDom.Tag
  final type Modifier = RM

  @inline final def EmptyTag = ReactVDom.EmptyTag // TODO here?

  // Util
  @inline implicit final def ___SeqNode   [A <% RM](xs: Seq[A])    = new A.SeqNode(xs)
  @inline implicit final def ___OptionNode[A <% RM](xs: Option[A]) = new A.SeqNode(xs.toSeq)
  @inline implicit final def ___ArrayNode [A <% RM](xs: Array[A])  = new A.SeqNode[A](xs.toSeq)
  @inline implicit final def ___UnitNode           (u: Unit)       = A.UnitNode(u)
  // Aggregate
  @inline implicit final def ___stringAttr            = A.stringAttr
  @inline implicit final def ___booleanAttr           = A.booleanAttr
  @inline implicit final def ___byteAttr              = A.byteAttr
  @inline implicit final def ___shortAttr             = A.shortAttr
  @inline implicit final def ___intAttr               = A.intAttr
  @inline implicit final def ___longAttr              = A.longAttr
  @inline implicit final def ___floatAttr             = A.floatAttr
  @inline implicit final def ___doubleAttr            = A.doubleAttr
  @inline implicit final def ___stringStyle           = A.stringStyle
  @inline implicit final def ___booleanStyle          = A.booleanStyle
  @inline implicit final def ___byteStyle             = A.byteStyle
  @inline implicit final def ___shortStyle            = A.shortStyle
  @inline implicit final def ___intStyle              = A.intStyle
  @inline implicit final def ___longStyle             = A.longStyle
  @inline implicit final def ___floatStyle            = A.floatStyle
  @inline implicit final def ___doubleStyle           = A.doubleStyle
  @inline implicit final def ___byteFrag(v: Byte)     = A.byteFrag(v)
  @inline implicit final def ___shortFrag(v: Short)   = A.shortFrag(v)
  @inline implicit final def ___intFrag(v: Int)       = A.intFrag(v)
  @inline implicit final def ___longFrag(v: Long)     = A.longFrag(v)
  @inline implicit final def ___floatFrag(v: Float)   = A.floatFrag(v)
  @inline implicit final def ___doubleFrag(v: Double) = A.doubleFrag(v)
  @inline implicit final def ___stringFrag(v: String) = A.stringFrag(v)

  // -------------------------------------------------------------------------------------------------------------------
  // Custom
  import ReactVDom._

  implicit val jsThisFnAttr = new GenericAttr[js.ThisFunction](f => f)
  implicit val jsFnAttr = new GenericAttr[js.Function](f => f)
  implicit val jsObjAttr = new GenericAttr[js.Object](f => f)
  implicit def reactRefAttr[T <: Ref[_]] = new GenericAttr[T](_.name)

  implicit def reactNodeAsDomChild[T <% ReactNode](c: T): Modifier = new Modifier {
    override def applyTo(t: VDomBuilder): Unit = t.appendChild(c)
  }

  @inline implicit def autoRender(t: Tag)      : ReactElement      = t.render
  @inline implicit def autoRenderS(t: Seq[Tag]): Seq[ReactElement] = t.map(_.render)

  final def compositeAttr[A](k: Attr, f: (A, List[A]) => A, e: => Modifier = EmptyTag) =
    new CompositeAttr(k, f, e)

  val classSwitch = compositeAttr[String](all.cls, (h,t) => (h::t) mkString " ")

  @inline final def classSet(ps: (String, Boolean)*): Modifier =
    classSwitch(ps.map(p => if (p._2) Some(p._1) else None): _*)

  @inline final def classSet1(a: String, ps: (String, Boolean)*): Modifier =
    classSet(((a, true) +: ps):_*)

  @inline final def classSetM(ps: Map[String, Boolean]): Modifier =
    classSet(ps.toSeq: _*)

  @inline final def classSet1M(a: String, ps: Map[String, Boolean]): Modifier =
    classSet1(a, ps.toSeq: _*)
}

object prefix_<* extends Xxxx {
  @inline final def < = XxxxConst.AllHtmlTags
  @inline final def * = XxxxConst.AllHtmlAttr
  @inline final def svg_< = XxxxConst.AllSvgTags
  @inline final def svg_* = XxxxConst.AllSvgAttr
}

object prefix_<^ extends Xxxx {
  @inline final def < = XxxxConst.AllHtmlTags
  @inline final def ^ = XxxxConst.AllHtmlAttr
  @inline final def *< = XxxxConst.AllSvgTags
  @inline final def *^ = XxxxConst.AllSvgAttr
}

//object noprefix extends Xxxx with XxxxConst.AllHtmlTags with XxxxConst.AllHtmlAttr

object scalatags_test {
  import prefix_<*._

  lazy val H1 = ReactComponentB[String]("H").render(p => <.h1(p)).build

  def test(subj: ReactElement): Unit = ()
  def reactNode: ReactNode = H1("cool")

  test(<.div(123))
  test(<.div(123L))
  test(<.div(12.3))
  test(<.div(123: js.Number))
  test(<.div("yo"))
  test(<.div(reactNode))
  test(<.div(H1("a")))
  test(<.div("<div>hehe</div>"))
  test(<.div(Seq (<.span(1), <.span(2))))
  test(<.div(List(<.span(1), <.span(2))))
  test(<.div(List(H1("a"), H1("b"))))
  test(<.div(List(H1("a"), H1("b")).toJsArray))
  test(<.div(js.Array(<.span(1), <.span(2))))
  test(<.div(js.Array(H1("a"), H1("b"))))
  test(<.div(*.dangerouslySetInnerHtml("<span>")))
  test(<.div(*.cls := "hi", "Str: ", 123, js.Array(H1("a"), H1("b")), <.p(*.cls := "pp")("!")))
}