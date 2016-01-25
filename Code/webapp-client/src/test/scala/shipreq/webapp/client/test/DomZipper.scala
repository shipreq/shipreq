package shipreq.webapp.client.test

import org.scalajs.dom.{Element, Node, window}
import org.scalajs.dom.html
import scala.reflect.ClassTag
import scala.scalajs.js
import DomZipper.{CssSelLookup, DOM, HndDown, Layer, MofN, Sole, showNameSel}

object DomZipper {
  type DOM = Node

  type CssSelLookup = (String, Node) => js.Array[Element]

  trait DomLike[A] {
    def apply(a: A): DOM
  }
  def DomLike[A](f: A => DOM): DomLike[A] =
    new DomLike[A] {
      override def apply(a: A) = f(a)
    }

  trait HndDown {
    type Result[A]
    def pass[A](a: A): Result[A]
    def fail[A](e: String): Result[A]
    def apply[A](e: Either[String, A]): Result[A] =
      e match {
        case Right(a) => pass(a)
        case Left(s) => fail(s)
      }
  }

  // ===================================================================================================================
  // Implicits that should be modular

  object Implicits {
    implicit val cssSelLookupSizzle: CssSelLookup =
      Sizzle(_, _)

    import japgolly.scalajs.react._
    implicit def domFromReact[A <: CompScope.Mounted[TopNode]]: DomLike[A] =
      DomLike(ReactDOM.findDOMNode)

    /*
    implicit object UseScalazEither extends EitherLike {
      import scalaz._
      override type Or[L, R] = L \/ R
      override def left[L, R](l: L)                               = -\/(l)
      override def right[L, R](r: R)                              = \/-(r)
      override def fold[L, R, O](x: L Or R)(l: L => O, r: R => O) = x.fold(l, r)
    }
    */

    implicit object JustThrow extends HndDown {
      override type Result[A]         = A
      override def pass[A](a: A)      = a
      override def fail[A](e: String) = sys error removeReactIds(e)
    }

    def removeReactIds(html: String): String =
      html.replaceAll(""" data-reactid=".*?"""", "")
  }

  // ===================================================================================================================

  def showNameSel(name: String, sel: String): String =
    (Option(name).filter(_.nonEmpty), Option(sel).filter(_.nonEmpty)) match {
      case (Some(n), Some(s)) => s"$n [$s]"
      case (None   , Some(s)) => s
      case (Some(n), None   ) => n
      case (None   , None   ) => "?"
    }

  case class Layer(name: String, sel: String, dom: DOM) {
    def show = showNameSel(name, sel)
  }

  def root(implicit $: CssSelLookup): DomZipper =
    new DomZipper(Vector.empty[Layer] :+ Layer("window.document", "", window.document), $)

  def apply[A](tgt: A)(implicit domLike: DomLike[A], $: CssSelLookup): DomZipper =
    apply("<manual>", tgt)

  def apply[A](name: String, tgt: A)(implicit domLike: DomLike[A], $: CssSelLookup): DomZipper =
    new DomZipper(Vector.empty[Layer] :+ Layer(name, "", domLike(tgt)), $)

  case class MofN(m: Int, n: Int) {
    override def toString = s"$m of $n"
    assert(n > 0, s"$this is invalid. $n must be > 0.")
    assert(m > 0, s"$this is invalid. $m must be > 0.")
    assert(m <= n, s"$this is invalid. $m must be ≤ $n.")
  }

  implicit class IntExt(private val i: Int) extends AnyVal {
    def of(n: Int) = MofN(i, n)
  }

  val Sole = 1 of 1
}

final class DomZipper private[test](layers: Vector[Layer], $: CssSelLookup) {
  assert(layers.nonEmpty)

  val dom: DOM =
    layers.last.dom

  def to_![D <: DOM]: D =
    dom.asInstanceOf[D]

  def to[D <: DOM](implicit ct: ClassTag[D]): Option[D] =
    ct.unapply(dom)

  def outerHTML: String = to[html.Element].fold("")(_.outerHTML)
  def innerHTML: String = to[html.Element].fold("")(_.innerHTML)
  def innerText: String = dom.textContent

  def downE(sel: String): Either[String, DomZipper] =
    downE("", sel)

  def downE(sel: String, which: MofN): Either[String, DomZipper] =
    downE("", sel, which)

  def downE(name: String, sel: String): Either[String, DomZipper] =
    downE(name, sel, Sole)

  def downE(name: String, sel: String, which: MofN): Either[String, DomZipper] = {
    val results = $(sel, dom)
    if (results.length != which.n)
      Left(failMsg(s"Query failed: ${showNameSel(name, sel)}. Expected ${which.n} results, not ${results.length}."))
    else {
      val nextLayer = Layer(name, sel, results(which.m - 1))
      Right(addLayer(nextLayer))
    }
  }

  def downO(sel: String)                           : Option[DomZipper] = downE(sel)             .right.toOption
  def downO(sel: String, which: MofN)              : Option[DomZipper] = downE(sel, which)      .right.toOption
  def downO(name: String, sel: String)             : Option[DomZipper] = downE(name, sel)       .right.toOption
  def downO(name: String, sel: String, which: MofN): Option[DomZipper] = downE(name, sel, which).right.toOption

  def down(sel: String)                           (implicit h: HndDown): h.Result[DomZipper] = h(downE(sel)             )
  def down(sel: String, which: MofN)              (implicit h: HndDown): h.Result[DomZipper] = h(downE(sel, which)      )
  def down(name: String, sel: String)             (implicit h: HndDown): h.Result[DomZipper] = h(downE(name, sel)       )
  def down(name: String, sel: String, which: MofN)(implicit h: HndDown): h.Result[DomZipper] = h(downE(name, sel, which))

//  def down_!(sel: String)                           (implicit e: HndDown): DomZipper = need_!(e)(down(sel))
//  def down_!(sel: String, which: MofN)              (implicit e: HndDown): DomZipper = need_!(e)(down(sel, which))
//  def down_!(name: String, sel: String)             (implicit e: HndDown): DomZipper = need_!(e)(down(name, sel))
//  def down_!(name: String, sel: String, which: MofN)(implicit e: HndDown): DomZipper = need_!(e)(down(name, sel, which))

  private def addLayer(nextLayer: Layer) =
    new DomZipper(layers :+ nextLayer, $)

  def describe: String =
    s"DESC: ${layers.map(_.show) mkString " → "}\nHTML: $outerHTML"

  private def failMsg(msg: String): String =
    msg + "\n" + describe

  // ======= hmmmm… =======

  def getAll(sel: String) =
    $(sel, dom)

  def collectDom[A](sel: String, f: DOM => A): Vector[A] =
    getAll(sel).foldLeft(Vector.empty[A])(_ :+ f(_))

  def collect[A](sel: String, f: DomZipper => A): Vector[A] =
    collectDom(sel, d => f(addLayer(Layer("collect", sel, d))))

  def collectInnerHTML[A](sel: String): Vector[String] =
    collect(sel, _.innerHTML)

  def collectInnerText[A](sel: String): Vector[String] =
    collectDom(sel, _.textContent)

  // ======= hmmmm… =======

  def getAll1(sel: String) = {
    val x = getAll(sel)
    assert(x.nonEmpty, s"No matches found for $sel") // TODO nooo.....
    x
  }

  def collectDom1[A](sel: String, f: DOM => A): Vector[A] =
    getAll1(sel).foldLeft(Vector.empty[A])(_ :+ f(_))

  def collect1[A](sel: String, f: DomZipper => A): Vector[A] =
    collectDom1(sel, d => f(addLayer(Layer("collect", sel, d))))

  def collectInnerHTML1[A](sel: String): Vector[String] =
    collect1(sel, _.innerHTML)

  def collectInnerText1[A](sel: String): Vector[String] =
    collectDom1(sel, _.textContent)

  // ======= hmmmm… =======

  def inputChecked: Option[Boolean] =
    to[html.Input].map(_.checked)

  /** The currently selected option in a &lt;select&gt; dropdown. */
  def selectedOption: Option[html.Option] =
    to[html.Select].flatMap(s =>
      if (s.selectedIndex >= 0)
        Some(s.options(s.selectedIndex))
      else
        None
    )

  /** The text value of the currently selected option in a &lt;select&gt; dropdown. */
  def selectedOptionText: Option[String] =
    selectedOption.map(_.text)
}

//  def assertCount(desc: String, expectedCount: Int, dom: Result, root: UndefOr[DOM]): Unit = {
//    def showDom(inner: Boolean)(d: DOM) = {
//      val html = if (inner) d.innerHTML else d.outerHTML
//      "\n" + removeReactIds(html).take(160)
//    }
//    def detail =
//      if (dom.isEmpty)
//        root.fold("")(showDom(true))
//      else
//        dom.map(showDom(false))
//    assertEq(desc + detail, dom.length, expectedCount)
//  }
//
//  def first(desc: String, dom: Result): DomZipper = {
//    if (dom.isEmpty)
//      fail(desc + ": empty")
//    new DomZipper(dom.head)
//  }
//
//  implicit val equality: Equal[DomZipper] =
//    Equal.equal((a, b) => a.get isSameNode b.get)
//}
//
//  def getAll(expectedCount: Int, sel: String): Result = {
//    val r = Sizzle(sel, root)
//    DomZipper.assertCount(sel, expectedCount, r, root)
//    r
//  }
