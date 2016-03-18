package shipreq.webapp.client.test.domzipper

import org.scalajs.dom.{Element, Node, window}
import org.scalajs.dom.html
import scala.reflect.ClassTag
import scala.scalajs.js
import shipreq.webapp.client.test.Sizzle

object DomZipper {
  type DOM = Node

  type CssSelLookupResult = js.Array[Element]
  type CssSelLookup = (String, Node) => CssSelLookupResult

  trait DomLike[-A] {
    type D <: DOM
    def apply(a: A): D
  }
  type DomLikeAux[-A, D2 <: DOM] = DomLike[A] {type D = D2}
  def DomLike[A, D2 <: DOM](f: A => D2): DomLikeAux[A, D2] =
    new DomLike[A] {
      override type D = D2
      override def apply(a: A) = f(a)
    }

  trait HandleError {
    type Result[A]
    def pass[A](a: A): Result[A]
    def fail[A](e: => String): Result[A]
    def map[A, B](r: Result[A])(f: A => B): Result[B]

    def apply[A](e: Either[String, A]): Result[A] =
      e match {
        case Right(a) => pass(a)
        case Left(s) => fail(s)
      }
  }

  object ThrowErrors extends HandleError {
    override type Result[A]                         = A
    override def pass[A](a: A)                      = a
    override def fail[A](e: => String)              = sys error e
    override def map[A, B](r: Result[A])(f: A => B) = f(r)
  }

  object ReturnWithEitherMsg extends HandleError {
    override type Result[A]                         = Either[String, A]
    override def pass[A](a: A)                      = Right(a)
    override def fail[A](e: => String)              = Left(e)
    override def map[A, B](r: Result[A])(f: A => B) = r.right map f
    override def apply[A](e: Either[String, A])     = e
  }

  object ReturnOption extends HandleError {
    override type Result[A]                         = Option[A]
    override def pass[A](a: A)                      = Some(a)
    override def fail[A](e: => String)              = None
    override def map[A, B](r: Result[A])(f: A => B) = r map f
  }

  object ReturnScalazDisjunction extends HandleError {
    import scalaz._
    override type Result[A]                         = String \/ A
    override def pass[A](a: A)                      = \/-(a)
    override def fail[A](e: => String)              = -\/(e)
    override def map[A, B](r: Result[A])(f: A => B) = r map f
  }

  // ===================================================================================================================
  // Implicits that should be modular

  object Implicits {
    implicit val cssSelLookupSizzle: CssSelLookup =
      Sizzle(_, _)

    import japgolly.scalajs.react._
    implicit def domFromReact[D <: TopNode with DOM]: DomLikeAux[CompScope.Mounted[D], D] =
      DomLike(ReactDOM.findDOMNode)

    implicit val hndError = DomZipper.ThrowErrors

    def removeReactIds(html: String): String = // TODO Remove
      html.replaceAll(""" data-reactid=".*?"""", "")

    implicit class IntExt(private val i: Int) extends AnyVal {
      def of(n: Int) = MofN(i, n)
    }
  }

  // ===================================================================================================================

  def showNameSel(name: String, sel: String): String =
    (Option(name).filter(_.nonEmpty), Option(sel).filter(_.nonEmpty)) match {
      case (Some(n), Some(s)) => s"$n [$s]"
      case (None   , Some(s)) => s
      case (Some(n), None   ) => n
      case (None   , None   ) => "?"
    }

  case class Layer[+D <: DOM](name: String, sel: String, dom: D) {
    def show = showNameSel(name, sel)
  }

  def root(implicit $: CssSelLookup): DomZipperAt[DOM] =
    new DomZipperAt[DOM](Vector.empty, Layer("window.document", "", window.document), $)

  def apply[A](tgt: A)(implicit domLike: DomLike[A], $: CssSelLookup): DomZipperAt[domLike.D] =
    apply("<manual>", tgt)

  def apply[A](name: String, tgt: A)(implicit domLike: DomLike[A], $: CssSelLookup): DomZipperAt[domLike.D] =
    new DomZipperAt(Vector.empty, Layer(name, "", domLike(tgt)), $)

  case class MofN(m: Int, n: Int) {
    override def toString = s"$m of $n"
    assert(n > 0, s"$this is invalid. $n must be > 0.")
    assert(m > 0, s"$this is invalid. $m must be > 0.")
    assert(m <= n, s"$this is invalid. $m must be ≤ $n.")
  }

  val Sole = MofN(1, 1)

  // ===================================================================================================================

  val EditableSel: String =
    List("input", "textarea", "select")
      .map(_ + ":not(:disabled)") // :not(:read-only) | Firefox errors out when :read-only is used
      .mkString(",")

  trait Container[C[_]] {
    def apply(sel: String, es: CssSelLookupResult)(implicit h: HandleError): h.Result[C[Element]]
    def map[A, B](c: C[A])(f: A => B): C[B]
  }

  object Container01 extends Container[Option] {
    override def apply(sel: String, es: CssSelLookupResult)(implicit h: HandleError) =
      es.length match {
        case 0 => h pass None
        case 1 => h pass Some(es.head)
        case n => h fail s"$n matches found for: $sel"
      }
    override def map[A, B](c: Option[A])(f: A => B) =
      c map f
  }

  object Container0N extends Container[Vector] {
    override def apply(sel: String, es: CssSelLookupResult)(implicit h: HandleError) =
      h pass es.toVector
    override def map[A, B](c: Vector[A])(f: A => B) =
      c map f
  }

  object Container1N extends Container[Vector] {
    override def apply(sel: String, es: CssSelLookupResult)(implicit h: HandleError) =
      if (es.isEmpty)
        h fail s"No matches found for: $sel"
      else
        h pass es.toVector
    override def map[A, B](c: Vector[A])(f: A => B) =
      c map f
  }

  final class Collector[C[_], E <: Element](from: DomZipperAt[DOM], sel: String, cont: Container[C]) {
    def as[EE <: E] =
      this.asInstanceOf[Collector[C, EE]]

    @inline def asHtml(implicit ev: html.Element <:< E) =
      this.asInstanceOf[Collector[C, html.Element]]

    def get()(implicit h: HandleError): h.Result[C[E]] = {
      val e1: h.Result[C[Element]] = cont(sel, from.directSelect(sel))
      val e2: h.Result[C[E]]       = e1.asInstanceOf[h.Result[C[E]]]
      e2
    }

    def getDZ()(implicit h: HandleError): h.Result[C[DomZipperAt[E]]] =
      h.map(get())(cont.map(_)(d => from.addLayer(Layer("collect", sel, d))))

    def mapDom[A](f: E => A)(implicit h: HandleError): h.Result[C[A]] =
      h.map(get())(cont.map(_)(f))

    def map[A](f: DomZipperAt[E] => A)(implicit h: HandleError): h.Result[C[A]] =
      mapDom(d => f(from.addLayer(Layer("collect", sel, d))))

    def innerHTML[A]()(implicit h: HandleError): h.Result[C[String]] =
      map(_.innerHTML)

    def innerText[A]()(implicit h: HandleError): h.Result[C[String]] =
      map(_.innerText)
  }
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████
import DomZipper._

final class DomZipperAt[+D <: DOM] private[test](prevLayers: Vector[Layer[DOM]],
                                                 curLayer: Layer[D],
                                                 $: CssSelLookup) {

  def directSelect(sel: String): js.Array[Element] =
    $(sel, dom)

  val dom: D =
    curLayer.dom

    @inline def asHtml(implicit h: HandleError) = as[html.Element]

  def as[D2 <: DOM](implicit h: HandleError, ct: ClassTag[D2]): h.Result[DomZipperAt[D2]] =
    h.map(domAs[D2])(d =>
      new DomZipperAt(prevLayers, curLayer.copy(dom = d), $))

  def domAs[D2 <: DOM](implicit h: HandleError, ct: ClassTag[D2]): h.Result[D2] =
    ct.unapply(dom) match {
      case Some(d) => h pass d
      case None => h fail s"${dom.nodeName} is not a ${ct.runtimeClass}."
    }

  def dynamicMethod[A](f: js.Dynamic => Any): Option[A] =
    f(dom.asInstanceOf[js.Dynamic]).asInstanceOf[js.UndefOr[A]].toOption

  def dynamicString(f: js.Dynamic => Any): String =
    dynamicMethod(f) getOrElse "<undefined>"

  import Implicits.removeReactIds
  def outerHTML: String = removeReactIds(dynamicString(_.outerHTML)) // TODO removeReactIds
  def innerHTML: String = removeReactIds(dynamicString(_.innerHTML)) // TODO removeReactIds
  def innerText: String = dom.textContent
  def value: String = dynamicString(_.value)

  def down(sel: String)(implicit h: HandleError): h.Result[DomZipperAt[DOM]] =
    down("", sel)

  def down(sel: String, which: MofN)(implicit h: HandleError): h.Result[DomZipperAt[DOM]] =
    down("", sel, which)

  def down(name: String, sel: String)(implicit h: HandleError): h.Result[DomZipperAt[DOM]] =
    down(name, sel, Sole)

  def down(name: String, sel: String, which: MofN)(implicit h: HandleError): h.Result[DomZipperAt[DOM]] = {
    val results = $(sel, dom)
    if (results.length != which.n)
      h.fail {
        val q = Option(name).filter(_.nonEmpty).fold("Q")(_ + " q")
        failMsg(s"${q}uery failed: [$sel]. Expected ${which.n} results, not ${results.length}.")
      }
    else {
      val nextLayer = Layer(name, sel, results(which.m - 1))
      h.pass(addLayer(nextLayer))
    }
  }

  private[test] def addLayer[D2 <: DOM](nextLayer: Layer[D2]) =
    new DomZipperAt(prevLayers :+ curLayer, nextLayer, $)

  def allLayers =
    prevLayers :+ curLayer

  def describe: String =
    s"DESC: ${allLayers.map(_.show) mkString " → "}\nHTML: $outerHTML"

  private def failMsg(msg: String): String =
    msg + "\n" + describe

  def inputChecked(implicit h: HandleError): h.Result[Boolean] =
    h.map(domAs[html.Input])(_.checked)

  /** The currently selected option in a &lt;select&gt; dropdown. */
  def selectedOption(implicit h: HandleError): h.Result[Option[html.Option]] =
    h.map(domAs[html.Select])(s =>
      if (s.selectedIndex >= 0)
        Some(s.options(s.selectedIndex))
      else
        None
    )

  /** The text value of the currently selected option in a &lt;select&gt; dropdown. */
  def selectedOptionText(implicit h: HandleError): h.Result[Option[String]] =
    h.map(selectedOption)(_.map(_.text))

  def collect01(sel: String) = new Collector[Option, Element](this, sel, Container01)
  def collect0n(sel: String) = new Collector[Vector, Element](this, sel, Container0N)
  def collect1n(sel: String) = new Collector[Vector, Element](this, sel, Container1N)

  def editables0n = collect0n(EditableSel)
  def editables1n = collect1n(EditableSel)

  def findSelfOrChildWithAttribute(attr: String)(implicit h: HandleError): h.Result[Option[DomZipperAt[Element]]] =
    dom.attributes.getNamedItem(attr) match {
      case null => collect01(s"*[$attr]").getDZ()
      case _    => h.map(as[Element])(Some(_))
    }
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
//  def first(desc: String, dom: Result): DomZipperAt = {
//    if (dom.isEmpty)
//      fail(desc + ": empty")
//    new DomZipperAt(dom.head)
//  }
//
//  implicit val equality: Equal[DomZipperAt] =
//    Equal.equal((a, b) => a.get isSameNode b.get)
//}
//
//  def getAll(expectedCount: Int, sel: String): Result = {
//    val r = Sizzle(sel, root)
//    DomZipperAt.assertCount(sel, expectedCount, r, root)
//    r
//  }