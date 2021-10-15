package shipreq.webapp.base.test

import japgolly.microlibs.testutil.TestUtil
import japgolly.scalajs.react.test._
import japgolly.scalajs.react.vdom.html_<^.VdomAttr
import org.scalajs.dom
import org.scalajs.dom.{document, html}
import scala.scalajs.js
import scalacss.internal.StyleA
import shipreq.base.test.BaseTestUtil
import shipreq.base.util.{Debug, Disabled, Enabled, ErrorMsg}
import shipreq.webapp.base.util.DomUtil._
import sourcecode.Line
import teststate.domzipper.DomZipperJsF.Dom
import teststate.run.Report.AssertionSettings

// TODO: trait ExtCats extends teststate.typeclass.Equal.ImplicitsLowPri {
// import cats._
// import cats.arrow.Profunctor
// import teststate.Exports._
// import teststate.{typeclass => T}

//   implicit def catsMonoidMonoComposableEmpty[Op, A](implicit e: Empty[A], c: T.PolyComposable.Mono[Op, A]): Monoid[A] =
//     new Monoid[A] {
//       override def empty               = e.instance
//       override def combine(x: A, y: A) = c.compose(x, y)
//     }

//   implicit lazy val catsMonoidReportStats: Monoid[Report.Stats] =
//     new Monoid[Report.Stats] {
//       override def empty                                     = Report.Stats.empty
//       override def combine(x: Report.Stats, y: Report.Stats) = x + y
//     }

//   implicit def catsProfunctorFromTestState[M[_, _]](implicit p: T.Profunctor[M]): Profunctor[M] =
//     new Profunctor[M] {
//       override def lmap [A, B, C]   (m: M[A, B])(f: C => A)            = p.lmap(m)(f)
//       override def rmap [A, B, C]   (m: M[A, B])(f: B => C)            = p.rmap(m)(f)
//       override def dimap[A, B, C, D](m: M[A, B])(f: C => A)(g: B => D) = p.dimap(m)(f, g)
//     }

//   implicit def catsNatTransFromTestState[F[_], G[_]](implicit t: F ~> G): T.~~>[F, G] =
//     new T.~~>[F, G] { def apply[A](fa: => F[A]) = t(fa) }

//   implicit def catsNatTransToTestState[F[_], G[_]](implicit t: T.~~>[F, G]): F ~> G =
//     new (F ~> G) { def apply[A](fa: F[A]) = t(fa) }

//   implicit def catsEqualFromTestState[A](implicit e: Eq[A]): Eq[A] =
//     Eq.instance(e.equal)

//   implicit def catsEqualToTestState[A](implicit e: Eq[A]): Eq[A] =
//     Equal(e.eqv)
// }

object TestState
 extends teststate.Exports
    with teststate.domzipper.sizzle.Exports
    // with teststate.ExtCats
    // with ExtCats
    with teststate.ExtNyaya
    with teststate.ExtScalaJsReact
    with Debug.Implicits {

  type Id[A] = A

  type Eq[A] = cats.Eq[A]

  type DomZipperTo[A] = DomZipperJsF[Id, A]

  implicit def BaseTestUtilOpsOption[A](a: Option[A]) =
    new BaseTestUtil.BaseTestUtilOpsOption(a)

  implicit def BaseTestUtilOpsSeq[A](a: Seq[A]) =
    new BaseTestUtil.BaseTestUtilOpsSeq(a)

  implicit final class TestStateStyleAExt(private val self: StyleA) extends AnyVal {
    def selector: String =
      "." + self.className.value
  }

  implicit final class TestStateElementExt(private val self: html.Element) extends AnyVal {
    def get(v: VdomAttr[_])(implicit srcFile: sourcecode.File, srcLine: sourcecode.Line): String =
      Option(v.attrName.trim).filter(_.nonEmpty) match {
        case Some(n) =>
          if (n.startsWith("data-"))
            self.dataset.get(n.drop(5)).getOrElse("")
          else
            Option(self.attributes.getNamedItem(n)).flatMap(x => Option(x.value)).getOrElse("")
        case None =>
          ErrorMsg(s"html.Element.get(∅) called from ${srcFile.value}:${srcLine.value}").throwException()
      }
  }

  implicit final class TestStateTextAreaExt(private val self: html.TextArea) extends AnyVal {
    def selectionRange: (Int, Int) =
      (self.selectionStart, self.selectionEnd)
  }

//  implicit val displayTestReq: Display[TestClientProtocol.Req] =
//    Display(i => s"${i.proc.protocol}: ${i.input}")

  override implicit def testStateErrorHandler: ErrorHandler[String] =
    ErrorHandler.toStringWithStackTrace("shipreq|scalajs.dom".r.pattern)

  def KB = japgolly.scalajs.react.test.SimEvent.Keyboard

  def dispatchEvent(target   : dom.EventTarget,
                    eventName: String,
                    mod      : dom.Event => Unit = null): Unit = {
    val name = eventName.toLowerCase
    val interface =
      if (name.startsWith("mouse") || name.contains("click"))
        "MouseEvents" // pluralised - not a typo. See MDN
      else if (name.startsWith("key"))
        "KeyboardEvents" // pluralised - not a typo. See MDN
      else
        "Event" // singular - not a typo. See MDN
    val event = document.createEvent(interface)
    event.asInstanceOf[js.Dynamic].initEvent(name, true, true)
    if (mod ne null)
      mod(event)
    target.dispatchEvent(event)
  }

  final val y = true
  final val n = false

  def assertTestState(r: Report[String], onFailure: => Unit = ())
                     (implicit as: AssertionSettings, se: DisplayError[String], l: Line): Unit =
    r.failureReason match {
      case None =>
        // as.onPass.print(r)
      case Some(f) =>
        onFailure
        as.onFail.print(r)
        // f.cause.foreach(_.printStackTrace())
        TestUtil.fail(f.failure)
    }

  def editableDomValue(d: html.Element): String =
    d match {
      case i: html.Input    => i.value
      case t: html.TextArea => t.value
    }

  private val semanticUiClasses: Set[String] =
    Set("input", "dropdown", "button")

  def collectSemanticUi($: DomZipperJs): DomZipper.DomCollection[DomZipperJsF, Id, Vector, Dom, Dom] =
    collectSemanticUi($, None)

  def collectSemanticUi($: DomZipperJs, e: Enabled): DomZipper.DomCollection[DomZipperJsF, Id, Vector, Dom, Dom] =
    collectSemanticUi($, Some(e))

  def collectSemanticUi($: DomZipperJs, e: Option[Enabled]): DomZipper.DomCollection[DomZipperJsF, Id, Vector, Dom, Dom] = {
    def withUi = semanticUiClasses.iterator.map(".ui." + _)
    def types: Iterator[String] =
      e match {
        case None           => withUi
        case Some(Enabled)  => withUi.map(sel => s"$sel:not(:disabled):not(.disabled)")
        case Some(Disabled) => withUi.flatMap(sel => s"$sel:disabled" :: s"$sel.disabled" :: Nil)
      }
    $.collect0n(types.mkString(","))
      .filter(_.domAsHtml.findParent(e => semanticUiClasses.exists(e.classList.contains)).isEmpty)
  }

  // ===================================================================================================================

  class OptionalEditorObs($: DomZipperJs) {
    def container()     = $.domAsHtml
    private val editorO = $.collect01("textarea").domsAs[html.TextArea]
    def editor()        = editorO.get
    val editorValue     = editorO.fold("")(_.value)
  }

  class OptionalEditorDslBase[R, O, S](final private val * : Dsl[Id, R, O, S, String],
                                       editorCount: Dsl[Id, R, O, S, String]#FocusValue[Int]) {

    final type Actions = TestState.Actions[Id, R, O, S, String]

    final type FocusValue[A] = Dsl[Id, R, O, S, String]#FocusValue[A]

    abstract class Core(f: O => OptionalEditorObs, label: => String) {

      protected def openEditor: Actions

      final def editorValue: FocusValue[String] =
        *.focus(label + " editor value").value(x => f(x.obs).editorValue)

      final def edit(newValue: String): Actions =
        edit(newValue, 1)

      final def edit(expectedAndNewValue: (String, String)): Actions =
        edit(expectedAndNewValue, 1)

      final def edit(newValue: String, editors: Int): Actions =
        _editCell(None, newValue, editors)

      final def edit(expectedAndNewValue: (String, String), editors: Int): Actions =
        _editCell(Some(expectedAndNewValue._1), expectedAndNewValue._2, editors)

      private def _editCell(old: Option[String], newValue: String, editors: Int): Actions =
        (openEditor
          +> editorCount.assert.increaseBy(editors)
          +> editorValue.rename("Initial editor value").assert.equalWhenDefined(old)
          >> setEditValue(newValue)
          >> commit
          +> editorCount.assert.decreaseBy(editors)
          ).group(s"Edit $label to ${newValue.quote}")

      final def setEditValue(newValue: String): Actions =
        *.action(s"Set $label to ${newValue.quote}")(x =>
          SimEvent.Change(newValue) simulate f(x.obs).editor())

      final def commit: Actions =
        *.action(s"Commit $label editor value")(x => KB.Enter.ctrl simulateKeyDown f(x.obs).editor())
    }

    class ForCell(f: O => OptionalEditorObs, label: => String) extends Core(f, label) {
      override final def openEditor = doubleClick

      final def doubleClick: Actions =
        *.action(s"Double-click $label")(x => Simulate doubleClick f(x.obs).container())
    }

    class ForFormWithButton(f: O => OptionalEditorObs, label: => String, button: O => html.Element) extends Core(f, label) {
      override final def openEditor = clickNew

      final def clickNew: Actions =
        *.action(s"Click $label button")(x => Simulate click button(x.obs))
    }
  }
}
