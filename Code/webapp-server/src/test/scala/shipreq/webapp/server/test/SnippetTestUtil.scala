package shipreq.webapp.server.test

import net.liftweb.common.Empty
import net.liftweb.http.js.JsCmd
import net.liftweb.http.{LiftSession, RedirectResponse, ResponseShortcutException, S}
import net.liftweb.util.Helpers.stringToSuper
import net.liftweb.util.StringHelpers
import org.mockito.Mockito.{never, times, verify, verifyNoMoreInteractions}
import org.scalatest.Matchers._
import scala.annotation.tailrec
import scala.xml.NodeSeq
import scalaz.{-\/, \/, \/-}
import shipreq.taskman.api.Msg
import shipreq.taskman.api.Msg._
import shipreq.webapp.server.app.DI
import shipreq.webapp.server.db.DaoT
import shipreq.webapp.server.util.NonEmptyTemplate

object SnippetTestUtil {

  def requireTemplate(name: String) = {
    PrepareEnv()
    NonEmptyTemplate.load(name).get
  }

  def countOccurrences(str1: String, str2: String): Int = {
    @tailrec def count(pos: Int, c: Int): Int = {
      val idx = str1 indexOf(str2, pos)
      if (idx == -1) c else count(idx + str2.size, c + 1)
    }
    count(0, 0)
  }

  def assertJsAlert(jsReaction: String, errorMsg: Option[String]) {
    if (errorMsg.isDefined) {
      jsReaction.toLowerCase should include ("alert")
      jsReaction should include(errorMsg.get)
    } else
      jsReaction.toLowerCase should not include "alert"
  }

  def assertJsErrorNotice(jsReaction: String, errorMsg: Option[String]) {
    if (errorMsg.isDefined) {
      jsReaction.toLowerCase should (include ("#notices") and include("alert-danger"))
      jsReaction should include(errorMsg.get.encJs.replaceAll("^\"|\"$", ""))
    } else {
      jsReaction.toLowerCase should not include "#notices"
      jsReaction.toLowerCase should not include "alert-danger"
    }
  }

  def any[T](implicit m: Manifest[T]) =
    org.mockito.Matchers.any(m.runtimeClass.asInstanceOf[Class[T]])

  def inMockSession[U](block: => U): U = {
    val session: LiftSession = new LiftSession("", StringHelpers.randomString(20), Empty)
    S.initIfUninitted(session) {block}
  }

  def withTestTaskman[R](f: => R): (R, TestTaskman) =
    TestTaskman.install(f)

  // ===================================================================================================================
  // Expectations: JS

  trait JsExp {
    a =>
    def test(js: JsCmd): Unit
    def &(b: JsExp): JsExp = new JsExp {override def test(js: JsCmd) = { a.test(js); b.test(js) }}
  }

  object NoErrorNotice extends JsExp {
    override def test(js: JsCmd) = assertJsErrorNotice(js.toJsCmd, None)
  }

  case class JsContains(frag: String, frags: String*) extends JsExp {
    override def test(js: JsCmd) = {
      val j = js.toJsCmd
      for (f <- frag +: frags) j should include(f)
    }
  }

  case class JsDoesntContain(frag: String, frags: String*) extends JsExp {
    override def test(js: JsCmd) = {
      val j = js.toJsCmd
      for (f <- frag +: frags) j should not include f
    }
  }

  case class HasErrorNotice(frag: String, frags: String*) extends JsExp {
    override def test(js: JsCmd) = {
      val j = js.toJsCmd
      for (f <- frag +: frags) assertJsErrorNotice(js.toJsCmd, Some(f))
    }
  }

  // ===================================================================================================================
  // Expectations: Taskman

  trait TestTaskmanExp {
    def test: TestTaskman => Unit
  }

  object NoTasksSubmitted extends TestTaskmanExp {
    override def test = _.msgsSubmitted shouldBe empty
  }

  type TaskTestPF = PartialFunction[Msg, Function0[Unit]]

  case class SubmittedOneTask(m: TaskTestPF) extends TestTaskmanExp {
    override def test = tt => tt.msgsSubmitted match {
      case t :: Nil => if (m isDefinedAt t) m(t)() else fail(s"Task didn't meet criteria: $t")
      case other    => other should have size 1
    }
  }

  private def absUrl(frag: String): String => Function0[Unit] =
    url => new Function0[Unit] {def apply = url should (startWith("http") and include(frag))}

  def RegistrationRequestedT(token: String): TaskTestPF =
    { case RegistrationRequested(_, url) => absUrl(token)(url) }

  val ReRegistrationAttemptedT: TaskTestPF =
    { case ReRegistrationAttempted(_) => () => () }

  val RegistrationCompletedT: TaskTestPF =
    { case RegistrationCompleted(_) => () => () }

  val PasswordResetRequestedT: TaskTestPF =
    { case PasswordResetRequested(_, url) => absUrl("/resetpw/")(url) }

  /**
   * Extensions for: JsCmd
   */
  implicit class MyRichJsCmd(private val j: JsCmd) extends AnyVal {
    def assertJsAlert(errorMsg: Option[String]) = SnippetTestUtil.this.assertJsAlert(j.toJsCmd, errorMsg)
    def assertJsErrorNotice(errorMsg: Option[String]) = SnippetTestUtil.this.assertJsErrorNotice(j.toJsCmd, errorMsg)
  }

  // ===================================================================================================================
  // Expectations: HTML

  trait HtmlExp {
    a =>
    def test(h: NodeSeq): Unit = test(h.toString)
    def test(h: String): Unit
    def &(b: HtmlExp): HtmlExp = new HtmlExp {override def test(html: String) = { a.test(html); b.test(html) }}
  }

  case class HtmlContains(frag: String, frags: String*) extends HtmlExp {
    override def test(html: String) = {
      for (f <- frag +: frags) html should include(f)
    }
  }

  case class HtmlDoesntContain(frag: String, frags: String*) extends HtmlExp {
    override def test(html: String) = {
      for (f <- frag +: frags) html should not include f
    }
  }

  // ===================================================================================================================
  // Expectations: Render

  def tryRender(f: => NodeSeq): Throwable \/ NodeSeq =
    try \/-(f) catch {case t: Throwable => -\/(t)}

  trait RenderExp {
    type R = Throwable \/ NodeSeq
    def test(r: R): Unit
  }

  trait ResponseShortcutExp extends RenderExp {
    override def test(r: R): Unit = r match {
      case -\/(e: ResponseShortcutException) => test(e)
      case _ => fail(s"ResponseShortcutException expected. Got: $r")
    }
    def test(r: ResponseShortcutException): Unit
  }

  object Redirects extends ResponseShortcutExp {
    def test(r: ResponseShortcutException): Unit = r.response shouldBe a [RedirectResponse]
  }

  case class RendersHtmlLike(he: HtmlExp) extends RenderExp {
    override def test(r: R): Unit = r match {
      case \/-(n) => he.test(n)
      case _ => fail(s"HTML expected. Got: $r")
    }
  }
  implicit def HtmlExpToRenderExp(he: HtmlExp): RenderExp = RendersHtmlLike(he)

  // ===================================================================================================================
  // Expectations: S.notices

  trait SNoticeExp {
    def test(): Unit
  }
  object NoNotices extends SNoticeExp {
    override def test() = S.getAllNotices shouldBe List.empty
  }
  case class HasErrorNoticeContaining(frag: String, frags: String*) extends SNoticeExp {
    val allFrags = frag :: frags.toList
    override def test() = S.errors.exists(e => check(e._1.toString)) shouldBe true
    def check(i: String): Boolean = !allFrags.exists(f => !i.contains(f))
  }

  // ===================================================================================================================
  // Setup

  trait DbSetup {
    def setup(d: DaoT): Unit
  }

  // ===================================================================================================================
  // Expectations: DB

  trait DbExp {
    final def test(): Unit = DI.DaoProvider.vend.withTransaction(dao => test(dao))
    def test(d: DaoT): Unit

    protected def verifyO[T](mock: T, on: Boolean) = verify(mock, if (on) times(1) else never)
  }

  object NoDbInteraction extends DbExp {
    override def test(d: DaoT) = verifyNoMoreInteractions(d)
  }
}
