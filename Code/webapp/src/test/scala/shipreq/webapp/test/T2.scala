package shipreq.webapp.test

import shipreq.webapp.app.DI
import shipreq.webapp.db.DaoT
import net.liftweb.http.js.JsCmd
import org.mockito.Mockito.{verify, times, never, verifyNoMoreInteractions}
import org.scalatest.Matchers
import xml.NodeSeq
import scalaz.{\/-, -\/, \/}
import net.liftweb.http.{S, RedirectResponse, ResponseShortcutException}

/**
 * Can't think of what else to call this. It's like Testing 2.0.
 * A better, more descriptive, more reusable approach.
 */
object T2 {

  // ===================================================================================================================
  // Setup

  trait DbSetup {
    def setup(d: DaoT): Unit
  }

  // ===================================================================================================================
  // Expectations: JS

  trait JsExp {
    a =>
    def test(js: JsCmd): Unit
    def &(b: JsExp): JsExp = new JsExp {override def test(js: JsCmd) = { a.test(js); b.test(js) }}
  }

  object NoErrorNotice extends JsExp {
    override def test(js: JsCmd) = TestHelpers.assertJsErrorNotice(js.toJsCmd, None)
  }

  case class JsContains(frag: String, frags: String*) extends JsExp with Matchers {
    override def test(js: JsCmd) = {
      val j = js.toJsCmd
      for (f <- (frag +: frags)) j should include(f)
    }
  }

  case class JsDoesntContain(frag: String, frags: String*) extends JsExp with Matchers {
    override def test(js: JsCmd) = {
      val j = js.toJsCmd
      for (f <- (frag +: frags)) j should not include(f)
    }
  }

  case class HasErrorNotice(frag: String, frags: String*) extends JsExp with Matchers {
    override def test(js: JsCmd) = {
      val j = js.toJsCmd
      for (f <- (frag +: frags)) TestHelpers.assertJsErrorNotice(js.toJsCmd, Some(f))
    }
  }

  // ===================================================================================================================
  // Expectations: Email

  trait EmailExp {
    def test(m: MailTestResult[_]): Unit
  }

  object NoEmailSent extends EmailExp {
    override def test(m: MailTestResult[_]) = m.assertNothingSent()
  }

  case class EmailSentContaining(frag: String, frags: String*) extends EmailExp {
    override def test(m: MailTestResult[_]) = m.assertSent((frag +: frags): _*)
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

  // ===================================================================================================================
  // Expectations: HTML

  trait HtmlExp {
    a =>
    def test(h: NodeSeq): Unit = test(h.toString)
    def test(h: String): Unit
    def &(b: HtmlExp): HtmlExp = new HtmlExp {override def test(html: String) = { a.test(html); b.test(html) }}
  }

  case class HtmlContains(frag: String, frags: String*) extends HtmlExp with Matchers {
    override def test(html: String) = {
      for (f <- (frag +: frags)) html should include(f)
    }
  }

  case class HtmlDoesntContain(frag: String, frags: String*) extends HtmlExp with Matchers {
    override def test(html: String) = {
      for (f <- (frag +: frags)) html should not include(f)
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

  trait ResponseShortcutExp extends RenderExp with Matchers {
    override def test(r: R): Unit = r match {
      case -\/(e: ResponseShortcutException) => test(e)
      case _ => fail(s"ResponseShortcutException expected. Got: $r")
    }
    def test(r: ResponseShortcutException): Unit
  }

  object Redirects extends ResponseShortcutExp {
    def test(r: ResponseShortcutException): Unit = r.response shouldBe a [RedirectResponse]
  }

  case class RendersHtmlLike(he: HtmlExp) extends RenderExp with Matchers {
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
  object NoNotices extends SNoticeExp with Matchers {
    override def test() = S.getAllNotices shouldBe List.empty
  }
  case class HasErrorNoticeContaining(frag: String, frags: String*) extends SNoticeExp with Matchers {
    val allFrags = frag :: frags.toList
    override def test() = S.errors.exists(e => check(e._1.toString)) shouldBe true
    def check(i: String): Boolean = !allFrags.exists(f => !i.contains(f))
  }
}
