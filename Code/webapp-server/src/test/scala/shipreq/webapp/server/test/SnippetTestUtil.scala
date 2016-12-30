package shipreq.webapp.server.test

import net.liftweb.http.js.JsCmd
import net.liftweb.http.{LiftSession, RedirectResponse, ResponseShortcutException, S}
import net.liftweb.util.Helpers.stringToSuper
import org.scalatest.Matchers.{fail => _, _}
import scala.annotation.tailrec
import scala.xml.NodeSeq
import scalaz.{-\/, Equal, \/, \/-}
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.server.util.NonEmptyTemplate

object SnippetTestUtil {

  def requireTemplate(name: String) = {
    PrepareEnv.lift()
    NonEmptyTemplate.load(name).get
  }

  def inLiftSession[U](block: => U): U = {
    import net.liftweb.common.Empty
    import net.liftweb.util.StringHelpers
    val session: LiftSession = new LiftSession("", StringHelpers.randomString(20), Empty)
    S.initIfUninitted(session)(block)
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

  implicit class JsCmdTestExt(private val j: JsCmd) extends AnyVal {
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
    override def test() = assertEq("notices", actual = S.getAllNotices, expect = Nil)(Equal.equalA)
  }
  case class HasErrorNoticeContaining(frag: String, frags: String*) extends SNoticeExp {
    val allFrags = frag :: frags.toList
    def check(i: String): Boolean = !allFrags.exists(f => !i.contains(f))
    override def test() = {
      val errors = S.errors.map(_._1.toString)
      if (!errors.exists(check)) fail("Expected error not found. Errors that occurred are2: " + errors)
    }
  }


}