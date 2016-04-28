package shipreq.webapp.server.test

import java.io.File
import org.apache.commons.io.FileUtils
import org.apache.shiro.SecurityUtils
import org.apache.shiro.authc.UsernamePasswordToken
import org.scalatest.{BeforeAndAfterEach, Suite, BeforeAndAfterAll}
import org.scalatest.Matchers
import org.scalatest.mock.MockitoSugar
import net.liftweb.common.{Logger, Failure, Box, Empty}
import net.liftweb.http.{ResponseShortcutException, S, LiftSession, LiftRules}
import net.liftweb.http.js.JsCmd
import net.liftweb.mocks.MockHttpServletRequest
import net.liftweb.mockweb.MockWeb
import net.liftweb.util.StringHelpers
import net.liftweb.util.Helpers.stringToSuper
import scala.annotation.tailrec
import scala.util.Random
import shipreq.base.util._
import shipreq.taskman.api.{EmailAddr, UserId}
import shipreq.webapp.server.db._
import shipreq.webapp.server.security.SecurityProvider
import shipreq.webapp.server.app.DI
import shipreq.webapp.server.data._
import shipreq.webapp.server.lib.Types._

case class FixedUser(ud: Option[UserDescriptor]) extends SecurityProvider {
  override def loggedInUser = ud
  override def enforceHumanSpeed() = ()
  def install[R](fn: => R): R = DI.SecurityProvider.doWith(this)(fn)
}

private object TestHelperConsts {
  val Random = new Random()
}

/**
 * @since 30/04/2013
 */
trait TestHelpers2 extends MockitoSugar with Matchers with DebugImplicits with Logger {

  val Cores = Math.max(1, Runtime.getRuntime().availableProcessors - 1)

  implicit def JsCmdToStr(js: JsCmd): String = js.toJsCmd

  val UD1 = UserDescriptor(UserId(5001), Username("U1"), EmailAddr("U1@TEST"), Set.empty)
  val UD2 = UserDescriptor(UserId(5002), Username("U2"), EmailAddr("U2@TEST"), Set.empty)

  def rnd = TestHelperConsts.Random

  // -------------------------------------------------------------------------------------------------------------------

  def withUserLoggedIn[R](loggedInUser: Option[UserDescriptor])(fn: => R): R =
    FixedUser(loggedInUser).install(fn)

  def eventually(cond: => Any): Unit = {
    val test = (sleep: Int) => try { cond; true } catch { case _: Throwable => Thread.sleep(sleep); false }
    if (!test(10))
      if (!test(10))
        if (!test(20))
          if (!test(20))
            if (!test(50))
              if (!test(100))
                if (!test(1000))
                  cond
  }

  def eventuallyIf(wait: Boolean)(cond: => Any) { if (wait) eventually(cond) else cond }

  def any[T](implicit m: Manifest[T]) = org.mockito.Matchers.any(m.runtimeClass.asInstanceOf[Class[T]])
  def meq[T](v: T) = org.mockito.Matchers.eq(v)

  def countOccurrences(str1: String, str2: String): Int = {
    @tailrec def count(pos: Int, c: Int): Int = {
      val idx = str1 indexOf(str2, pos)
      if (idx == -1) c else count(idx + str2.size, c + 1)
    }
    count(0, 0)
  }

  def createTempDir(prefix: String, suffix: String = ""): File = {
    val tmpDir = File.createTempFile(prefix, suffix)
    tmpDir.delete
    tmpDir.mkdir
    FileUtils.forceDeleteOnExit(tmpDir)
    tmpDir
  }

  def findTransformable[I, O](inputs: IndexedSeq[I], eval: I => O)(test: O => Boolean): Option[(I, O)] = {
    val listSize = inputs.length
    @tailrec def go(attemptsRem: Int, pos: Int): Option[(I, O)] = {
      if (attemptsRem == 0) None
      else {
        val i = inputs(pos)
        val o = eval(i)
        if (test(o)) Some((i, o))
        else go(attemptsRem - 1, (pos + 1) % listSize)
      }
    }
    if (listSize == 0) None else go(listSize, rnd.nextInt(listSize))
  }

  def findSuitable[T](get: => T)(check: T => Boolean): T = {
    var t = get
    while (!check(t)) t = get
    t
  }

  def testListOfZeroOrOne[T](expectation: Option[Any], actual: List[T])(testFn: T => Any) {
    if (expectation.isEmpty)
      actual shouldBe empty
    else {
      actual should have size (1)
      testFn(actual(0))
    }
  }

  def login(username: String, password: String): Unit =
    SecurityUtils.getSubject.login(new UsernamePasswordToken(username, password))

  def logout(): Unit = SecurityUtils.getSubject.logout

  def inMockSession[U](block: => U): U = {
    val session: LiftSession = new LiftSession("", StringHelpers.randomString(20), Empty)
    S.initIfUninitted(session) {block}
  }

  def withSessionAttrs[U](attrs: (String, String)*)(block: => U): U = withSessionAttrs(Map(attrs: _*))(block)
  def withSessionAttrs[U](attrs: Map[String, String])(block: => U): U = inMockSession{S.withAttrs(S.mapToAttrs(attrs))(block)}

  def withSessionParams[U](params: Map[String, String])(block: => U): U = withSessionParams(params.toSeq: _*)(block)
  def withSessionParams[U](params: (String, String)*)(block: => U): U = {
    val req = new MockHttpServletRequest()
    req.parameters = params.toList
    MockWeb.testS(req)(block)
  }

  def assertJsAlert(jsReaction: String, errorMsg: Option[String]) {
    if (errorMsg.isDefined) {
      jsReaction.toLowerCase should include ("alert")
      jsReaction should include(errorMsg.get)
    } else
      jsReaction.toLowerCase should not include ("alert")
  }

  def assertJsErrorNotice(jsReaction: String, errorMsg: Option[String]) {
    if (errorMsg.isDefined) {
      jsReaction.toLowerCase should (include ("#notices") and include("alert-danger"))
      jsReaction should include(errorMsg.get.encJs.replaceAll("^\"|\"$", ""))
    } else {
      jsReaction.toLowerCase should not include ("#notices")
      jsReaction.toLowerCase should not include ("alert-danger")
    }
  }

  def assertRedirect(block: => Any): ResponseShortcutException = {
    val err = intercept[ResponseShortcutException](block)
    err.redirectTo should not be empty
    err
  }

  def withTestTaskman[R](f: => R): (R, TestTaskman) =
    TestTaskman.install(f)

  class Timer {
    val start = System.currentTimeMillis
    def elapsedMs = System.currentTimeMillis - start
    def elapsedSec = elapsedMs.toFloat / 1000f
    def elapsedSec2dp = "%.2f".format(elapsedSec)
  }

  def time[U](logFn: Float => Any)(fn: => U): U = {
    val t = new Timer
    val result = fn
    logFn(t.elapsedSec)
    result
  }

  // ===================================================================================================================

  /**
   * Extensions for: Any
   */
  implicit class AnyExt[T](val v: T) {
    // Equality assertion with type equivalence ala Specs2
    def ====(that: T): Unit = v should be(that)
  }

  /**
   * Extensions for: Int
   */
  implicit class MyRichInt(val i: Int) {
    def times(block: => Any) { 1 to i foreach(_ => block) }
  }

  /**
   * Extensions for: String
   */
  implicit class MyRichString(val self: String) {
    def occurrences(of: String) = countOccurrences(self, of)
  }

  /**
   * Extensions for: JsCmd
   */
  implicit class MyRichJsCmd(val j: JsCmd) {
    def assertJsAlert(errorMsg: Option[String]) = TestHelpers2.this.assertJsAlert(j.toJsCmd, errorMsg)
    def assertJsErrorNotice(errorMsg: Option[String]) = TestHelpers2.this.assertJsErrorNotice(j.toJsCmd, errorMsg)
  }

  /**
   * Extensions for: Box
   */
  implicit class MyRichBox[T](val b: Box[T]) {
    def gimme: T = b.openOrThrowException(s"Box was expected to be Full, but was: $b")
    def gimmeErr: String = b match {
      case Failure(err,_,_) => err
      case r => fail(s"Failure expected. Got: $r")
    }
  }

  implicit class CreateProjectResultExt(r: CreateProjectResult) {
    def gimme: ProjectId = r match {
      case CreateProjectResult.DbSuccess(x) => x
      case x => fail("Failed to create random project id: " + x)
    }
  }
}

object TestHelpers extends TestHelpers2 {
  def initLift(): Unit =
    if (!LiftRules.doneBoot) {
      val b = new bootstrap.liftweb.Boot
      b.configureLift()
      b.preloadTemplates()

      // Disable SecurityProvider.enforceHumanSpeed()
      val defaultSecProv = DI.SecurityProvider.default.get.vend
      DI.SecurityProvider.default.set(new SecurityProvider {
        def loggedInUser: Option[UserDescriptor] = defaultSecProv.loggedInUser
        override def enforceHumanSpeed() = ()
      })
    }
}

trait TestHelpers extends TestHelpers2 with BeforeAndAfterAll with BeforeAndAfterEach {
  this: Suite =>

  TestHelpers.initLift()

  override def beforeAll(): Unit = {
    if (!logoutBeforeEach) logout()
    super.beforeAll
  }

  override def beforeEach(): Unit = {
    if (logoutBeforeEach) logout()
    super.beforeEach
  }

  /** Logout performed before each test when true, and once before all tests when false. */
  var logoutBeforeEach = true
}
