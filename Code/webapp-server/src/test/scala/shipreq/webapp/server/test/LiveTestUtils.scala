package shipreq.webapp.server.test

import boopickle.Pickler
import net.liftweb.http.testing._
import org.apache.commons.httpclient.{HttpClient, HttpMethodBase}
import shipreq.base.test.BaseTestUtil._
import shipreq.base.util.FxModule._
import shipreq.webapp.base.protocol.ClientSideProc
import shipreq.webapp.base.protocol2.{BinaryJvm, Protocol}
import shipreq.webapp.server.app.Global
import shipreq.webapp.server.logic.{Cookie, Security}
import shipreq.webapp.server.security.SecurityInterpreter

/**
 * A test case that requires connectivity to a running Jetty instance.
 */
object LiveTestUtils {

  private def jetty = TestJetty

  val init: () => Unit = onceUnit {
    PrepareEnv.db()
    PrepareEnv.routes()
    jetty.start()
    _shutdown = onceUnit {
      //import Console._
      //println(s"$BLUE_B$BOLD${WHITE}SHUTTING DOWN!$RESET")
      jetty.shutdown()
      TestDb.shutdown()
    }
  }

  private var _shutdown: () => Unit =
    () => ()

  def shutdown(): Unit =
    _shutdown()

  lazy val testKit: TestKit =
    new TestKit {
      init()

      override def baseUrl = jetty.url

      implicit override def responseCapture(fullUrl: String, httpClient: HttpClient, getter: HttpMethodBase) = {
        getter.setFollowRedirects(false)
        super.responseCapture(fullUrl, httpClient, getter)
      }
    }
  import testKit.responseCapture

  def newDbConnection() = TestDb.newConnection()
  lazy val dbUtil = DbUtil(newDbConnection())
  lazy val userFixture = UserFixture(dbUtil.xa)
  implicit def dbAlgebra = PrepareEnv.dbAlgebra
  def xa = userFixture.xa

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  def get(url    : String,
          token  : Option[Security.SessionToken] = None,
          headers: List[(String, String)] = Nil,
          params : List[(String, String)] = Nil): HttpResponse = {
    val h2 = token.map(tokenCookie).toList
    testKit.get(url, testKit.theHttpClient, h2 ::: headers, params: _*).asInstanceOf[HttpResponse]
  }

  def post(url    : String,
           token  : Option[Security.SessionToken] = None,
           headers: List[(String, String)] = Nil,
           params : List[(String, String)] = Nil): HttpResponse = {
    val h2 = token.map(tokenCookie).toList
    testKit.post(url, testKit.theHttpClient, h2 ::: headers, params: _*).asInstanceOf[HttpResponse]
  }

  def ajaxPost(p: Protocol.Ajax[Pickler])
              (req: p.protocol.RequestType,
               token  : Security.SessionToken = Security.SessionToken.anonymous,
               headers: List[(String, String)] = Nil): HttpResponse = {
    val h2 = tokenCookie(token)
    val prep = p.protocol.prepareSend(req)
    val body = BinaryJvm.encode(p.prepReq)(prep.request).toNewArray
    testKit.post(p.url.relativeUrl, testKit.theHttpClient, h2 :: headers, body, "application/octet-stream")
      .asInstanceOf[HttpResponse]
  }

  private def tokenCookie(t: Security.SessionToken): (String, String) = {
    Global.security.sessionPersist(t).unsafeRun() match {
      case Cookie.Update(c :: Nil, Nil) => ("Cookie", s"${c.name.value}=${c.value}")
      case c => sys.error("Got: " + c)
    }
  }

  def retainSession(r: HttpResponse): List[(String, String)] =
      r.headers.getOrElse("Set-Cookie", Nil)
        .filter(_ contains "JSESSIONID")
        .map(v => ("Cookie", v.takeWhile(_ != ';')))

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final class LiveTestHttpResponse(private val resp: HttpResponse) extends AnyVal {
    def bodyString: String =
      resp.bodyAsString.openOrThrowException(s"Unable to read body from ${resp.body}")

    def bodyTitle: String =
      "(?<=<title>).*?(?=</title>)".r.findFirstIn(bodyString) getOrElse sys.error(s"Page doesn't have a <title> tag.\n$bodyString")

    def redirectedTo: Option[String] =
      resp.headers.get("Location").flatMap(_.headOption)

    def tap(f: HttpResponse => Any): HttpResponse = { f(resp); resp }
    def tap2[A](f: HttpResponse => A)(g: A => Any): HttpResponse = { g(f(resp)); resp }

    def assertStatus(expect: Int) = tap(_ =>
      if (resp.code != expect)
        fail(s"Expected status $expect, got ${resp.code}.\nHeaders: ${resp.headers}\nBody: $bodyString"))

    def assertOk = assertStatus(200)
    def assertRedirect = assertStatus(302)
    def assertRedirectTo(url: String) = assertRedirect.tap2(_.redirectedTo)(assertEq(_, Some(url)))

    def assertContentTypeContains(s: String) = tap2(_.contentType)(assertContains(_, s))
    def assertContentType        (s: String) = tap2(_.contentType)(assertEq(_, s))
    def assertContentTypeHtml                = assertContentTypeContains("text/html")
    def assertContentTypeJs                  = assertContentTypeContains("application/javascript")

    def assertBodyContains(s: String) = tap(_ => assertContains(bodyString, s))
    def assertBodyTitle(s: String) = tap2(_.bodyTitle)(assertEq(_, s))

    def assertSpa(spaJs: String, spaEP: ClientSideProc[_]) = this
        .assertOk
        .assertContentTypeHtml
        .assertBodyContains(spaJs)
        .assertBodyContains(spaEP.objectAndMethod + "(")

    def assertJwt(expect: Option[Security.SessionToken]) = {
      val prefix = SecurityInterpreter.cookieName.value + "="
      val cookieValue = resp.headers.getOrElse("Set-Cookie", Nil).find(_.startsWith(prefix)).map(_.drop(prefix.length))
      val actual = cookieValue.flatMap { v =>
        val m = Map(SecurityInterpreter.cookieName -> v.takeWhile(_ != ';'))
        Global.security.sessionRestore(m.get).unsafeRun()
      }
      assertEq(actual, expect)
      this
    }
  }

  implicit def toLiveTestHttpResponse(a: HttpResponse): LiveTestHttpResponse =
    new LiveTestHttpResponse(a)
}