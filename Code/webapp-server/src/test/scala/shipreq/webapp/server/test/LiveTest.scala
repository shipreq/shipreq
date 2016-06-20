package shipreq.webapp.server
package test

import org.apache.commons.httpclient.{HttpMethodBase, HttpClient}
import org.scalatest.{Matchers, Suite, BeforeAndAfterAll}
import net.liftweb.http.testing._
import net.liftweb.json._

/**
 * A test case that requires connectivity to a running Jetty instance.
 */
trait LiveTest extends TestKit with LiveTestHelpers with BeforeAndAfterAll with Matchers {
  self: Suite =>

  private def jetty = TestJetty

  override def baseUrl = jetty.url

  implicit val reportError = new ReportFailure {
    def fail(msg: String): Nothing = self.fail(s"Error: '$msg'")
  }

  override def beforeAll(): Unit = {
    TestDb.init()
    jetty.start()
  }

  override def afterAll(): Unit = {
    TestDb.reinitOnNextUse()
  }
}

trait LiveTestHelpers {
  self: TestKit with Matchers =>

  def newSession() = TestDb.Slick.createSession()

  implicit val JsonFormats = DefaultFormats.lossless

  implicit class ResponseTypeExt(val r: TestResponse) {

    def asHttpResponse = r.asInstanceOf[HttpResponse]

    /** Checks the result code of a HTTP request. */
    def !(code: Int)(implicit errorFunc: ReportFailure) =
      r.!(code, s"Expected $code. Got ${asHttpResponse.code}. Response headers: ${asHttpResponse.headers}")

    def shouldRedirect(implicit errorFunc: ReportFailure) =
      r.!(302, s"Expected redirect (302). Got ${asHttpResponse.code}")

    def shouldRedirectTo(url: String)(implicit errorFunc: ReportFailure) = {
      shouldRedirect
      asHttpResponse.headers.get("Location").flatMap(_.headOption).map(_.replaceFirst("#.*", "")) shouldBe Some(url)
      asHttpResponse
    }
  }

  implicit def string2byteArray(s: String): Array[Byte] = s.getBytes("UTF-8")

  implicit class HttpResponseExt(val r: HttpResponse) {
    def map[T](f: HttpResponse => T): T = f(r)
    def flatMap[T](f: HttpResponse => Option[T]): Option[T] = f(r)
    def responseText = r.bodyAsString.openOrThrowException(s"Unable to read body from ${r.body}")
    /**
     * Converts the response body to JSON and then to a Scala class.
     *
     * Result will never be `Empty`. It's `Some()` so that it can be used in for-comprehensions.
     */
    def expectJson[T](implicit m: Manifest[T]): Some[T] = Some(parse(responseText).extract[T])
  }

  def jsonPut(url: String, value: JValue)(implicit capture: (String, HttpClient, HttpMethodBase) => ResponseType): self.ResponseType =
    put(url, compact(render(value)), "application/json")
}

