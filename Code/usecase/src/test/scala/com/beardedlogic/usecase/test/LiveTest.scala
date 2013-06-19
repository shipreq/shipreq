package com.beardedlogic.usecase
package test

import org.apache.commons.httpclient.{HttpMethodBase, HttpClient}
import org.scalatest.{Suite, BeforeAndAfterAll}
import net.liftweb.http.testing._
import net.liftweb.json._
import LiveTestHelpers._

/**
 * A test case that requires connectivity to a running Jetty instance.
 */
trait LiveTest extends TestHelpers with TestKit with LiveTestHelpers with BeforeAndAfterAll with TestDatabaseSupport {
  self: Suite =>

  override def baseUrl = Jetty.Default.url

  override val wrapTestsInTransaction = false

  implicit val reportError = new ReportFailure {
    def fail(msg: String): Nothing = self.fail(s"Error: '$msg'")
  }

  override def beforeAll() {
    TestDatabaseSupport.init()
    Jetty.Default.acquire
  }

  override def afterAll() {
    Jetty.Default.release
  }
}

object LiveTestHelpers {

  implicit val JsonFormats = DefaultFormats.lossless

  implicit class ResponseTypeExt(val r: TestResponse) extends AnyVal {
    /**
     * Checks the result code of a HTTP request.
     */
    def !(code: Int)(implicit errorFunc: ReportFailure) = {
      val r2 = r.asInstanceOf[HttpResponse]
      r !(code, s"Expected $code. Got ${r2.code}")
    }
  }

  implicit def string2byteArray(s: String): Array[Byte] = s.getBytes("UTF-8")

  implicit class HttpResponseExt(val r: HttpResponse) extends AnyVal {
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
}

trait LiveTestHelpers {
  self: TestKit =>

  def jsonPut(url: String, value: JValue)(implicit capture: (String, HttpClient, HttpMethodBase) => ResponseType): self.ResponseType =
    put(url, compact(render(value)), "application/json")
}

