package com.beardedlogic.usecase
package api

import org.apache.commons.httpclient.{HttpMethodBase, HttpClient}
import org.scalatest.{Suite, BeforeAndAfterAll}
import net.liftweb.http.testing._
import net.liftweb.json._
import test.{TestDatabaseSupport, Jetty, TestHelpers}
import ApiTestHelpers._

/**
 * Configures a test case to test APIs, and provides helpful functionality to aid in the testing.
 */
trait ApiTest extends TestHelpers with TestKit with ApiTestHelpers with BeforeAndAfterAll {
  self: Suite =>

  override def baseUrl = Jetty.URL

  implicit val reportError = new ReportFailure {
    def fail(msg: String): Nothing = self.fail(s"Error: '$msg'")
  }

  override def beforeAll() {
    TestDatabaseSupport.init()
    Jetty.acquire
  }

  override def afterAll() {
    Jetty.release
  }
}

object ApiTestHelpers {

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

    /**
     * Converts the response body to JSON and then to a Scala class.
     */
    def expectJson[T](implicit m: Manifest[T]): Some[T] = {
      val body = r.bodyAsString.openOrThrowException(s"Unable to read body from ${r.body}")
      val t: T = parse(body).extract[T]
      Some(t)
    }
  }
}

trait ApiTestHelpers {
  self: TestKit =>

  def jsonPut(url: String, value: JValue)(implicit capture: (String, HttpClient, HttpMethodBase) => ResponseType): self.ResponseType =
    put(url, compact(render(value)), "application/json")
}

