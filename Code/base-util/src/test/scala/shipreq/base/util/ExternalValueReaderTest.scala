package shipreq.base.util

import org.specs2.mutable.Specification
import ExternalValueReader._
import scalaz.{\/-, -\/}
import org.specs2.matcher.{DataTables, Matcher}
import shipreq.base.util.jodatime.{JodaTimeHelpers, JodaTimeValueRetrievers}
import org.specs2.time.NoTimeConversions

class ExternalValueReaderTest extends Specification with DataTables with NoTimeConversions {

  def fromMap(kvs: (String, () => String)*) = {
    val m = kvs.toMap
    new Retriever[String](k => m.get(k).map(ErrorOr apply _()))
  }

  val data = new StringBasedValueReader(fromMap("hi" -> (() => "hello"), "!" -> (() => ???), "n" -> (() => "123")))
  val dataNs = new StringBasedValueReader(fromMap("yay.hi" -> (() => "hello"), "yay.!" -> (() => ???)))

  def beAnError: Matcher[ErrorOr[_]] = beLike{ case -\/(_) => ok }
  def beSomeError: Matcher[Option[ErrorOr[_]]] = beLike{ case Some(-\/(_)) => ok }

  "Retrieval" >> {
    def retrievalTests(reader: StringBasedValueReader, scope: PropScope) = {
      import reader._
      implicit def _scope = scope

      "when value available" >> {
        "getOE"  in { getOE[String]("hi") ==== Some(ErrorOr("hello")) }
        "getO"   in { getO [String]("hi") ==== Some("hello") }
        "get"    in { get  [String]("hi") ==== ErrorOr("hello") }
        "need"   in { need [String]("hi") ==== "hello" }
        "tryGet.1" in { tryGet[String]("hi", "what") ==== ErrorOr("hello") }
        "tryGet.2" in { tryGet[String]("what", "hi") ==== ErrorOr("hello") }
      }

      "when value not specified" >> {
        "getOE"  in { getOE[String]("x") must beNone }
        "getO"   in { getO [String]("x") must beNone }
        "get"    in { get  [String]("x") must beAnError }
        "need"   in { need [String]("x") must throwA[ErrorAsThrowable] }
        "tryGet" in { tryGet[String]("x") must beAnError }
      }

      "when read throws an exception" >> {
        "getOE"  in { getOE[String]("!") must beSomeError }
        "getO"   in { getO [String]("!") must throwA[ErrorAsThrowable] }
        "get"    in { get  [String]("!") must beAnError }
        "need"   in { need [String]("!") must throwA[ErrorAsThrowable] }
        "tryGet" in { tryGet[String]("!", "hi") must beAnError }
      }
    }

    "With global scope" >> { retrievalTests(data, GlobalScope) }
    "With namespace"    >> { retrievalTests(dataNs, scopeByNS("yay")) }
  }

  "Validation & testing" >> {
    implicit def scope = GlobalScope
    import data._

    val numTest100 = valTest[Int](_ > 100, "Must be > 100")
    val numTest200 = valTest[Int](_ > 200, "Must be > 200")

    "validate" >> {
      "value doesn't exist" in { validate("?", need[Int])(numTest100) must throwA[ErrorAsThrowable] }
      "value parsing fails" in { validate("!", need[Int])(numTest100) must throwA[ErrorAsThrowable] }
      "value fails test"    in { validate("n", need[Int])(numTest200) must throwA[ErrorAsThrowable] }
      "value passes test"   in { validate("n", need[Int])(numTest100) must be_==(123) }
    }

    "validateO" >> {
      "value doesn't exist" in { validateO("?", getO[Int])(numTest100) must beNone }
      "value parsing fails" in { validateO("!", getO[Int])(numTest100) must throwA[ErrorAsThrowable] }
      "value fails test"    in { validateO("n", getO[Int])(numTest200) must throwA[ErrorAsThrowable] }
      "value passes test"   in { validateO("n", getO[Int])(numTest100) must beSome(be_==(123)) }
    }

    "test" >> {
      "value doesn't exist" in { test("?", get[Int])(numTest100) must beAnError }
      "value parsing fails" in { test("!", get[Int])(numTest100) must beAnError }
      "value fails test"    in { test("n", get[Int])(numTest200) must beAnError }
      "value passes test"   in { test("n", get[Int])(numTest100) ==== ErrorOr(123) }
    }

    "testO" >> {
      "value doesn't exist" in { testO("?", getOE[Int])(numTest100) must beNone }
      "value parsing fails" in { testO("!", getOE[Int])(numTest100) must beSomeError }
      "value fails test"    in { testO("n", getOE[Int])(numTest200) must beSomeError }
      "value passes test"   in { testO("n", getOE[Int])(numTest100) ==== Some(ErrorOr(123)) }
    }
  }

  "String-based retrievers" >> {
    implicit def scope = GlobalScope

    def pass[T](t: T): Option[ErrorOr[T]] = Some(ErrorOr(t))
    def fail[T]: Option[ErrorOr[T]] = Some(-\/(Error error "any error"))

    def test[T](r: StringBasedValueReader => Retriever[T])(s: String, exp: Option[ErrorOr[T]]) = {
      val a = new StringBasedValueReader(fromMap("X" -> (() => s)))
      val e: Matcher[Option[ErrorOr[T]]] = exp match {
        case Some(-\/(_)) => beLike{case Some(-\/(_)) => ok}
        case Some(\/-(v)) => be_==(Some(ErrorOr(v)))
        case None         => beNone
      }
      r(a).run("X") must e
    }

    def testJoda[T](r: JodaTimeValueRetrievers => Retriever[T])(s: String, exp: Option[ErrorOr[T]]) =
      test(a => r(JodaTimeValueRetrievers(a.retrieverS)))(s, exp)

    "Removes whitespace-trim and comments" ^ {
      "In"                 || "Exp"       |
      ""                   !! None        |
      "\t   "              !! None        |
      "\t\t300 # You know" !! pass("300") |
      " 300#"              !! pass("300") |
      "# TODO"             !! None        |> test(_.retrieverS)
    }

    "Int parsing" ^ {
      "In"      || "Exp"         |
      "012"     !! pass(12)      |
      "-321654" !! pass(-321654) |
      "???"     !! fail          |
      "23.1515" !! fail          |> test(_.retrieverI)
    }

    "Long parsing" ^ {
      "In"      || "Exp"          |
      "012"     !! pass(12L)      |
      "-321654" !! pass(-321654L) |
      "???"     !! fail           |
      "23.1515" !! fail           |> test(_.retrieverL)
    }

    "Boolean parsing" ^ {
      "In"    || "Exp"       |
      "t"     !! pass(true)  |
      "true"  !! pass(true)  |
      "TRUE"  !! pass(true)  |
      "1"     !! pass(true)  |
      "y"     !! pass(true)  |
      "yes"   !! pass(true)  |
      "on"    !! pass(true)  |
      "f"     !! pass(false) |
      "false" !! pass(false) |
      "FALSE" !! pass(false) |
      "0"     !! pass(false) |
      "n"     !! pass(false) |
      "no"    !! pass(false) |
      "off"   !! pass(false) |
      "???"   !! fail        |> test(_.retrieverB)
    }

    "JodaTime Period parsing" ^ {
      import JodaTimeHelpers._
      "In"        || "Exp"           |
      "1 second"  !! pass(1 sec)     |
      "1 minute"  !! pass(1 minutes) |
      "1 hour"    !! pass(1 hour)    |
      "1 day"     !! pass(1 days)    |
      "1 month"   !! pass(1 months)  |
      "1 week"    !! pass(1 weeks)   |
      "1 year"    !! pass(1 years)   |
      "3 ms"      !! pass(3 millis)  |
      "3 millis"  !! pass(3 millis)  |
      "3 sec"     !! pass(3 sec)     |
      "3 seconds" !! pass(3 sec)     |
      "3 min"     !! pass(3 minutes) |
      "3 minutes" !! pass(3 minutes) |
      "3 hr"      !! pass(3 hour)    |
      "3 hours"   !! pass(3 hour)    |
      "3 days"    !! pass(3 days)    |
      "3 months"  !! pass(3 months)  |
      "3 weeks"   !! pass(3 weeks)   |
      "3 years"   !! pass(3 years)   |
      "2min"      !! pass(2 minutes) |
      "2   MIN"   !! pass(2 minutes) |
      "2"         !! fail            |
      "min"       !! fail            |
      "2 min sec" !! fail            |
      "2 2"       !! fail            |
      "x sec"     !! fail            |
      "???"       !! fail            |> testJoda(_.retrieverPeriod)
    }

  }
}
