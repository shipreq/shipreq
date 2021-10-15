package shipreq.base.util

import japgolly.microlibs.testutil.TestUtil._
import sourcecode.Line
import utest._

object UrlTest extends TestSuite {
  import Url._

  val googleStr = "https://google.com"
  val google = Absolute.Base(googleStr)

  override def tests = Tests {

    "relative" - {
      "nullary" - {
        def test(u: Relative, noSlash: String): Unit = {
          assertEq(u.relativeUrlNoHeadSlash, noSlash)
          assertEq(u.relativeUrl, "/" + noSlash)
        }
        "/"     - test(Relative("/"), "")
        "/x"    - test(Relative("/x"), "x")
        "/x/y"  - test(Relative("/x/y"), "x/y")
        "/x/y/" - test(Relative("/x/y/"), "x/y/")
        "empty"  - assertEq(Relative(""), Relative("/"))
        "head2"  - assertEq(Relative("//"), Relative("/"))
        "x"     - assertEq(Relative("x"), Relative("/x"))
      }
      "unary" - {
        def test(p: Relative, prefix: String): String = {
          val u = p.thenParam[Int](_.toString)(123)
          assertEq(u.relativeUrl, s"$prefix/123")
          assert(!u.relativeUrlNoHeadSlash.startsWith("/"))
          u.relativeUrl
        }
        "/"      - test(Relative("/"), "")
        "/x"     - test(Relative("/x"), "/x")
        "/x/y"   - test(Relative("/x/y"), "/x/y")
        "/x/y/"  - test(Relative("/x/y/"), "/x/y")
        "/x/y//" - test(Relative("/x/y//"), "/x/y")
      }
      "/" - {
        def test(a: String, b: String)(e: String): Unit = {
          val c = Url.Relative(b)
          assertEq(Url.Relative(a) / c.relativeUrl, Url.Relative(e))
          assertEq(Url.Relative(a) / c.relativeUrlNoHeadSlash, Url.Relative(e))
        }
        "1" - test("/", "/")("/")
        "2" - test("/a", "/")("/a")
        "3" - test("/", "/a")("/a")
        "4" - test("/a", "/b")("/a/b")
      }

      "isParentOf" - {
        def test(a: String, b: String, e: Boolean): Unit =
          assertEq(s"$a isParentOf $b", Relative(a).isParentOf(Relative(b)), e)
        "1" - test("/abc", "/abc", false)
        "2" - test("/abc", "/ab", false)
        "3" - test("/abc", "/abc/def", true)
        "4" - test("/abc", "/abcdef", false)
        "5" - test("/abc/", "/abc", false)
        "6" - test("/abc/", "/ab", false)
        "7" - test("/abc/", "/abc/def", true)
        "8" - test("/abc/", "/abcdef", false)
      }

      "isEqualToOrParentOf" - {
        def test(a: String, b: String, e: Boolean): Unit =
          assertEq(s"$a isEqualToOrParentOf $b", Relative(a).isEqualToOrParentOf(Relative(b)), e)
        "1" - test("/abc", "/abc", true)
        "2" - test("/abc", "/ab", false)
        "3" - test("/abc", "/abc/def", true)
        "4" - test("/abc", "/abcdef", false)
        "5" - test("/abc/", "/abc", true)
        "6" - test("/abc/", "/ab", false)
        "7" - test("/abc/", "/abc/def", true)
        "8" - test("/abc/", "/abcdef", false)
      }
    }

    "absoluteBase" - {
      "noSlash" - assertEq(Absolute.Base(googleStr).value, googleStr)
      "slash" - assertEq(Absolute.Base(googleStr + "/").value, googleStr)
    }

    "absolute" - {
      "nullary" - {
        def test(r: Relative, path: String): String = {
          val a = google / r
          assertEq(a.absoluteUrl, googleStr + path)
          a.absoluteUrl
        }
        "/"  - test(Relative("/"), "")
        "/x" - test(Relative("/x"), "/x")
      }
      "unary" - {
        def test(r: Relative, pathPrefix: String): String = {
          val a = google / r.thenParam[Int](_.toString)(123)
          assertEq(a.absoluteUrl, googleStr + pathPrefix + 123)
          a.absoluteUrl
        }
        "/"  - test(Relative("/"), "/")
        "/x" - test(Relative("/x"), "/x/")
      }
      "relative" - {
        "1" - assertEq(Absolute("http://qwe.asd/qwe").relativeUrl.relativeUrl, "/qwe")
        "2" - assertEq(Absolute("http://qwe.asd:123/qwe").relativeUrl.relativeUrl, "/qwe")
        "3" - assertEq(Absolute("http://qwe.asd/qwe/zxc").relativeUrl.relativeUrl, "/qwe/zxc")
        "4" - assertEq(Absolute("http://qwe.asd/").relativeUrl.relativeUrl, "/")
        "5" - assertEq(Absolute("http://qwe.asd").relativeUrl.relativeUrl, "/")
      }
      "/" - {
        def test(abs: String, rel: String)(expect: String)(implicit l: Line): Unit =
          assertEq(Absolute(abs)./(Relative(rel)).absoluteUrl, expect)
        val abs = List("http://localhost", "https://localhost:1234")
        val rels = List("", "/x", "/q/w")
        for {
          a <- abs
          x <- rels
          y <- rels
        } {
          test(a + x, y)(a + y)
        }
      }
    }

  }
}
