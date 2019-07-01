package shipreq.base.util

import japgolly.microlibs.testutil.TestUtil._
import utest._

object UrlTest extends TestSuite {
  import Url._

  val googleStr = "https://google.com"
  val google = Absolute.Base(googleStr)

  override def tests = Tests {

    'relative {
      'nullary {
        def test(u: Relative, noSlash: String): Unit = {
          assertEq(u.relativeUrlNoHeadSlash, noSlash)
          assertEq(u.relativeUrl, "/" + noSlash)
        }
        "/"     - test(Relative("/"), "")
        "/x"    - test(Relative("/x"), "x")
        "/x/y"  - test(Relative("/x/y"), "x/y")
        "/x/y/" - test(Relative("/x/y/"), "x/y/")
        'empty  - assertEq(Relative(""), Relative("/"))
        'head2  - assertEq(Relative("//"), Relative("/"))
        "x"     - assertEq(Relative("x"), Relative("/x"))
      }
      'unary {
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
      '/ {
        def test(a: String, b: String)(e: String): Unit = {
          val c = Url.Relative(b)
          assertEq(Url.Relative(a) / c.relativeUrl, Url.Relative(e))
          assertEq(Url.Relative(a) / c.relativeUrlNoHeadSlash, Url.Relative(e))
        }
        * - test("/", "/")("/")
        * - test("/a", "/")("/a")
        * - test("/", "/a")("/a")
        * - test("/a", "/b")("/a/b")
      }

      'isParentOf {
        def test(a: String, b: String, e: Boolean): Unit =
          assertEq(s"$a isParentOf $b", Relative(a).isParentOf(Relative(b)), e)
        * - test("/abc", "/abc", false)
        * - test("/abc", "/ab", false)
        * - test("/abc", "/abc/def", true)
        * - test("/abc", "/abcdef", false)
        * - test("/abc/", "/abc", false)
        * - test("/abc/", "/ab", false)
        * - test("/abc/", "/abc/def", true)
        * - test("/abc/", "/abcdef", false)
      }

      'isEqualToOrParentOf {
        def test(a: String, b: String, e: Boolean): Unit =
          assertEq(s"$a isEqualToOrParentOf $b", Relative(a).isEqualToOrParentOf(Relative(b)), e)
        * - test("/abc", "/abc", true)
        * - test("/abc", "/ab", false)
        * - test("/abc", "/abc/def", true)
        * - test("/abc", "/abcdef", false)
        * - test("/abc/", "/abc", true)
        * - test("/abc/", "/ab", false)
        * - test("/abc/", "/abc/def", true)
        * - test("/abc/", "/abcdef", false)
      }
    }

    'absoluteBase {
      'noSlash - assertEq(Absolute.Base(googleStr).value, googleStr)
      'slash - assertEq(Absolute.Base(googleStr + "/").value, googleStr)
    }

    'absolute {
      'nullary {
        def test(r: Relative, path: String): String = {
          val a = google / r
          assertEq(a.absoluteUrl, googleStr + path)
          a.absoluteUrl
        }
        "/"  - test(Relative("/"), "")
        "/x" - test(Relative("/x"), "/x")
      }
      'unary {
        def test(r: Relative, pathPrefix: String): String = {
          val a = google / r.thenParam[Int](_.toString)(123)
          assertEq(a.absoluteUrl, googleStr + pathPrefix + 123)
          a.absoluteUrl
        }
        "/"  - test(Relative("/"), "/")
        "/x" - test(Relative("/x"), "/x/")
      }
    }

  }
}
