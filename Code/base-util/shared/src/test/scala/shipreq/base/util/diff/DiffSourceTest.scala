package shipreq.base.util.diff

import japgolly.microlibs.testutil.TestUtil._
import sourcecode.Line
import utest._

object DiffSourceTest extends TestSuite {

  private def assertSubtr(a: DiffSource[String, Any])(offset: Int, expected: String)(implicit l: Line): Unit =
    assertEq(a.offset -> a.value, offset -> expected)

  override def tests = Tests {

    "split" - {

      "1" - {
        val str = "  12345678  abcdefgh  "
        val s   = DiffSource.Str.split(str, ' ')

        "len"  - assertEq(s.length, 5)
        "full" - assertSubtr(s.value)(0, str)
        "0"    - assertSubtr(s.just(0).value)(0, "  ")
        "1"    - assertSubtr(s.just(1).value)(2, "12345678")
        "2"    - assertSubtr(s.just(2).value)(10, "  ")
        "3"    - assertSubtr(s.just(3).value)(12, "abcdefgh")
        "4"    - assertSubtr(s.just(4).value)(20, "  ")
        "L0"   - assertEq(s(0).value, "  ")
        "L1"   - assertEq(s(1).value, "12345678")
        "L2"   - assertEq(s(2).value, "  ")
        "L3"   - assertEq(s(3).value, "abcdefgh")
        "L4"   - assertEq(s(4).value, "  ")
      }

      "2" - {
        val str = "a b c d"
        val s   = DiffSource.Str.split(str, ' ')

        "len"  - assertEq(s.length, 7)
        "full" - assertSubtr(s.value)(0, str)
        "0"    - assertSubtr(s.just(0).value)(0, "a")
        "1"    - assertSubtr(s.just(1).value)(1, " ")
        "2"    - assertSubtr(s.just(2).value)(2, "b")
        "3"    - assertSubtr(s.just(3).value)(3, " ")
        "4"    - assertSubtr(s.just(4).value)(4, "c")
        "5"    - assertSubtr(s.just(5).value)(5, " ")
        "6"    - assertSubtr(s.just(6).value)(6, "d")

        "ab"   - assertSubtr(s.slice(0, 3).value)(0, "a b")
        "bc"   - assertSubtr(s.slice(2, 5).value)(2, "b c")
        "cd"   - assertSubtr(s.slice(4, 7).value)(4, "c d")
        "abc"  - assertSubtr(s.slice(0, 5).value)(0, "a b c")
        "bcd"  - assertSubtr(s.slice(2, 7).value)(2, "b c d")

        "z0"   - assertSubtr(s.slice(0, 0).value)(0, "")
        "z1"   - assertSubtr(s.slice(1, 1).value)(1, "")
        "z2"   - assertSubtr(s.slice(2, 2).value)(2, "")
        "z3"   - assertSubtr(s.slice(3, 3).value)(3, "")
        "z4"   - assertSubtr(s.slice(4, 4).value)(4, "")
        "z5"   - assertSubtr(s.slice(5, 5).value)(5, "")
        "z6"   - assertSubtr(s.slice(6, 6).value)(6, "")
        "z7"   - assertSubtr(s.slice(7, 7).value)(7, "")
      }

    }
  }
}
