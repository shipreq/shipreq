package shipreq.base.util.diff

import japgolly.microlibs.testutil.TestUtil._
import sourcecode.Line
import utest._

object DiffSourceTest extends TestSuite {

  private def assertSubtr(a: DiffSource[String, Any])(offset: Int, expected: String)(implicit l: Line): Unit =
    assertEq(a.offset -> a.value, offset -> expected)

  override def tests = Tests {
    "lines" - {

      "1" - {
        val str = "\n\n12345678\n\nabcdefgh\n\n"
        val s   = DiffSource.Str.lines(str)

        "full" - assertSubtr(s.value)(0, str)
        "0"    - assertSubtr(s.just(0).value)(0, "\n\n12345678\n\n")
        "1"    - assertSubtr(s.just(1).value)(12, "abcdefgh\n\n")
        "L0"   - assertEq(s(0).value, "12345678")
        "L1"   - assertEq(s(1).value, "abcdefgh")
      }

      "2" - {
        val str = "a\nb\nc\nd"
        val s   = DiffSource.Str.lines(str)

        "full" - assertSubtr(s.value)(0, str)
        "0"    - assertSubtr(s.slice(0, 1).value)(0, "a\n")
        "1"    - assertSubtr(s.slice(1, 2).value)(2, "b\n")
        "2"    - assertSubtr(s.slice(2, 3).value)(4, "c\n")
        "3"    - assertSubtr(s.slice(3, 4).value)(6, "d")
        "01"   - assertSubtr(s.slice(0, 2).value)(0, "a\nb\n")
        "12"   - assertSubtr(s.slice(1, 3).value)(2, "b\nc\n")
        "23"   - assertSubtr(s.slice(2, 4).value)(4, "c\nd")
        "012"  - assertSubtr(s.slice(0, 3).value)(0, "a\nb\nc\n")
        "123"  - assertSubtr(s.slice(1, 4).value)(2, "b\nc\nd")
        "z0"   - assertSubtr(s.slice(0, 0).value)(0, "")
        "z1"   - assertSubtr(s.slice(1, 1).value)(2, "")
        "z2"   - assertSubtr(s.slice(2, 2).value)(4, "")
        "z3"   - assertSubtr(s.slice(3, 3).value)(6, "")
        "z4"   - assertSubtr(s.slice(4, 4).value)(7, "")
      }

//      "3" - {
//        //                            012345 678901234 567890 123 456789
//        //                            |    |         |      |   |
//        val s = DiffSource.Str.lines("BADBC\nDACAABDD\nBCDDB\nDA\nDCACC")
//        assertEq(s.length, 5)
//
//        assertSubtr(s(0))(0, "BADBC")
//        assertSubtr(s(1))(6, "DACAABDD")
//        assertSubtr(s(2))(15, "BCDDB")
//        assertSubtr(s(3))(21, "DA")
//        assertSubtr(s(4))(24, "DCACC")
//
//        assertSubtr(s.just(0).value)(0, "BADBC\n")
//        assertSubtr(s.just(1).value)(5, "DACAABDD\n")
//        assertSubtr(s.just(2).value)(14, "BCDDB\n")
//        assertSubtr(s.just(3).value)(20, "DA\n")
//        assertSubtr(s.just(4).value)(23, "DCACC")
//      }

    }
  }
}
