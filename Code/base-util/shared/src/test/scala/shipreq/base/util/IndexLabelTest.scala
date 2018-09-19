package shipreq.base.util

import utest._
import IndexLabel._

object IndexLabelTest extends TestSuite {

  def testFn(ll: IndexLabel): (Int, String) => Unit =
    (i, s) => {
      val al = ll.label(i)
      val ap = ll.parse(s)
      assert(al == s)
      assert(ap == Option(i))
    }

  override def tests = Tests {

    'numeric0 {
      val test = testFn(NumericFrom0)
      " 0 ↔ 0"  - test( 0,  "0")
      " 1 ↔ 1"  - test( 1,  "1")
      "50 ↔ 50" - test(50, "50")
    }

    'numeric1 {
      val test = testFn(NumericFrom1)
      " 0 ↔ 1"  - test( 0,  "1")
      " 1 ↔ 2"  - test( 1,  "2")
      "50 ↔ 51" - test(50, "51")
    }

    'alpha {
      val test = testFn(Alpha)
      " 0 ↔ a"  - test( 0, "a" )
      " 1 ↔ b"  - test( 1, "b" )
      " 4 ↔ e"  - test( 4, "e" )
      "25 ↔ z"  - test(25, "z" )
      "26 ↔ aa" - test(26, "aa")
      "27 ↔ ab" - test(27, "ab")
      "51 ↔ az" - test(51, "az")
      "52 ↔ ba" - test(52, "ba")
    }

    'roman {
      val test = testFn(Roman)
      " 0 ↔ i"       - test( 0, "i"      )
      " 1 ↔ ii"      - test( 1, "ii"     )
      " 2 ↔ iii"     - test( 2, "iii"    )
      " 3 ↔ iv"      - test( 3, "iv"     )
      " 4 ↔ v"       - test( 4, "v"      )
      " 5 ↔ vi"      - test( 5, "vi"     )
      " 6 ↔ vii"     - test( 6, "vii"    )
      " 7 ↔ viii"    - test( 7, "viii"   )
      " 8 ↔ ix"      - test( 8, "ix"     )
      " 9 ↔ x"       - test( 9, "x"      )
      "10 ↔ xi"      - test(10, "xi"     )
      "13 ↔ xiv"     - test(13, "xiv"    )
      "18 ↔ xix"     - test(18, "xix"    )
      "19 ↔ xx"      - test(19, "xx"     )
      "37 ↔ xxxviii" - test(37, "xxxviii")
    }

  }
}
