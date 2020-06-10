package shipreq.base.util

import utest._
import shipreq.base.util.univeq._
import ScalaExt._

object RelPosTest extends TestSuite {

  def toVec(s: String) = s.toCharArray.toVector
  val abcd = toVec("abcd")

  override def tests = Tests {

    "get" - {
      assert(RelPos.get(abcd, 'd') ==* None)
      assert(RelPos.get(abcd, 'c') ==* 'd'.some)
      assert(RelPos.get(abcd, 'b') ==* 'c'.some)
      assert(RelPos.get(abcd, 'a') ==* 'b'.some)
    }

    "set" - {
      def test(c: Char, pos: RelPos[Char], expectS: String = "abcd"): Unit = {
        val actual = RelPos.set(abcd, c, pos)
        val expect = toVec(expectS)
        assert(actual ==* expect)
      }

      test('d', None)
      test('c', None, "abdc")
      test('b', None, "acdb")
      test('a', None, "bcda")

      test('d', 'a'.some, "dabc")
      test('c', 'a'.some, "cabd")
      test('b', 'a'.some, "bacd")
      test('a', 'a'.some)

      test('d', 'b'.some, "adbc")
      test('c', 'b'.some, "acbd")
      test('b', 'b'.some)
      test('a', 'b'.some)

      test('d', 'c'.some, "abdc")
      test('c', 'c'.some)
      test('b', 'c'.some)
      test('a', 'c'.some, "bacd")

      test('d', 'd'.some)
      test('c', 'd'.some)
      test('b', 'd'.some, "acbd")
      test('a', 'd'.some, "bcad")
    }

    "set.get = id" - {
      for (i <- abcd) {
        val setGet = RelPos.set(abcd, i, RelPos.get(abcd, i))
        assert(setGet ==* abcd)
      }
    }

    // TODO RelPos needn't be an Option. Fix and uncomment the following prop test
//    "get(set(p)) = p" - {
//      for {i <- abcd; p <- abcd.map(_.some) :+ None} {
//        val getSet = RelPos.get(RelPos.set(abcd, i, p), i)
//        assert(getSet ==* p)
//      }
//    }
  }
}
