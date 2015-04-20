package shipreq.webapp.base.text

import japgolly.nyaya._
import japgolly.nyaya.test._
import japgolly.nyaya.test.PropTest._
import scalaz.Memo
import scalaz.std.anyVal.booleanInstance
import scalaz.std.string.stringInstance
import utest._
import shipreq.webapp.base.test.BaseTestUtil._
import TextSearch._

object TextSearchTest extends TestSuite {

  case class Input(needle: String, haystack: String)

  implicit def dontNormalise(s: String): Normalised = new Normalised(s.toCharArray)

  val algo = Memo.mutableHashMapMemo(
    (s: String) => new BoyerMooreHorspool(s))

  def prop = Prop.equal[Input]("search == String.contains")(
    i => algo(i.needle).search(i.haystack),
    i => i.haystack contains i.needle)

  def alphabet = Domain.ofValues('a', '2', 'ψ')
//  def alphabet = Domain.ofValues(List(-2, -1, 0, 1, 2).map(i => (CharArrayCutoff + i).toChar): _*)

  def words = alphabet.pair.pair.map {
    case ((a, b), (c, d)) =>
      a.toString + b + c + d
  }

  def domain = words.pair.map((Input.apply _).tupled)

  override def tests = TestSuite {
    'algorithm {
      // println(s"Proving text search algorithm with ${domain.size} tests...")
      domain.mustProve(prop)(DefaultSettings.propSettings.setSingleThreaded)
    }
    'normalisation {
      assertEq(String valueOf defaultNormaliser("AB cd 3 EF").data, "ab cd 3 ef")
    }
  }
}
