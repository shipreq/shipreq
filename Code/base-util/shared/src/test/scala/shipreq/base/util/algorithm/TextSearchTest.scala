package shipreq.base.util.algorithm

import japgolly.microlibs.utils.Memo
import nyaya.prop._
import nyaya.test._
import nyaya.test.PropTest._
import utest._
import japgolly.microlibs.testutil.TestUtil._

abstract class TextSearchTest(search: Array[Char] => Array[Char] => Boolean) extends TestSuite {

  private case class Input(needle: String, haystack: String)

  private implicit def dontNormalise(s: String): Array[Char] = s.toCharArray

  private val algo = Memo((s: String) => search(s))

  private def prop = Prop.equal[Input]("search == String.contains")(
    i => algo(i.needle)(i.haystack),
    i => i.haystack contains i.needle)

  private def alphabet = Domain.ofValues('a', '2', 'ψ')
  // private def alphabet = Domain.ofValues(List(-2, -1, 0, 1, 2).map(i => (CharArrayCutoff + i).toChar): _*)

  private def words(start: Int) = alphabet.array(start to 4) map String.valueOf

  private def domain = (words(1) *** words(0)).map((Input.apply _).tupled)

  override def tests = Tests {
    println(s"Proving text search algorithm with ${domain.size} samples...")
    domain.mustProve(prop)(DefaultSettings.propSettings.setSingleThreaded)
  }
}
