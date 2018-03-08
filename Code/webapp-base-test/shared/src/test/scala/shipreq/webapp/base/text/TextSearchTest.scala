package shipreq.webapp.base.text

import nyaya.prop._
import nyaya.test._
import nyaya.test.PropTest._
import utest._
import shipreq.base.util.Memo
import shipreq.webapp.base.test.WebappTestUtil._
import TextSearch._

object TextSearchTest extends TestSuite {

  case class Input(needle: String, haystack: String)

  implicit def dontNormalise(s: String): Normalised = new Normalised(s.toCharArray)

  val algo = Memo((s: String) => new BoyerMooreHorspool(s))

  def prop = Prop.equal[Input]("search == String.contains")(
    i => algo(i.needle).search(i.haystack),
    i => i.haystack contains i.needle)

  def alphabet = Domain.ofValues('a', '2', 'ψ')
//  def alphabet = Domain.ofValues(List(-2, -1, 0, 1, 2).map(i => (CharArrayCutoff + i).toChar): _*)

  def words(start: Int) = alphabet.array(start to 4) map String.valueOf

  def domain = (words(1) *** words(0)).map((Input.apply _).tupled)

  override def tests = TestSuite {
    'algorithm {
      println(s"Proving text search algorithm with ${domain.size} samples...")
      domain.mustProve(prop)(DefaultSettings.propSettings.setSingleThreaded)
    }
    'normalisation {
      assertEq(String valueOf Normaliser.ignoreCaseSingleSpaces("AB cd 3 EF").data, "ab cd 3 ef")
    }
  }
}
