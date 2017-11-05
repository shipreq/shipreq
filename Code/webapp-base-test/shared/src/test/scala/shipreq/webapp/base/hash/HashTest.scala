package shipreq.webapp.base.hash

import boopickle._
import nyaya.gen.Gen
import nyaya.prop._
import nyaya.util.NyayaUtilAnyExt
import nyaya.test._
import nyaya.test.PropTestOps._
import utest._
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.base.RandomData
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.protocol.BinCodecMemberData._

object HashTest extends TestSuite {

  implicit val settings = DefaultSettings.propSettings
    .setSampleSize(80 `JVM|JS` 40).setGenSize(20)
//    .setDebug

  // ===================================================================================================================
  // Hash algorithms

  val nv = (0 to 10 by 2).toVector
  val ef = nv.map(_.toString)
  val of = nv.map(i => (i + 1).toString)
  val v1f = ef ++ of
  val v2f = of ++ ef
  val v3f = (0 to 11).toVector.map(_.toString)
  val v1r = v1f.reverse
  val v2r = v2f.reverse
  val v3r = v3f.reverse
  val vs  = Vector(v1f, v1r, v2f, v2r, v3f, v3r)

  def algorithmProp(expect: AlgorithmResults): Prop[Hash.Algorithm] = {
    type A = Hash.Algorithm

    def sameResultsOnJvmAndJs = Prop.equal[A]("Same Results on JVM & JS")(
      AlgorithmResults.calc,
      _ => expect)

    def unorderedHashIgnoresOrder = Prop.atom[A]("unorderedHash ignores order", a => {
      import a.hashString
      val h = a.hashUnordered[Vector, String]
      val hs = vs.map(h.hashFn).toSet
      if (hs.size == 1)
        None
      else
        Some(s"${hs.size} distinct hashes returned.")
    })

    def noCollisions[B](name: String, f: A => HashFn[B])(b1: B, b2: B, bn: B*): Prop[A] =
      Prop.atom[A](s"hashValuesDiffer: $name", a => {
        val bs = bn.toVector :+ b1 :+ b2
        val h  = f(a)
        var hs = Set.empty[Int]
        bs foreach (hs += h(_))
        val c = bs.size - hs.size
        if (c == 0)
          None
        else
          Some(s"$c collisions detected.")
      })

    def hashFnsVary = (
      noCollisions("int"          , _.hashInt                                )(3, 4)
    ∧ noCollisions("long"         , _.hashLong                               )(3L, 4L)
    ∧ noCollisions("string"       , _.hashString                             )("a", "aa", "b")
    ∧ noCollisions("pair"         , a ⇒ a.hashPair(a.hashInt, a.hashString)  )((4, "a"), (5, "a"), (4, "b"), (5, "b"))
    ∧ noCollisions("map"          , a ⇒ a.hashMap(a.hashInt, a.hashString)   )(Map(4 -> "f", 6 -> "s"), Map(6 -> "s"), Map(3 -> "f", 6 -> "s"))
    ∧ noCollisions("set"          , a ⇒ a.hashSet(a.hashString)              )(Set("a"), Set("b"), Set("a", "b"))
    ∧ noCollisions("list"         , a ⇒ a.hashList(a.hashString)             )(List("a"), List("b"), List("a", "b"), List("b", "a"))
    ∧ noCollisions("vector"       , a ⇒ a.hashVector(a.hashString)           )(Vector("a"), Vector("b"), Vector("a", "b"), Vector("b", "a"))
    ∧ noCollisions("hashUnordered", a ⇒ a.hashUnordered[List, Int](a.hashInt))(List(1), List(2), List(1, 2))
    ) rename "hashFnsVary"

    sameResultsOnJvmAndJs & unorderedHashIgnoresOrder & hashFnsVary
  }

  // Ensure that JVM & JS produce the same results
  def testAlgo(a: Hash.Algorithm, expect: AlgorithmResults): Unit =
    algorithmProp(expect).assert(a)

  val murmur3 = AlgorithmResults(1231,1237,-3506311,528568105,1842429670,607967924,438654265,-1390910323,586134407,2075563892,-1936667874,-1122530123)

  // ===================================================================================================================
  // Data hashing

  /*
  case class DataHashTest[A: HashFn : Pickler](a1: A, a2: A, a3: A) {
    val E = EvalOver(this)

    val h1 = Hash(a1)
    val h2 = Hash(a2)
    val h3 = Hash(a3)

    def consistent(h: Int, a: A) = {
      val bb = PickleImpl.intoBytes(a)
      val a2 = UnpickleImpl[A].fromBytes(bb)
      E.equal("consistent: hash(a) = hash(deser(ser(a)))", h, Hash(a2))
    }

    def allConsistent =
      consistent(h1, a1) ∧ consistent(h2, a2) ∧ consistent(h3, a3)

    def hashesDiffer =
      E.test("Hashes must differ", Set(h1, h2, h3).size >= 2)

    def main =
      allConsistent ∧ hashesDiffer
  }

  def dataHashTest[A] = Prop.eval[DataHashTest[A]](_.main)

  def testData[A: HashFn : Pickler](g: Gen[A]): Unit = {
    val t = for {a <- g; b <- g; c <- g} yield DataHashTest(a, b, c)
    dataHashTest[A] mustBeSatisfiedBy t
  }
  */

  // ===================================================================================================================
  override def tests = TestSuite {
    'algorithms {
      'murmur3 - testAlgo(MurmurHash3, murmur3)
    }
//    'data {
//      Should we redo this by HashScope? It seems to be pretty low value
//      //implicit val h = Hash.fn[Project](HashScheme.latest.hasher(HashScope.WholeProject, _))
//      'project - testData(RandomData.project)
//    }
  }
}
