package shipreq.base.util

import nyaya.gen.Gen
import nyaya.prop.Prop
import nyaya.test.PropTest._
import shipreq.base.util.MTrie.Ops
import utest._

object MTrieTest extends TestSuite {
  val  Trie = new MTrie.Types[String, Int]
  import Trie._

  implicit def autoPath(s: String) = NonEmptyVector force s.split('.').toVector

  def testRevert(expect: Trie, f: Trie => Trie): Unit = {
    val actual = f(expect)
    assert(actual == expect)
  }

  val removeProp =
    Prop.atom[(Trie, Iterable[Path])]("remove" , i => {
      val t1 = i._1
      val s1 = t1.flatIterator().size
      i._2.iterator.map { p =>
        val t2 = t1.remove(p)
        def s2 = t2.flatIterator().size
        def d = s2 - s1
        t2.lookup(p) match {
          case Some(_) => Some(s"Removal failed. Δ=$d")
          case None =>
          if (t1.lookup(p).isEmpty) {
            if (t1 == t2) None else Some(s"Element didn't exist yet remove altered the trie. Δ=$d")
          } else
            if (d == - 1) None else Some(s"Expected size to change by -1, not $d.")
        }
      }.find(_.isDefined).flatten
    })

  val genPath: Gen[Path] =
    Gen.numeric.string(1 to 3).vector(1 to 3).map(NonEmptyVector.force)

  val removeGen: Gen[(Trie, Iterable[Path])] =
    for {
      trieKeys <- genPath.vector
      remove1 <- genPath.vector
      remove2 <- Gen.subset(trieKeys)
    } yield {
      val t = trieKeys.foldLeft(empty)(_.put(_, 0))
      val r = remove1 ++ remove2
      (t, r)
    }

  override def tests = Tests {
    "remove" - {
      "l1_all" - testRevert(empty,               _.put("x", 6).remove("x"))
      "l1_sib" - testRevert(empty.put("a", 7),   _.put("x", 6).remove("x"))
      "l1_pr1" - testRevert(empty.put("x.y", 7), _.put("x", 6).remove("x"))

      "l3_all" - testRevert(empty,                 _.put("a.b.e", 6).remove("a.b.e"))
      "l3_sib" - testRevert(empty.put("a.b.c", 7), _.put("a.b.e", 6).remove("a.b.e"))
      "l3_ch2" - testRevert(empty.put("a.b", 7),   _.put("a.b.e", 6).remove("a.b.e"))
      "l3_ch1" - testRevert(empty.put("a", 7),     _.put("a.b.e", 6).remove("a.b.e"))
      "l3_pr1" - testRevert(empty.put("a.b.c", 7), _.put("a", 6).remove("a"))
      "l3_pr2" - testRevert(empty.put("a.b.c", 7), _.put("a.b", 6).remove("a.b"))
      "l3_pr3" - testRevert(empty.put("a.b.c.d", 7), _.put("a.b.c", 6).remove("a.b.c"))

      "prop" - removeProp.mustBeSatisfiedBy(removeGen)
    }
  }
}
