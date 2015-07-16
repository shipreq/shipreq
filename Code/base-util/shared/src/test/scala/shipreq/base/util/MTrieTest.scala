package shipreq.base.util

import utest._
import MTrie.Ops

object MTrieTest extends TestSuite {
  val  Trie = new MTrie.Types[String, Int]
  type Trie = Trie.Trie
  val empty = Trie.empty

  implicit def autoPath(s: String) = NonEmptyVector force s.split('.').toVector

  def testRevert(expect: Trie, f: Trie => Trie): Unit = {
    val actual = f(expect)
    assert(actual == expect)
  }

  override def tests = TestSuite {
    'remove {
      'l1_all - testRevert(empty,               _.put("x", 6).remove("x"))
      'l1_sib - testRevert(empty.put("a", 7),   _.put("x", 6).remove("x"))
      'l1_pr1 - testRevert(empty.put("x.y", 7), _.put("x", 6).remove("x"))

      'l3_all - testRevert(empty,                 _.put("a.b.e", 6).remove("a.b.e"))
      'l3_sib - testRevert(empty.put("a.b.c", 7), _.put("a.b.e", 6).remove("a.b.e"))
      'l3_ch2 - testRevert(empty.put("a.b", 7),   _.put("a.b.e", 6).remove("a.b.e"))
      'l3_ch1 - testRevert(empty.put("a", 7),     _.put("a.b.e", 6).remove("a.b.e"))
      'l3_pr1 - testRevert(empty.put("a.b.c", 7), _.put("a", 6).remove("a"))
      'l3_pr2 - testRevert(empty.put("a.b.c", 7), _.put("a.b", 6).remove("a.b"))
      'l3_pr3 - testRevert(empty.put("a.b.c.d", 7), _.put("a.b.c", 6).remove("a.b.c"))
    }
  }
}
