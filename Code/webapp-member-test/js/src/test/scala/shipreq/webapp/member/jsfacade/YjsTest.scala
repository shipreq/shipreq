package shipreq.webapp.member.jsfacade

import shipreq.base.test.BaseTestUtil._
import utest._

object YjsTest extends TestSuite {
  import Yjs._

  private def merge(src: Doc, tgt: Doc): Unit = {
    val u = encodeStateAsUpdate(src)
    applyUpdate(tgt, u)
  }

//  private def merge(prevSrc: Doc, src: Doc, tgt: Doc): Unit = {
//    val u = encodeStateAsUpdate(src, encodeStateVector(prevSrc))
//    applyUpdate(tgt, u)
//  }

  private def init(text: String, clientId: Int = 0): Doc = {
    val d = new Doc()
    d.clientID = clientId
    d.getText().insert(0, text)
    d
  }

  override def tests = Tests {

    "sanityCheck" - {
      val doc1 = new Doc()
      val doc2 = new Doc()
      val t1 = doc1.getText()
      val t2 = doc2.getText()
      def values() = (t1.toText(), t2.toText())

      // Init from committed value
      val orig = "hello there"
      val init1 = init(orig)
      val init2 = init(orig)
      merge(init1, doc1)
      merge(init2, doc2)
      assertEq(values(), (orig, orig))

      // Separate edits
      t1.delete(6, 5)
      t1.insert(t1.length, "world")
      t2.delete(0, 5)
      t2.insert(0, "Hey")
      assertEq(values(), ("hello world", "Hey there"))

      // Merge
      val both = "Hey world"
      val u1 = encodeStateAsUpdate(doc1, encodeStateVector(init1))
      val u2 = encodeStateAsUpdate(doc2, encodeStateVector(init2))
      applyUpdate(doc1, u2)
      applyUpdate(doc2, u1)
      assertEq(values(), (both, both))

      (u1.length, u2.length)
    }

  }
}
