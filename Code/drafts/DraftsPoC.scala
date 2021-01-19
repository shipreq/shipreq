package shipreq.base.test.drafts

import sourcecode.Line
import shipreq.base.test.BaseTestUtil._
import utest.{assert => _, _}

object DraftsPoC extends TestSuite {

  // Drafts are saved (per user) locally and remotely
  // start, edit, commit, abort, recv, send, resume

  class Clients2 {
    val n = new Network
    val s = new Server(n)
    val a = new Client(n, "a")
    val b = new Client(n, "b")
  }

  class Synced2(value: String)(implicit l: Line) extends Clients2 {
    s.connect(a, b)
    a.startEditing()
    a.append(value)
    a --> s
    n.flushAll()
    assertEq("Synced2.a", a.editValue(), Some(value))
    assertEq("Synced2.b", b.editValue(), Some(value))
  }

  override def tests = Tests {

    "fromEmpty" - {

      "sync" - {
        val x = new Synced2("this is my draft")
        import x._

        val c = new Client(n, "c")
        s.connect(c)
        assertEq(c.editValue(), Some("this is my draft"))

        b.replaceFirst("t", "T")
        c.replaceFirst("my", "our")
        assertEq(b.editValue(), Some("This is my draft"))
        assertEq(c.editValue(), Some("this is our draft"))
        b --> s
        c --> s
        n.flushAll()

        assertEq(a.editValue(), Some("This is our draft"))
        assertEq(b.editValue(), Some("This is our draft"))
        assertEq(c.editValue(), Some("This is our draft"))
      }

      "abort" - {
        val x = new Synced2("hey there")
        import x._

        a.abort()
        assertEq(a.editValue(), None)

        b.append("\ncool")
        assertEq(b.editValue(), Some("hey there\ncool"))

        a --> s
        b --> s
        n.flushAll()
        assertEq(a.editValue(), Some("\ncool"))
        assertEq(b.editValue(), Some("\ncool"))

        b.abort()
        a --> s
        b --> s
        n.flushAll()
        assertEq(a.editValue(), None)
        assertEq(b.editValue(), None)
      }
    }

    "fromOrd" - {
      "separateEditStreams" - {
        val x = new Clients2
        import x._

        s.addEvent(Event(123, "swimming"))
        s.connect(a, b)
        assertEq(a.editValue(), None)
        assertEq(b.editValue(), None)

        a.startEditing()
        assertEq(a.editValue(), Some("swimming"))
        a.replaceFirst("swimm", "runn")
        assertEq(a.editValue(), Some("running"))

        b.startEditing()
        assertEq(b.editValue(), Some("swimming"))
        b.append("?")
        assertEq(b.editValue(), Some("swimming?"))

        a --> s
        b --> s
        n.flushAll()
        assertEq(a.editValue(), Some("running?"))
        assertEq(b.editValue(), Some("running?"))
      }
    }

  }
}
