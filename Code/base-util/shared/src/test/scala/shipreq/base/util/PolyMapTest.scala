package shipreq.base.util

/*
import utest._

object PolyMapTest extends TestSuite {

  val M = PolyMap.Fix[Option[Int], Any]
  val noneKey = M.accessKey[None.type](None)[String].value
  val someKey = M.accessKey[Some[Int]](Some(666))[Int].value

  override def tests = Tests {
    var m = M.empty
    assert(noneKey.get(m) == None)
    assert(someKey.get(m) == None)

    m = noneKey.set(Some("blah"))(m)
    assert(noneKey.get(m) == Some("blah"))
    assert(someKey.get(m) == None)

    m = someKey.set(Some(111))(m)
    assert(noneKey.get(m) == Option("blah"))
    assert(someKey.get(m) == Option(111))

    m = noneKey.set(None)(m)
    assert(noneKey.get(m) == None)
    assert(someKey.get(m) == Option(111))

    // Check generic access too
    assert(m.get(None)      == None)
    assert(m.get(Some(666)) == Option(111))

    m = someKey.set(None)(m)
    assert(noneKey.get(m) == None)
    assert(someKey.get(m) == None)
    assert(m.isEmpty)
  }
}
*/