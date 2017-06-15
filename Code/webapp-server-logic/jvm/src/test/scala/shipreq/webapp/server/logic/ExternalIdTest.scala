package shipreq.webapp.server.logic

import nyaya.gen._
import nyaya.prop._
import nyaya.test.PropTest._
import utest._
import shipreq.base.test.BaseTestUtil._

object ExternalIdTest extends TestSuite {

  val dictStr = scala.util.Random.shuffle("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz".toList).mkString
  val scheme = ExternalId.scheme[Any, Long](identity, identity, dictStr)
  val extFmt = "^[a-zA-Z0-9]{4,12}$".r.pattern
  val lastCh = "(.)$".r

  class Test(id: Long) {
    val E = EvalOver(s"$id * $dictStr")

    val ext = scheme(id).value

    val result =
      E.equal("encode.decode = id", scheme.parseOption(ext), Some(id)) &
      E.test("Format", extFmt.matcher(ext).matches) &
      E.test("Non-sequential", {
        val e1b = lastCh.replaceSomeIn(ext, m => Some((m.group(1)(0) + 1).toChar.toString))
        val e2 = scheme(id + 1).value
        e1b !=* e2
      })
  }

  val prop = Prop.eval[Long](new Test(_).result)

  override def tests = TestSuite {
    Gen.long mustSatisfy prop
  }
}
