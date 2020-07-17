package shipreq.webapp.server.logic

import nyaya.gen._
import nyaya.prop._
import nyaya.test.PropTest._
import shipreq.base.util.Valid
import utest._

object ObfuscatorTest extends TestSuite {

  val dictStr = scala.util.Random.shuffle("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz".toList).mkString
  val scheme = Obfuscator.long(dictStr)
  val lastCh = "(.)$".r

  class Test(id: Long) {
    val E = EvalOver(s"$id * $dictStr")

    val o = scheme.obfuscate(id)

    val result =
      E.equal("encode.decode = id", scheme.deobfuscate(o), \/-(id)) &
      E.equal("generated values are valid", scheme.validate(o), Valid) &
      E.test("Non-sequential", {
        val e1b = lastCh.replaceSomeIn(o.value, m => Some((m.group(1)(0) + 1).toChar.toString))
        val e2 = scheme.obfuscate(id + 1).value
        e1b !=* e2
      })
  }

  val prop = Prop.eval[Long](new Test(_).result)

  override def tests = Tests {
    Gen.long mustSatisfy prop
  }
}
