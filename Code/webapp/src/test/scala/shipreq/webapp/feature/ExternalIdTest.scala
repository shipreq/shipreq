package shipreq.webapp.feature

import org.scalacheck.Prop._
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalatest.Matchers
import ExternalId.UseCase._
import shipreq.webapp.lib.Types._

class ExternalIdTest extends FunSuite with Checkers with Matchers {

  implicit override val generatorDrivenConfig = PropertyCheckConfig(minSuccessful = 10000)

  implicit def autoTagLongs(x: Long) = x.tag[IsUseCaseIdentId]

  // println((100 to 150).map(toExternal(_)).mkString("\n"))
  // println(toExternal(Long.MaxValue))
  // println(toExternal(Long.MinValue))
  // println(toExternal(-1))

  test("Conversion back and fro") {
    check {
      id: Long =>
        val ext = toExternal(id)
        val i2 = parseO(ext).get
        (i2 == id) :| s"$id --> $ext --> $i2"
    }
  }

  test("External string format") {
    check {
      id: Long =>
        toExternal(id) should fullyMatch regex "^[a-zA-Z0-9]{4,12}$"
        true
    }
  }

  test("External strings should not be sequential") {
    val lastCh = "(.)$".r
    check {
      id: Long =>
        val a = toExternal(id)
        val b = toExternal(id + 1)
        val a2 = lastCh.replaceSomeIn(a, m => Some((m.group(1)(0) + 1).toChar.toString))
        (a2 != b) :| s"$a + 1 matches $b"
    }
  }
}
