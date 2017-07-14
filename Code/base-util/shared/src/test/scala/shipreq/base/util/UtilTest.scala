package shipreq.base.util

import nyaya.gen.Gen
import nyaya.prop.Prop
import nyaya.test.PropTest._
import utest._

object UtilTest extends TestSuite {

  override def tests = TestSuite {

    'quickStringLookup {
      val x = "x"
      val p: Prop[Set[String]] =
        Prop.test("quickStringLookup", ss => {
          val f = Util.quickStringLookup(ss)
          (ss + "" + "123").toList
            .flatMap(s => s.drop(1) :: (s + x) :: (x + s + x) :: s :: Nil)
            .forall(s => {
              // println(s"${ss.contains(s)} / ${f(s)} -- [$s]")
              ss.contains(s) == f(s)
            })
        })
      Gen.string(0 to 8).set.mustSatisfy(p)
    }

  }
}
