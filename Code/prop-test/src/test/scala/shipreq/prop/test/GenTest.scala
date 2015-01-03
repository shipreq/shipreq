package shipreq.prop.test

import scalaz.std.AllInstances._
import utest._
import shipreq.prop._
import shipreq.prop.test._
import shipreq.prop.test.PropTest._

object GenTest extends TestSuite {

  type I = List[Int]

  val shuffleProp =
    Prop.equal[(I, I), I]("shuffle.sorted = sorted", _._1.sorted, _._2.sorted)

  val shuffleGen =
    for {
      before <- Gen.int.list
      after  <- Gen.shuffle(before)
    } yield (before, after)

  override def tests = TestSuite {
    'shuffle - shuffleGen.mustSatisfy(shuffleProp)
  }
}
