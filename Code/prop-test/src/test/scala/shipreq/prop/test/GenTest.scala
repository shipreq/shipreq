package shipreq.webapp.base.prop

import scalaz.std.list._
import shipreq.webapp.base.TestUtil._
import shipreq.base.prop._
import utest._

object GenTest extends TestSuite {

  val prop = Prop[List[Int]]("distinct ints", is => is.distinct == is)
  val intGen = Gen.chooseint(0,5).list.lim(10).map(Distinct.int.lift[List].run)

  override def tests = TestSuite {

    'distinct {
      prop mustBeSatisfiedBy intGen
    }
  }
}
