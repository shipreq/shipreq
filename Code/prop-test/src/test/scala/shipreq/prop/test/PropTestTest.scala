package shipreq.prop.test

import scalaz.std.list._
import utest._
import shipreq.prop._
import TestUtil._

object PropTestTest extends TestSuite {

  val prop = Prop[List[Int]]("distinct ints", is => is.distinct == is)
  val intGen = Gen.chooseint(0,5).list.lim(10).map(Distinct.int.lift[List].run)

  override def tests = TestSuite {

    'distinct {
      prop mustBeSatisfiedBy intGen
    }

    'proof {
      val lock = new Object
      var is = List.empty[Option[Boolean]]
      val p = Prop[Option[Boolean]]("proof", i => lock.synchronized{is ::= i; true})
      p mustBeProvedBy Domain.boolean.option
      lock.synchronized{is = is.sortBy(_.toString)}
      assert(is == List(None, Some(false), Some(true)))
    }
  }
}
