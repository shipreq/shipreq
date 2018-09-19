package shipreq.base.util

import utest._

object ArticulateErrorTest extends TestSuite {

  val exception = ArticulateError.attempt {
    val i = new RuntimeException("Inner")
    val o = new RuntimeException("Outer", i)
    throw o
  }.swap.toOption.get

  override def tests = Tests {

    'tag {
      var a = ArticulateError("Stuff failed.")
      assert(!a.isDeterministic)
      a = a.tagDeterministic
      assert(a.isDeterministic)
    }

//    'show - println(exception.setErrorMsg("FFF").hint("blah").tagDeterministic.show)

//    'strackTrace - exception.setErrorMsg("FFF").printStackTrace()

  }
}
