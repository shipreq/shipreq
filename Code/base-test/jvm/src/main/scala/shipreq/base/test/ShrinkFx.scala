package shipreq.base.test

import shipreq.base.util.FxModule._
import shipreq.base.util._

object ShrinkFx {
  import Shrink.DefaultBreadthLimit

  def apply[A](a           : A)
              (shrinker    : Shrinker[A],
               size        : A => Int,
               validity    : A => Fx[Validity],
               breadthLimit: Int = DefaultBreadthLimit): Fx[A] = {
    Fx {
      // No need for a separate implementation now that Shrink.apply is tail-recursive.
      Shrink(a)(shrinker, size, validity(_).unsafeRun(), breadthLimit)
    }
  }
}
