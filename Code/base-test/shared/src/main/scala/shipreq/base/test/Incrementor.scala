package shipreq.base.test

import java.util.concurrent.atomic._
import nyaya.gen.Gen

object Incrementor {

  object int {
    def apply(prev: Int): () => Int = {
      var i = new AtomicInteger(prev)
      () => i.incrementAndGet()
    }

    def gen(prev: Int): Gen[Int] = {
      val f = apply(prev)
      Gen(_ => f())
    }
  }

  object long {
    def apply(prev: Long): () => Long = {
      var i = new AtomicLong(prev)
      () => i.incrementAndGet()
    }

    def gen(prev: Long): Gen[Long] = {
      val f = apply(prev)
      Gen(_ => f())
    }
  }

}
