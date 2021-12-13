package shipreq.base.test

import java.util.concurrent.atomic._
import nyaya.gen.Gen

object Incrementor {

  object int {
    def apply(prev: Int): () => Int = {
      val i = new AtomicInteger(prev)
      () => i.incrementAndGet()
    }

    def apply[A](prev: Int, f: Int => A): () => A = {
      val i = new AtomicInteger(prev)
      () => f(i.incrementAndGet())
    }

    def gen(prev: Int): Gen[Int] = {
      val f = apply(prev)
      Gen(_ => f())
    }
  }

  object long {
    def apply(prev: Long): () => Long = {
      val i = new AtomicLong(prev)
      () => i.incrementAndGet()
    }

    def apply[A](prev: Long, f: Long => A): () => A = {
      val i = new AtomicLong(prev)
      () => f(i.incrementAndGet())
    }

    def gen(prev: Long): Gen[Long] = {
      val f = apply(prev)
      Gen(_ => f())
    }
  }

}
