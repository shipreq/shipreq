package shipreq.base.test

import nyaya.gen.Gen

/**
 * Mutable counters that incrementally larger values on each call.
 *
 * NOT thread-safe.
 */
object IncCounter {

  def int(prev: Int): () => Int = {
    var i = prev
    () => {
      i += 1
      i
    }
  }

  def genInt(prev: Int): Gen[Int] = {
    val f = int(prev)
    Gen(_ => f())
  }

}
