package shipreq.prop.test

import scalaz.EphemeralStream
import scalaz.effect.IO
import shipreq.prop.Prop
import PTest._

object Executor {
  type Data[A] = SampleSize => IO[EphemeralStream[A]]
}

import Executor.Data

trait Executor {
  def run[A](p: Prop[A], g: Data[A], S: Settings): RunState[A]
  def prove[A](p: Prop[A], d: Domain[A], S: Settings): RunState[A]
}

object SingleThreadedExecutor extends Executor {
  override def run[A](p: Prop[A], g: Data[A], S: Settings): RunState[A] = {
    val data = g(S.sampleSize).unsafePerformIO()
    var i = 0
    testN(p, data, () => {i+=1; i}, S)
  }

  override def prove[A](p: Prop[A], d: Domain[A], S: Settings): RunState[A] =
    proveN(p, d, 0, 1, _ + 1, S)
}