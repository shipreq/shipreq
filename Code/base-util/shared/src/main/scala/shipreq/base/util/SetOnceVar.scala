package shipreq.base.util

final class SetOnceVar[A] {
  private var instance = Option.empty[A]

  def getOrSet(value: => A): A =
    synchronized {
      if (instance.isDefined)
        instance.get
      else {
        val a = value
        instance = Some(a)
        a
      }
    }
}

object SetOnceVar {
  def apply[A]: SetOnceVar[A] =
    new SetOnceVar
}