import scalaz.Scalaz.Id
import scalaz.{State, StateT}
import scalaz.effect.IO
import monocle._
import japgolly.scalajs.react._

object Lib {
  type SSetter[S, A] = Setter[S, S, _, A]

  implicit def autoLiftStateIntoIO[S, A](s: StateT[Id, S, A]): StateT[IO, S, A] = s.lift[IO]

  implicit final class ComponentScope_SS_Ext3[S](val u: ComponentScope_SS[S]) extends AnyVal {

    //    @inline def setStateL [V](l: Setter[S, S, _, V])(v: V)                    = u.modState((s: S) => l.set(s, v))
    //    @inline def setStateLC[V](l: Setter[S, S, _, V])(v: V)(callback: => Unit) = u.modState((s: S) => l.set(s, v), callback)
    @inline def setStateL[V](l: Setter[S, S, _, V])(v: V)                    = u.modState(l.set(_, v))
    @inline def setStateL[V](l: Setter[S, S, _, V], callback: => Unit)(v: V) = u.modState(l.set(_, v), callback)

    // Using StateT[Id] instead of State so that Intellij doesn't paint the entire screen red
    @inline def runState(m: StateT[Id, S, _])                    = u.modState(m(_)._1)
    @inline def runState(m: StateT[Id, S, _], callback: => Unit) = u.modState(m(_)._1, callback)
    @inline def runStateC(m: StateT[Id, S, _])(callback: () => Unit) = runState(m, callback())
  }

  def textChangeRecv(f: String => Unit): InputEvent => Unit = e => f(e.target.value)
  def textChangeRecvL[State](t: ComponentScope_SS[State], l: Setter[State, State, _, String]) =
    textChangeRecv(t setStateL l)
  def textChangeRecvIO(f: String => IO[Unit]): InputEvent => IO[Unit] =
    e => f(e.target.value)

  def SimpleLens2[T] = new {
    def apply[A](f: T => A) = new {
      def apply(g: (T, A) => T) = SimpleLens[T, A](f, g)
    }
  }

  //  implicit class MonOptionalExt[S, T, A, B](val o: Optional[S, T, A, B]) extends AnyVal {
  //    def modifyOptionF(f: A => B)(from: S) = o.modifyOption(from, f)
  //    def setOptionF(newValue: B)(from: S) = o.setOption(from, newValue)
  //  }
  implicit class MonSetterExt[S, T, A, B](val o: Setter[S, T, A, B]) extends AnyVal {
    final def setF(newValue: B) = (from: S) => o.set(from, newValue)
    final def modifyF(f: A => B) = (from: S) => o.modify(from, f)
  }

  class StateHelper[S] {
    @inline final def apply[A](f: S => (S, A))        = State.apply(f)
    @inline final def constantState[A](a: A, s: => S) = State.constantState(a, s)
    @inline final def state[A](a: A)                  = State.state(a)
    @inline final def init                            = State.init[S]
    @inline final def get                             = State.get[S]
    @inline final def gets[A](f: S => A)              = State.gets(f)
    @inline final def put(s: S)                       = State.put(s)
    @inline final def modify(f: S => S)               = State.modify(f)
  }

}
