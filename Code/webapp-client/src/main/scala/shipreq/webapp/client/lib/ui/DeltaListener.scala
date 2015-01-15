package shipreq.webapp.client.lib.ui

import japgolly.scalajs.react.ComponentScopeM
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.extra.{Listenable, OnUnmount}
import scalaz.Scalaz.Id
import scalaz.effect.IO
import shipreq.webapp.base.delta._
import shipreq.webapp.client.ClientData
import shipreq.webapp.client.delta._

object DeltaListener {

  class Handler[S](val fs: List[LocalDeltaG => S => S]) {
    def compose(h: Handler[S]): Handler[S] =
      new Handler(h.fs ::: fs)

    def merge: LocalDeltaG => S => S =
      d => fs.foldLeft[S => S](identity)(_ compose _(d))

    def listener: LocalDelta => ReactS[S, Unit] = {
      val f = merge
      ds => ReactS mod ds.foldLeft[S => S](identity)(_ compose f(_))
    }
  }

  object Handler {
    def apply[S](f: LocalDeltaG => S => S): Handler[S] =
      new Handler(List(f))

    def optional[S](f: LocalDeltaG => Option[S => S]): Handler[S] =
      apply(f.andThen(_ getOrElse identity[S]))

    def part[S](p: Partition)(f: LocalDeltaP[p.type] => S => S): Handler[S] =
      optional(_ matchPartition p map f)
  }

  // -------------------------------------------------------------------------------------------------------------------

  def apply[P, S, B <: OnUnmount](cd: P => ClientData, handler: Handler[S]) =
    Listenable.installS[P, S, B, Id, LocalDelta](cd, handler.listener)

  class OneByOne[S, I, D](val remove: (S, I) => S,
                          val put: (S, I, D) => S) {

    def partialContramap[J, B](ji: J => Option[I], bd: B => Option[D]): OneByOne[S, J, B] =
      new OneByOne(
        (s, j)    => ji(j).fold(s)(i => remove(s, i)),
        (s, j, b) => ji(j).flatMap(i => bd(b).map(d => put(s, i, d))) getOrElse s)

    def handler(p: Partition.Aux[D, I]): Handler[S] =
      Handler.part[S](p)(d => s1 => {
        val s2 = (s1 /: d.del)((s, id)   => remove(s, id))
        val s3 = (s2 /: d.upd)((s, data) => put(s, p.di.id(data), data))
        s3
      })

    def partialHandler[J, B](p: Partition.Aux[B, J])(ji: J => Option[I], bd: B => Option[D]): Handler[S] =
      partialContramap(ji, bd).handler(p)
  }

  def store[S, I, D](store: SavedRowStore[S, I, D, _]): OneByOne[S, I, D] =
    new OneByOne[S, I, D](
      (s, i)    => store.remove(i)(s),
      (s, i, d) => store.set(i, d)(s))

  def refresh[P, S, B <: OnUnmount](cd: P => ClientData, refreshIO: ComponentScopeM[P, S, B] => IO[Unit])(p1: Partition, pn: Partition*) = {
    val ps = pn.toSet + p1
    Listenable.installIO[P, S, B, LocalDelta](cd, ($, ds) =>
      if (ds.exists(ps contains _.p))
        refreshIO($)
      else
        IO(())
    )
  }
}