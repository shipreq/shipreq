package shipreq.webapp.client.project.app.state

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.{Listenable, OnUnmount}
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.EventSeqSummary
import shipreq.webapp.client.project.app.cfg.shared.SavedRowStore

object ChangeListener {

  class Updater[S](val h: EventSeqSummary.WithProject => S => S) extends AnyVal {
    def install[P, C <: Children, B <: OnUnmount, U <: UpdateSnapshot](global: P => Global): ScalaComponent.Config[P, C, S, B, U, U] =
      Listenable.listen[P, C, S, B, U, EventSeqSummary.WithProject](global, $ => ess =>
        $.modState(h(ess)))
  }

  @inline def update[S](h: EventSeqSummary.WithProject => S => S) = new Updater(h)

  def store[S, Id, Data](store: SavedRowStore[S, Id, Data, _])(ids: EventSeqSummary.WithProject => Set[Id], get: Project => Id => Option[Data]): Updater[S] =
    oneByOne(ids, get)((s, i) => store.remove(i)(s), (s, i, d) => store.set(i, d)(s))

  def oneByOne[S, Id, Data](ids: EventSeqSummary.WithProject => Set[Id], get: Project => Id => Option[Data])
                           (remove: (S, Id) => S, update: (S, Id, Data) => S): Updater[S] =
    new Updater(ess => {
      val g = get(ess.project)
      ids(ess).foldLeft[S => S](identity)((q, id) =>
        g(id) match {
          case Some(v) => q.andThen(update(_, id, v))
          case None    => q.andThen(remove(_, id))
        }
      )
    })

  // ===================================================================================================================

  class Refresher(val refresh: EventSeqSummary.WithProject => Boolean) extends AnyVal {
    def install[P, C <: Children, S, B <: OnUnmount, U <: UpdateSnapshot](global: P => Global): ScalaComponent.Config[P, C, S, B, U, U] =
      Listenable.listen[P, C, S, B, U, EventSeqSummary.WithProject](global, $ => ess =>
        if (refresh(ess))
          $.forceUpdate
        else
          Callback.empty
      )
  }

  def refreshWhen(cond: EventSeqSummary.WithProject => Boolean): Refresher =
    new Refresher(cond)

  def refreshWhenChanged(changeSets: (EventSeqSummary.WithProject => Set[_])*): Refresher =
    new Refresher(c => changeSets.exists(f => f(c).nonEmpty))

  val refreshWhenFieldNamesChange: Refresher =
    refreshWhen(_.fieldNamesChanged)
}
