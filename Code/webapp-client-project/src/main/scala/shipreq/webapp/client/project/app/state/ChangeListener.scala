package shipreq.webapp.client.project.app.state

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.{Listenable, OnUnmount}
import shipreq.webapp.base.data._
import shipreq.webapp.client.project.app.cfg.shared.SavedRowStore

object ChangeListener {

  class Updater[S](val h: Changes => S => S) extends AnyVal {
    def install[P, C <: Children, B <: OnUnmount, U <: UpdateSnapshot](global: P => Global): ScalaComponent.Config[P, C, S, B, U, U] =
      Listenable.listen[P, C, S, B, U, Changes](global, $ => changes =>
        $.modState(h(changes)))
  }

  @inline def update[S](h: Changes => S => S) = new Updater(h)

  def store[S, Id, Data](store: SavedRowStore[S, Id, Data, _])(ids: Changes => Set[Id], get: Project => Id => Option[Data]): Updater[S] =
    oneByOne(ids, get)((s, i) => store.remove(i)(s), (s, i, d) => store.set(i, d)(s))

  def oneByOne[S, Id, Data](ids: Changes => Set[Id], get: Project => Id => Option[Data])
                           (remove: (S, Id) => S, update: (S, Id, Data) => S): Updater[S] =
    new Updater(changes => {
      val g = get(changes.p2)
      ids(changes).foldLeft[S => S](identity)((q, id) =>
        g(id) match {
          case Some(v) => q.andThen(update(_, id, v))
          case None    => q.andThen(remove(_, id))
        }
      )
    })

  // ===================================================================================================================

  class Refresher(val refresh: Changes => Boolean) extends AnyVal {
    def install[P, C <: Children, S, B <: OnUnmount, U <: UpdateSnapshot](global: P => Global): ScalaComponent.Config[P, C, S, B, U, U] =
      Listenable.listen[P, C, S, B, U, Changes](global, $ => changes =>
        if (refresh(changes))
          $.forceUpdate
        else
          Callback.empty
      )
  }

  def refreshWhen(cond: Changes => Boolean): Refresher =
    new Refresher(cond)

  def refreshWhenChanged(changeSets: (Changes => Set[_])*): Refresher =
    new Refresher(c => changeSets.exists(f => f(c).nonEmpty))

  val refreshWhenFieldNamesChange: Refresher =
    refreshWhen(_.fieldNames)
}
