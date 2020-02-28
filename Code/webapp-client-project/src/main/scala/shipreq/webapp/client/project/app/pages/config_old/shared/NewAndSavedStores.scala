package shipreq.webapp.client.project.app.pages.config_old.shared

import japgolly.univeq.UnivEq
import monocle.Lens
import monocle.function.Field1.first
import monocle.function.Field2.second
import monocle.std.tuple2._

object NewAndSavedStores {

  type SS[K, P, I] = (NewRowStore.SS[I], SavedRowStore.SS[K, P, I])

  def fields[P, I](fields: FieldSet[P, I]) = new {
    @inline def keyedBy[K: UnivEq] =
      NewAndSavedStores[SS[K, P, I], K, P, I](
        NewRowStore.of(fields).contramap(first),
        SavedRowStore.fields(fields).keyedBy[K].contramap(second))
  }
}

import NewAndSavedStores.SS

/**
 * @tparam P Persisted data. Data known to be saved.
 * @tparam I Input. A subset of P's fields in a form that matches the editor state.
 * @tparam K Key. Data ID.
 */
case class NewAndSavedStores[S, K: UnivEq, P, I](n: NewRowStore[S, I],
                                                 s: SavedRowStore[S, K, P, I]) {

  type Input = I
  type State = S

  def contramap[T](f: Lens[T, S]): NewAndSavedStores[T, K, P, I] =
    NewAndSavedStores(n contramap f, s contramap f)

  def initState(f: SavedRowStore[S, K, P, I] => SavedRowStore.SS[K, P, I]): SS[K, P, I] =
    (n.initState, f(s))
}
