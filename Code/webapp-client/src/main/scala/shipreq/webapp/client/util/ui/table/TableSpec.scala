package shipreq.webapp.client.util.ui.table

import japgolly.scalajs.react._, ScalazReact._
import scalaz.{Equal, Bind, Name}
import scalaz.effect.IO
import scalaz.syntax.bind._
import shipreq.webapp.client.protocol.FailureIO
import shipreq.webapp.client.util.ui.Implicits._
import shipreq.webapp.client.util.ui._
import RowStatus.Sync
import TableSpec._

final class TableSpecB[S, D, U, P, II, VV](val p2ii: P => II,
                                           val multiFieldRenderer: Option[D] => MultiFieldRenderer[S, U, P, II, VV],
                                           val savedUnsaved: SavedUnsavedL[S, D, P, II],
                                           val initialState: Seq[(D, P)] => S) {

  def saveNotNeededWhen(f: (U, P) => SaveNeed) =
    new B2(f)

  def saveNotNeededWhenE(f: P => U)(implicit U: Equal[U]) =
    saveNotNeededWhen((u,p) => if (U.equal(u, f(p))) SaveNotNeeded else SaveNeeded)

  def saveNotNeededWhenI(implicit ev: II =:= U, U: Equal[U]) =
    saveNotNeededWhenE(ev compose p2ii)

  def saveNotNeededWhenP(implicit ev: P =:= U, U: Equal[U]) =
    saveNotNeededWhenE(ev)

  class B2(saveNotNeeded: (U, P) => SaveNeed) {
    import TableSpecU._

    def asyncSave[Arb](update: (Arb, D, P, U, SuccessIO, FailureIO) => IO[Unit]) =
      new TableSpecU(TableSpecB.this, saveNotNeeded, asyncUpdateIO[Arb, S, D, U, P](update))

    def asyncSaveP[Arb](update: (Arb, P, U, SuccessIO, FailureIO) => IO[Unit]) =
      asyncSave[Arb]((x, d, p, u, s, f) => update(x, p, u, s, f))
  }
}

object TableSpecB {

  def default[D, G, P, II, VV](spec: RowSpec[SavedUnsaved[D, P, II], Option[D], G, P, II, VV]) = {
    val init = spec.initial _
    val initialState: Seq[(D, P)] => spec.S =
      xs => (xs.map{ case (d,p) => d -> SavedRow(Sync, p, init(p)) }.toMap, None)
    new TableSpecB[spec.S, D, G, P, II, VV](init, spec.forRow, SavedUnsavedL.default, initialState)
  }
}

// =====================================================================================================================

object TableSpec {

  type RowStatusS[S] = RowStatus => ReactS[S, Unit]

  type Retry[S] = Name[ReactST[IO, S, Unit]]

  sealed trait SaveNeed {
    def asOption[A](a: A): Option[A]
  }
  case object SaveNeeded extends SaveNeed {
    override def asOption[A](a: A) = Some(a)
  }
  case object SaveNotNeeded extends SaveNeed {
    override def asOption[A](a: A) = None
  }

  /**
   * Renders all n fields in a row of an n-field table (See SpecN for impls).
   */
  private[table] trait MultiFieldRenderer[S, U, P, II, VV] {
    def prepare[M[_] : Bind : Optional2](ig: InputGatewayE[M,S,II]): MultiFieldRenderer2[M, S, U, P, VV]
  }
  private[table] trait MultiFieldRenderer2[M[_], S, U, P, VV] {
    def savableU: S => Option[U]
    def render(s2mp: S => M[P], save: ReactST[IO, S, Unit]): ComponentStateFocus[S] => M[VV]
  }

  @inline private[table] final def rowRenderer[M[_] : Bind, S, V1, V2, SubId](
      renderFields: SubId => ComponentStateFocus[S] => M[(RowStatus, V1)],
      renderRow: (ComponentStateFocus[S], SubId, RowStatus, V1) => V2
      ): ComponentStateFocus[S] => SubId => M[V2] =
    T => id => renderFields(id)(T).map(rv => renderRow(T, id, rv._1, rv._2))

  def failureIO[S](T: ComponentStateFocus[S], rs: RowStatusS[S], retry: Retry[S]) = {
    val retryIO  = T runState retry.value
    val failureS = rs(RowStatus.Failed(retryIO))
    FailureIO(T runState failureS.liftIO)
  }
}

final case class SavedRowDP[D, P](status: RowStatus, d: D, p: P)

final case class SuccessIO(io: IO[Unit]) // TODO Either move and use in ClientProtocol too, or rename
object SuccessIO {
  def nop = SuccessIO(IO(()))
}
