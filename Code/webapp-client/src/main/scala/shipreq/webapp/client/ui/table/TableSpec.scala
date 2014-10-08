package shipreq.webapp.client.ui.table

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.ReactVDom._
import monocle._
import scalaz.{Equal, Bind}
import scalaz.Scalaz.Id
import scalaz.effect.IO
import scalaz.std.option._
import scalaz.syntax.bind._
import shipreq.webapp.client.protocol.FailureIO
import shipreq.webapp.client.ui.Implicits._
import shipreq.webapp.client.ui._
import RowStatus.Sync
import TableSpec._

// TODO rename X

final class TableSpecB[S, D, U, P, II, VV](val p2ii: P => II,
                                           val multiFieldRenderer: Option[D] => MultiFieldRenderer[S, U, P, II, VV],
                                           val savedUnsaved: SavedUnsavedL[S, D, P, II],
                                           val initialState: Seq[(D, P)] => S) {

  def saveNotNeededWhen(f: (U, P) => Boolean) =
    new B2(f)

  def saveNotNeededWhenE(f: P => U)(implicit U: Equal[U]) =
    saveNotNeededWhen((u,p) => U.equal(u, f(p)))

  def saveNotNeededWhenI(implicit ev: II =:= U, U: Equal[U]) =
    saveNotNeededWhenE(ev compose p2ii)

  def saveNotNeededWhenP(implicit ev: P =:= U, U: Equal[U]) =
    saveNotNeededWhenE(ev)

  class B2(saveNotNeeded: (U, P) => Boolean) {

    def syncSave(saveIO: (Option[(D, P)], U) => IO[(D, P)]) =
      new TableSpec.SyncSave(TableSpecB.this, saveNotNeeded, saveIO)

    def syncSaveP(id: P => D, saveIO: (Option[P], U) => IO[P]) =
      syncSave((odp, o) => saveIO(odp.map(_._2), o).map(p => (id(p), p)))

    def asyncSave[X](saveIO: (X, Option[(D, P)], U, FailureIO) => IO[Unit]) =
      new TableSpec.AsyncSave(TableSpecB.this, saveNotNeeded, saveIO)

    def asyncSaveP[X](id: P => D, saveIO: (X, Option[P], U, FailureIO) => IO[Unit]) =
      asyncSave[X]((x, odp, o, f) => saveIO(x, odp.map(_._2), o, f))
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

abstract class TableSpec[X, S, D, U, P, II, VV](tsb: TableSpecB[S, D, U, P, II, VV], saveNotNeeded: (U, P) => Boolean) {
  import tsb.{p2ii, multiFieldRenderer}
  import tsb.savedUnsaved._

  @inline final private def ST = ReactS.Fix[S]

  final type DP = (D, P)
  final type CSF = ComponentStateFocus[S]

  @inline private final def initSavedRow(p: P): SavedRow[P, II] =
    SavedRow(Sync, p, p2ii(p))

  def initialState(d: Seq[P], id: P => D): S = tsb.initialState(d.map(x => id(x) -> x))
  def initialState(d: Map[D, P])         : S = tsb.initialState(d.toSeq)
  def initialState(d: Seq[(D, P)])       : S = tsb.initialState(d)

  private def inputGateway[M[_]](rs: RowStatus, i: II, iL: WeirdLens[M, S, S, II]): InputGateway[M, S, II] = rs match {
    case RowStatus.Sync | RowStatus.Failed =>
      EditAllowed(i, iL)
    case RowStatus.Locked =>
      ReadOnly(i)
  }

  protected def createIO: X => CSF => (S, U) => IO[S]

  private val unsavedIG: InputGatewayS[Option, S, II] = {
    val getI: S => Option[II] =
      unsavedL.get(_).map(_.ii)
    val setI: (S, II) => Option[S] =
      (s,i) => unsavedL.get(s).map(_ => unsavedL.set(s, Some(UnsavedRow(Sync, i))))
    val iL = WeirdLens[Option, S, S, II](getI, setI)
    unsavedL.get(_).map(u => inputGateway(u.status, u.ii, iL))
  }

  private val unsavedS2OP: S => Option[P] = _ => None

  private def unsavedRenderAttr(saveIO: CSF => (S, U) => IO[S]) =
    (T: CSF) =>
      for {
        rs <- unsavedStatusL.getOption(T.state)
        r = multiFieldRenderer(None).render(unsavedIG, unsavedS2OP, saveIO(T))
        v <- r(T)
      } yield (rs, v)

  def unsavedInitS(empty: II) =
    ReactS.mod(unsavedL.modifyF(_ orElse Some(UnsavedRow(Sync, empty))))

  val unsavedRemoveF =
    unsavedL setF None

  val unsavedRemoveS =
    ReactS.mod(unsavedRemoveF)

  def unsavedToSavedF(dp: DP): S => S =
    unsavedToSavedF(dp._1, dp._2)

  def unsavedToSavedF(id: D, p: P): S => S =
    savedSetF(id, p) compose unsavedRemoveF

  def unsavedRow[V2](renderRow: (CSF, RowStatus, VV) => V2)(implicit x: X) = {
    val rr = rowRenderer[Option, S, VV, V2, Unit](
      _ => unsavedRenderAttr(createIO(x))(_),
      (T, _, r, v) => renderRow(T, r, v))
    (T: CSF) => rr(T)(())
  }

  protected def updateIO: (CSF, X) => (S, DP, U) => IO[S]

  private def savedIG(id: D): InputGatewayS[Id, S, II] = {
    val rL = rowL(id)
    val iL = WeirdLens from (rL composeLens savedIL)
    s => {
      val v = rL.get(s)
      inputGateway(v.status, v.ii, iL)
    }
  }

  private def savedRenderAttr(x: X)(id: D) = {
    val ig = savedIG(id)
    (T: CSF) => {
      val saveIO = updateIfNeeded[S, U, DP, DP](
        s => (id, rowP(id)(s)),
        (dp, u) => if (saveNotNeeded(u, dp._2)) None else Some(dp)
      )(updateIO(T, x))
      val rs = rowStatus(id).get(T.state)
      val r = multiFieldRenderer(Some(id)).render[Id](ig, rowP(id), saveIO)
      (rs, r(T))
    }
  }

  def savedRemoveF(id: D) =
    savedL.modifyF(m => m - id)

  def savedRemoveS(id: D) =
    ReactS.mod(savedRemoveF(id))

  def savedDeleteIO_(f: D => IO[Unit]): D => ReactST[IO, S, Unit] =
    id => ReactS.retM(f(id)) >> savedRemoveS(id)

  def savedRevertS(id: D) =
    ST.mod(s => rowIL(id).set(s, p2ii(rowP(id)(s))))

  def updateSavedIO_(saveIO: DP => IO[DP]): D => ReactST[IO, S, Unit] = id =>
    ST.gets(rowDP(id)).liftIO
      .flatMap(px1 => ST.retM(saveIO(px1)))
      .flatMap(savedSetS)

  def savedSetF(dp: DP): S => S =
    savedSetF(dp._1, dp._2)

  def savedSetF(id: D, p: P): S => S =
    savedL.modifyF(_ + (id -> initSavedRow(p)))

  def savedSetS(dp: DP) =
    ST.mod(savedSetF(dp))

  def savedRowP[V2](renderRow: (CSF, D, RowStatus, P, VV) => V2)(implicit x: X) =
    savedRow((t, d, r, v) => renderRow(t, d, r, rowP(d)(t.state), v))

  def savedRow[V2](renderRow: (CSF, D, RowStatus, VV) => V2)(implicit x: X) =
    rowRenderer[Id, S, VV, V2, D](savedRenderAttr(x), renderRow)

  final type SavedPs = Stream[(RowStatus, D, P)]

  def savedRows(T: CSF, r: ComponentStateFocus[S] => D => Tag)(f: SavedPs => SavedPs) = {
    val rr = r(T)
    f(getSaved(T)).map(x => rr(x._2)).toJsArray
  }

  def getSaved(T: CSF): SavedPs =
    savedL.get(T.state).toStream.map{ case (d,SavedRow(r,p,_)) => (r,d,p) }
}

// =====================================================================================================================

object TableSpec {

  implicit object NoX

  final class SyncSave[S, D, U, P, II, VV](
      tsb:           TableSpecB[S, D, U, P, II, VV],
      saveNotNeeded: (U, P) => Boolean,
      saveIO:        (Option[(D, P)], U) => IO[(D, P)])
      extends TableSpec[NoX.type, S, D, U, P, II, VV](tsb, saveNotNeeded) {

    override protected def createIO = _ => _ => (s, u) =>
      saveIO(None, u)
        .map(unsavedToSavedF(_)(s))

    override protected def updateIO = (_, _) => (s, dp, u) =>
      saveIO(Some(dp), u)
        .map(savedSetF(_)(s))
  }

  // ===================================================================================================================

  final class AsyncSave[X, S, D, U, P, II, VV](
      tsb:           TableSpecB[S, D, U, P, II, VV],
      saveNotNeeded: (U, P) => Boolean,
      saveIO:        (X, Option[(D, P)], U, FailureIO) => IO[Unit])
      extends TableSpec[X, S, D, U, P, II, VV](tsb, saveNotNeeded) {

    import tsb.savedUnsaved._

    override protected def createIO = x => T => (s, u) =>
      saveIO(x, None, u, failureIO(T, None))
        .map(_ => lockRow(None)(s))

    override protected def updateIO = (T, x) => (s, dp, u) => {
      val row = Some(dp._1)
      saveIO(x, Some(dp), u, failureIO(T, row))
        .map(_ => lockRow(row)(s))
    }

    private def setStatus(status: RowStatus): Option[D] => S => S = {
      case None    => unsavedStatusL.setF(status)
      case Some(d) => rowStatus(d).setF(status)
    }

    val lockRow = setStatus(RowStatus.Locked)

    def failureS(row: Option[D]) =
      ReactS.mod(setStatus(RowStatus.Failed)(row))

    def failureIO(T: CSF, row: Option[D]) =
      FailureIO(T.runState(failureS(row)))
  }

  // ===================================================================================================================

  /**
   * Renders all n fields in a row of an n-field table (See SpecN for impls).
   */
  private[table] trait MultiFieldRenderer[S, U, P, II, VV] {
    // TODO save here should include ComponentStateFocus[S]. Save those λs in renderAttrForUnsaved etc
    def render[M[_] : Bind : Optional2](
      ig: InputGatewayS[M, S, II], s2mp: S => M[P], save: (S, U) => IO[S]): ComponentStateFocus[S] => M[VV]
  }

  @inline private[TableSpec] final def rowRenderer[M[_] : Bind, S, V1, V2, SubId](
      renderFields: SubId => ComponentStateFocus[S] => M[(RowStatus, V1)],
      renderRow: (ComponentStateFocus[S], SubId, RowStatus, V1) => V2
      ): ComponentStateFocus[S] => SubId => M[V2] =
    T => id => renderFields(id)(T).map(rv => renderRow(T, id, rv._1, rv._2))

  @inline private[TableSpec] final def updateIfNeeded[S, U, L, L2](
      getLast: S => L, needSave: (L, U) => Option[L2])(f: (S, L2, U) => IO[S]): (S, U) => IO[S] =
    (s, u) => {
      needSave(getLast(s), u) match {
        case Some(l2) => f(s, l2, u)
        case None     => IO(s)
      }
    }
}
