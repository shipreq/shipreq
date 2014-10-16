package shipreq.webapp.client.ui.table

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.ReactVDom._
import scalaz.{Equal, Bind, Name, Need, Value}
import scalaz.Scalaz.Id
import scalaz.effect.IO
import scalaz.std.option._
import scalaz.syntax.bind._
import shipreq.webapp.client.protocol.FailureIO
import shipreq.webapp.client.ui.Implicits._
import shipreq.webapp.client.ui._
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

    def syncSave(saveIO: (Option[(D, P)], U) => IO[(D, P)]) =
      new TableSpec.SyncSave(TableSpecB.this, saveNotNeeded, saveIO)

    def syncSaveP(id: P => D, saveIO: (Option[P], U) => IO[P]) =
      syncSave((odp, o) => saveIO(odp.map(_._2), o).map(p => (id(p), p)))

    def asyncSave[X](saveIO: (X, Option[(D, P)], U, SuccessIO, FailureIO) => IO[Unit]) =
      new TableSpec.AsyncSave(TableSpecB.this, saveNotNeeded, saveIO)

    def asyncSaveP[X](id: P => D, saveIO: (X, Option[P], U, SuccessIO, FailureIO) => IO[Unit]) =
      asyncSave[X]((x, odp, o, s, f) => saveIO(x, odp.map(_._2), o, s, f))
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

sealed abstract class TableSpec[Arb, S, D, U, P, II, VV](tsb: TableSpecB[S, D, U, P, II, VV], needSave: (U, P) => SaveNeed) {
  import tsb.{p2ii, multiFieldRenderer}
  import tsb.savedUnsaved._

  @inline final protected val ST = ReactS.Fix[S]
  @inline final protected val nopIOS = ST.retM(IO(()))

  final type DP = (D, P)
  final type CSF = ComponentStateFocus[S]

  protected def createIO: (Arb, CSF, Retry[S], U) => ReactST[IO, S, Unit]

  protected def updateIO: (Arb, CSF, Retry[S], DP, U) => ReactST[IO, S, Unit]

  @inline private final def initSavedRow(p: P): SavedRow[P, II] =
    SavedRow(Sync, p, p2ii(p))

  def initialState(d: Seq[P], id: P => D): S = tsb.initialState(d.map(x => id(x) -> x))
  def initialState(d: Map[D, P])         : S = tsb.initialState(d.toSeq)
  def initialState(d: Seq[(D, P)])       : S = tsb.initialState(d)

  def unsavedInitS(empty: II) =
    ReactS.mod(unsavedL.modifyF(_ orElse Some(UnsavedRow(Sync, empty))))

  private def inputGateway[M[_]: Bind : Optional2, T](getT: S => M[T], trs: T => RowStatus, ti: T => II,
                                                      setI: (S, II) => M[S]): InputGatewayE[M, S, II] = {
    val te = trs andThen editMode
    val setA2 = (s: S, i: II) =>
      getT(s).toOption.flatMap(t => te(t) match {
        case EditMode.ReadWrite => setI(s, i).toOption
        case EditMode.ReadOnly  => None
      })
    new InputGateway(getT, te, ti, setA2)
  }

  private val editMode: RowStatus => EditMode = {
    case RowStatus.Sync | RowStatus.Failed(_) => EditMode.ReadWrite
    case RowStatus.Locked                     => EditMode.ReadOnly
  }

  private val unsavedS2OP: S => Option[P] = _ => None

  private val unsavedIG = inputGateway[Option, UnsavedRow[II]](
    unsavedLO.getOption, _.status, _.ii,
    (s, i) => unsavedL.get(s).map(_ => unsavedL.set(s, Some(UnsavedRow(Sync, i)))))

  private def savedIG(id: D): InputGatewayE[Id, S, II] = {
    val rL = rowL(id)
    val iL = rL composeLens savedIL
    inputGateway[Id, SavedRow[P, II]](rL.get, _.status, _.ii, iL.set)
  }

  private def unsavedRenderAttr(x: Arb) = {
    val mr2 = multiFieldRenderer(None).prepare(unsavedIG)
    (T: CSF) => {
      lazy val save: ReactST[IO, S, Unit] = ST.liftR(s =>
        mr2.savableU(s).fold(nopIOS)(u => createIO(x, T, Value(save), u)))
      for {
        rs <- unsavedStatusL.getOption(T.state)
        r = mr2.render(unsavedS2OP, save)
        v <- r(T)
      } yield (rs, v)
    }
  }

  private def savedRenderAttr(x: Arb)(id: D) = {
    val mr2 = multiFieldRenderer(Some(id)).prepare[Id](savedIG(id))
    val save1: S => Option[U] = mr2.savableU
    val save2: (S, U) => Option[(DP, U)] = (s, u) => {
      val dp = rowDP(id)(s)
      needSave(u, dp._2).asOption((dp, u))
    }
    val s2p = rowP(id)
    (T: CSF) => {
      lazy val save: ReactST[IO, S, Unit] = ST.liftR(s =>
        save1(s)
          .flatMap(save2(s, _))
          .fold(nopIOS)(dpu => updateIO(x, T, Value(save), dpu._1, dpu._2)))
      val r = mr2.render(s2p, save)
      val rs = rowStatus(id).get(T.state)
      (rs, r(T))
    }
  }

  def unsavedRow[V2](renderRow: (CSF, RowStatus, VV) => V2)(implicit x: Arb) = {
    val rr = rowRenderer[Option, S, VV, V2, Unit](
      _ => unsavedRenderAttr(x)(_),
      (T, _, r, v) => renderRow(T, r, v))
    (T: CSF) => rr(T)(())
  }

  def savedRowP[V2](renderRow: (CSF, D, RowStatus, P, VV) => V2)(implicit x: Arb) =
    savedRow((t, d, r, v) => renderRow(t, d, r, rowP(d)(t.state), v))

  def savedRow[V2](renderRow: (CSF, D, RowStatus, VV) => V2)(implicit x: Arb) =
    rowRenderer[Id, S, VV, V2, D](savedRenderAttr(x), renderRow)

  val unsavedRemoveF =
    unsavedL setF None

  val unsavedRemoveS =
    ReactS.mod(unsavedRemoveF)

  def savedRemoveF(id: D) =
    savedL.modifyF(m => m - id)

  def savedRemoveS(id: D) =
    ReactS.mod(savedRemoveF(id))

  def unsavedToSavedF(dp: DP): S => S =
    unsavedToSavedF(dp._1, dp._2)

  def unsavedToSavedF(id: D, p: P): S => S =
    savedSetF(id, p) compose unsavedRemoveF

  def savedSetF(dp: DP): S => S =
    savedSetF(dp._1, dp._2)

  def savedSetF(id: D, p: P): S => S =
    savedL.modifyF(_ + (id -> initSavedRow(p)))

  def savedSetS(dp: DP) =
    ST.mod(savedSetF(dp))

  def savedRevertS(id: D) =
    ST.mod(s => rowIL(id).set(s, p2ii(rowP(id)(s))))

  def savedDeleteIO_(f: D => IO[Unit]): D => ReactST[IO, S, Unit] =
    id => ReactS.retM(f(id)) >> savedRemoveS(id)

  def updateSavedIO_(saveIO: DP => IO[DP]): D => ReactST[IO, S, Unit] = id =>
    ST.gets(rowDP(id)).liftIO
      .flatMap(px1 => ST.retM(saveIO(px1)))
      .flatMap(savedSetS)

  final type SavedPs = Stream[SavedRowDP[D, P]]

  def savedRows(T: CSF, r: ComponentStateFocus[S] => D => Tag)(f: SavedPs => SavedPs) = {
    val rr = r(T)
    f(savedGet(T)).map(x => rr(x.d)).toJsArray
  }

  def savedGet(T: CSF): SavedPs =
    savedL.get(T.state).toStream.map{ case (d,SavedRow(r,p,_)) => SavedRowDP(r,d,p) }

  def unsavedGet(T: CSF): Option[UnsavedRow[II]] =
    unsavedL.get(T.state)

  def unsavedRowExists(T: CSF): Boolean =
    unsavedGet(T).isDefined
}

final case class SavedRowDP[D, P](status: RowStatus, d: D, p: P)

// =====================================================================================================================

object TableSpec {

  implicit object NoArb

  final class SyncSave[S, D, U, P, II, VV](
      tsb:           TableSpecB[S, D, U, P, II, VV],
      saveNotNeeded: (U, P) => SaveNeed,
      saveIO:        (Option[(D, P)], U) => IO[(D, P)])
      extends TableSpec[NoArb.type, S, D, U, P, II, VV](tsb, saveNotNeeded) {

    override protected def createIO = (_, _, _, u) =>
      ST.modT(s => saveIO(None, u).map(unsavedToSavedF(_)(s)))

    override protected def updateIO = (_, _, _, dp, u) =>
      ST.modT(s => saveIO(Some(dp), u).map(savedSetF(_)(s)))
  }

  // ===================================================================================================================

  final class AsyncSave[Arb, S, D, U, P, II, VV](
      tsb:           TableSpecB[S, D, U, P, II, VV],
      saveNotNeeded: (U, P) => SaveNeed,
      saveIO:        (Arb, Option[(D, P)], U, SuccessIO, FailureIO) => IO[Unit])
      extends TableSpec[Arb, S, D, U, P, II, VV](tsb, saveNotNeeded) {

    import tsb.savedUnsaved._

    override protected def createIO = (x, T, retry, u) =>
      saveS(x, T, retry, None, u, SuccessIO(T runState unsavedRemoveS))

    override protected def updateIO = (x, T, retry, dp, u) =>
      saveS(x, T, retry, Some(dp), u, SuccessIO.nop)

    private def saveS(x: Arb, T: CSF, retry: Retry[S], o: Option[(D, P)], u: U, s: SuccessIO) = {
      val row = o.map(_._1)
      val f = failureIO(T, row, retry)
      val io = saveIO(x, o, u, s, f)
      ST.retM(io) >> lockRowS(row).liftIO
    }

    private def setStatusS(status: RowStatus): Option[D] => ReactS[S, Unit] = {
      case None    => ReactS.mod(unsavedStatusL setF status)
      case Some(d) => ReactS.mod(rowStatus(d) setF status)
    }

    val lockRowS = setStatusS(RowStatus.Locked)

    def failureS(row: Option[D], retry: IO[Unit]) =
      setStatusS(RowStatus.Failed(retry))(row)

    def failureIO(T: CSF, row: Option[D], retry: Retry[S]) =
      FailureIO(T runState failureS(row, T runState retry.value))
  }

  // ===================================================================================================================

  sealed trait SaveNeed {
    def asOption[A](a: A): Option[A]
  }
  case object SaveNeeded extends SaveNeed {
    override def asOption[A](a: A) = Some(a)
  }
  case object SaveNotNeeded extends SaveNeed {
    override def asOption[A](a: A) = None
  }

  type Retry[S] = Name[ReactST[IO, S, Unit]]

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

  @inline private[TableSpec] final def rowRenderer[M[_] : Bind, S, V1, V2, SubId](
      renderFields: SubId => ComponentStateFocus[S] => M[(RowStatus, V1)],
      renderRow: (ComponentStateFocus[S], SubId, RowStatus, V1) => V2
      ): ComponentStateFocus[S] => SubId => M[V2] =
    T => id => renderFields(id)(T).map(rv => renderRow(T, id, rv._1, rv._2))
}

final case class SuccessIO(io: IO[Unit]) // TODO Either move and use in ClientProtocol too, or rename
object SuccessIO {
  def nop = SuccessIO(IO(()))
}
