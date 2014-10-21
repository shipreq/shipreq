package shipreq.webapp.client.util.ui.table

import japgolly.scalajs.react._, vdom.ReactVDom._, ScalazReact._
import scalaz.{Bind, Value}
import scalaz.Scalaz.Id
import scalaz.effect.IO
import shipreq.webapp.client.protocol.FailureIO
import shipreq.webapp.client.util.ui.Implicits._
import shipreq.webapp.client.util.ui._
import TableSpec._
import TableSpecU.UpdateIO

object TableSpecU {

  type UpdateIO[Arb, S, D, U, P] = (Arb, ComponentStateFocus[S], D, P, U, Retry[S], RowStatusS[S]) => ReactST[IO, S, Unit]

  def asyncUpdateIO[Arb, S, D, U, P](saveIO: (Arb, D, P, U, SuccessIO, FailureIO) => IO[Unit]): UpdateIO[Arb, S, D, U, P] =
    (x, T, d, p, u, retry, rs) => {
      val s  = SuccessIO.nop
      val f  = failureIO(T, rs, retry)
      val io = saveIO(x, d, p, u, s, f)
      ReactS.retM[IO, S, Unit](io) >> rs(RowStatus.Locked).liftIO
    }
}

class TableSpecU[Arb, S, D, U, P, II, _VV](val tsb: TableSpecB[S, D, U, P, II, _VV], needSave: (U, P) => SaveNeed, updateIO: UpdateIO[Arb, S, D, U, P]) {
  import tsb.{p2ii, multiFieldRenderer}
  import tsb.savedUnsaved._

  @inline final val ST = ReactS.Fix[S]
  @inline final val nopIOS = ST.retM(IO(()))

  final type DP = (D, P)
  final type CSF = ComponentStateFocus[S]
  final type VV = _VV

  @inline private final def initSavedRow(p: P): SavedRow[P, II] =
    SavedRow(RowStatus.Sync, p, p2ii(p))

  def initialState(d: Seq[P], id: P => D): S = tsb.initialState(d.map(x => id(x) -> x))
  def initialState(d: Map[D, P])         : S = tsb.initialState(d.toSeq)
  def initialState(d: Seq[(D, P)])       : S = tsb.initialState(d)

  private[table] def inputGateway[M[_]: Bind : Optional2, T](getT: S => M[T], trs: T => RowStatus, ti: T => II,
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

  private def savedIG(id: D): InputGatewayE[Id, S, II] = {
    val rL = srowL(id)
    val iL = rL composeLens savedRowIL
    inputGateway[Id, SavedRow[P, II]](rL.get, _.status, _.ii, iL.set)
  }

  private def savedRenderAttr(x: Arb)(id: D) = {
    val mr2 = multiFieldRenderer(Some(id)).prepare[Id](savedIG(id))
    val save1: S => Option[U] = mr2.savableU
    val save2: (S, U) => Option[(DP, U)] = (s, u) => {
      val dp = srowDP(id)(s)
      needSave(u, dp._2).asOption((dp, u))
    }
    val s2p = srowP(id)
    (T: CSF) => {
      lazy val save: ReactST[IO, S, Unit] = ST.liftR(s =>
        save1(s)
          .flatMap(save2(s, _))
          .fold(nopIOS){case ((d, p), u) => updateIO(x, T, d, p, u, Value(save), savedStatusSetS(d)) })
      val r = mr2.render(s2p, save)
      val rs = srowStatusL(id).get(T.state)
      (rs, r(T))
    }
  }

  def savedRowP[V2](renderRow: (CSF, D, RowStatus, P, VV) => V2)(implicit x: Arb) =
    savedRow((t, d, r, v) => renderRow(t, d, r, srowP(d)(t.state), v))

  def savedRow[V2](renderRow: (CSF, D, RowStatus, VV) => V2)(implicit x: Arb) =
    rowRenderer[Id, S, VV, V2, D](savedRenderAttr(x), renderRow)

  def savedRemoveF(id: D) =
    savedL.modifyF(m => m - id)

  def savedRemoveS(id: D) =
    ReactS.mod(savedRemoveF(id))

  def savedSetF(dp: DP): S => S =
    savedSetF(dp._1, dp._2)

  def savedSetF(id: D, p: P): S => S =
    savedL.modifyF(_ + (id -> initSavedRow(p)))

  def savedSetS(dp: DP) =
    ST.mod(savedSetF(dp))

  def savedRevertS(id: D) =
    ST.mod(s => srowIL(id).set(s, p2ii(srowP(id)(s))))

  def savedDeleteIO_(f: D => IO[Unit]): D => ReactST[IO, S, Unit] =
    id => ReactS.retM(f(id)) >> savedRemoveS(id)

  def updateSavedIO_(saveIO: DP => IO[DP]): D => ReactST[IO, S, Unit] = id =>
    ST.gets(srowDP(id)).liftIO
      .flatMap(px1 => ST.retM(saveIO(px1)))
      .flatMap(savedSetS)

  final type SavedPs = Stream[SavedRowDP[D, P]]

  def savedRows(T: CSF, r: ComponentStateFocus[S] => D => Tag)(f: SavedPs => SavedPs) = {
    val rr = r(T)
    f(savedGet(T)).map(x => rr(x.d)).toJsArray
  }

  def savedGet(T: CSF): SavedPs =
    savedL.get(T.state).toStream.map{ case (d,SavedRow(r,p,_)) => SavedRowDP(r,d,p) }

  def savedStatusSetS(d: D) = (status: RowStatus) =>
    ReactS.mod(srowStatusL(d) setF status)
}