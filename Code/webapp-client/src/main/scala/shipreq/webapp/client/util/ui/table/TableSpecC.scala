package shipreq.webapp.client.util.ui.table

import japgolly.scalajs.react._, ScalazReact._
import scalaz.Value
import scalaz.effect.IO
import scalaz.std.option._
import shipreq.webapp.client.protocol.FailureIO
import shipreq.webapp.client.util.ui.Implicits._
import TableSpec._
import TableSpecC.CreateIO

object TableSpecC {

  type CreateIO[Arb, S, U] = (Arb, ComponentStateFocus[S], U, ReactS[S, Unit], Retry[S], RowStatusS[S]) => ReactST[IO, S, Unit]

  def apply[Arb, S, D, U, P, II, _VV](spec: TableSpecU[Arb, S, D, U, P, II, _VV])
                                     (create: (Arb, U, SuccessIO, FailureIO) => IO[Unit]) =
    new TableSpecC(spec, asyncCreateIO[Arb, S, U](create))

  def asyncCreateIO[Arb, S, U](saveIO: (Arb, U, SuccessIO, FailureIO) => IO[Unit]): CreateIO[Arb, S, U] =
    (x, T, u, unsavedRemoveS, retry, rs) => {
      val s  = SuccessIO(T runState unsavedRemoveS)
      val f  = failureIO(T, rs, retry)
      val io = saveIO(x, u, s, f)
      ReactS.retM[IO, S, Unit](io) >> rs(RowStatus.Locked).liftIO
    }
}

class TableSpecC[Arb, S, D, U, P, II, _VV](spec: TableSpecU[Arb, S, D, U, P, II, _VV], createIO: CreateIO[Arb, S, U]) {
  import spec._, tsb.multiFieldRenderer, tsb.savedUnsaved._

  def unsavedInitS(empty: II) =
    ReactS.mod(unsavedL.modifyF(_ orElse Some(UnsavedRow(RowStatus.Sync, empty))))

  private val unsavedS2OP: S => Option[P] = _ => None

  private val unsavedIG = inputGateway[Option, UnsavedRow[II]](
    unsavedRowL.getOption, _.status, _.ii,
    (s, i) => unsavedL.get(s).map(_ => unsavedL.set(s, Some(UnsavedRow(RowStatus.Sync, i)))))

  private def unsavedRenderAttr(x: Arb) = {
    val mr2 = multiFieldRenderer(None).prepare(unsavedIG)
    (T: CSF) => {
      lazy val save: ReactST[IO, S, Unit] = ST.liftR(s =>
        mr2.savableU(s).fold(nopIOS)(u => createIO(x, T, u, unsavedRemoveS, Value(save), unsavedStatusSetS)))
      for {
        rs <- unsavedStatusL.getOption(T.state)
        r = mr2.render(unsavedS2OP, save)
        v <- r(T)
      } yield (rs, v)
    }
  }

  def unsavedRow[V2](renderRow: (ComponentStateFocus[S], RowStatus, VV) => V2)(implicit x: Arb) = {
    val rr = rowRenderer[Option, S, VV, V2, Unit](
      _ => unsavedRenderAttr(x)(_),
      (T, _, r, v) => renderRow(T, r, v))
    (T: ComponentStateFocus[S]) => rr(T)(())
  }

  def unsavedGet(T: CSF): Option[UnsavedRow[II]] =
    unsavedL.get(T.state)

  def unsavedRowExists(T: CSF): Boolean =
    unsavedGet(T).isDefined

  val unsavedRemoveF =
    unsavedL setF None

  val unsavedRemoveS =
    ReactS.mod(unsavedRemoveF)

  def unsavedToSavedF(dp: DP): S => S =
    unsavedToSavedF(dp._1, dp._2)

  def unsavedToSavedF(id: D, p: P): S => S =
    savedSetF(id, p) compose unsavedRemoveF

  def unsavedStatusSetS(status: RowStatus) =
    ReactS.mod(unsavedStatusL setF status)

  def statusSetS(status: RowStatus): Option[D] => ReactS[S, Unit] = {
    case None    => unsavedStatusSetS(status)
    case Some(d) => savedStatusSetS(d)(status)
  }
}
