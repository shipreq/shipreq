package shipreq.webapp.client.app.ui.cfg.fields

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._, MonocleReact._
import monocle.Lens
import monocle.std.map.atMap
import scalaz.effect.IO
import scalaz.syntax.bind.ToBindOps
import scalaz.syntax.equal._
import shipreq.base.util.{UnivEq, IMap}
import shipreq.webapp.base.data._
import shipreq.webapp.client.app.ui.ISubsetEditor
import shipreq.webapp.client.lib.ui.{FieldSet => _, _}
import shipreq.webapp.client.lib.ConsoleIO
import Field.ApplicableReqTypes
import ISubsetEditor._

private[fields] object AppReqTypesEditor {
  type A = ReqType.Id
  type M = Mode[A]
  type K = Field.Id
  type S = Map[K, EditState[A]]

  def initialState(fs: FieldSet): S = UnivEq.emptyMap

  @inline final def stateFor(k: K): Lens[S, Option[EditState[A]]] = atMap.at(k)
}

// =====================================================================================================================
class AppReqTypesEditor(customReqTypes: TraversableOnce[CustomReqType]) {
  import AppReqTypesEditor._

  val reqtypemap = {
    def allReqTypes: List[ReqType] = {
      val x = StaticReqType.values.list.foldLeft(Nil: List[ReqType])((q, v) => v :: q)
      customReqTypes.foldLeft(x)((q, v) => v :: q)
    }
    IMap.empty((_: ReqType).reqTypeId) ++ allReqTypes
  }

  def lookup(id: A) = reqtypemap.get(id).get

  val preprocess: Stream[A] => Stream[A] =
    _.sortBy(lookup(_).mnemonic.value)

  val renderValue: A => ReactNode = id => {
    val a = lookup(id)
    if (a.fold(_ => true, _.alive ≟ Alive))
      a.mnemonic.value
    else
      <.span(CSS.deadInline, a.mnemonic.value)
  }

  val static = ISubsetEditor.StaticProps[A](preprocess, renderValue, reqtypemap.keys)

  val component = ISubsetEditor.Component(static)

  def editor($: ComponentStateFocus[S]): SimpleEditor2[(Option[K], ApplicableReqTypes), ApplicableReqTypes] =
    Editor { ei =>
      val (id, value) = ei.data

      def cbh(cb: ei.CBH): IO[Unit] =
        ei.editable.fold(ConsoleIO(_ log s"Can't interpret ApplicableReqTypesEditor callback."))(_(cb))

      val mode: M =
        id match {
          case Some(k) =>
            @inline def * = stateFor(k)
            def setIO(s: EditState[A]): IO[Unit] = $ modStateIO *.set(Some(s))

            $.state.get(k) match {

              case None =>
                ViewMode(
                  value     = value,
                  startEdit = ei.editable.map(_ => setIO(EditState.init(value, UnivEq.emptySet))))

              case Some(es) =>
                EditMode[A](
                  state      = es,
                  update     = setIO,
                  finishEdit = _.fold[IO[Unit]](
                                 $ modStateIO *.set(None))(
                                 SimpleEditor.onChangeAndEditFinished(cbh)))
            }

          case None =>
            ViewMode(value, None)
        }

      component(mode)
    }

  def renderReadOnly(a: ApplicableReqTypes) =
    component(ViewMode(a, None))

}