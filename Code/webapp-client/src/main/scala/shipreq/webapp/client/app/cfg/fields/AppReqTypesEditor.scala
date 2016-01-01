package shipreq.webapp.client.app.cfg.fields

import japgolly.scalajs.react._, vdom.prefix_<^._
import monocle.Lens
import monocle.std.map.atMap
import scalacss.ScalaCssReact._
import shipreq.base.util.{UnivEq, IMap}
import shipreq.webapp.base.data._
import shipreq.webapp.client.app.Style
import shipreq.webapp.client.app.cfg.shared.{FieldSet => _, _}
import shipreq.webapp.client.lib.Logger
import shipreq.webapp.client.widgets.ISubsetEditor
import Field.ApplicableReqTypes
import ISubsetEditor._

private[fields] object AppReqTypesEditor {
  type A = ReqTypeId
  type M = Mode[A]
  type K = FieldId
  type S = Map[K, EditState[A]]

  def initialState(fs: FieldSet): S = UnivEq.emptyMap

  @inline final def stateFor(k: K): Lens[S, Option[EditState[A]]] = atMap.at(k)
}

// =====================================================================================================================
class AppReqTypesEditor(customReqTypes: TraversableOnce[CustomReqType]) {
  import AppReqTypesEditor._

  val reqtypemap = {
    def allReqTypes: List[ReqType] = {
      val x = StaticReqType.values.foldLeft(Nil: List[ReqType])((q, v) => v :: q)
      customReqTypes.foldLeft(x)((q, v) => v :: q)
    }
    IMap.empty((_: ReqType).reqTypeId) ++ allReqTypes
  }

  def lookup(id: A) = reqtypemap.get(id).get

  val preprocess: Stream[A] => Stream[A] =
    _.sortBy(lookup(_).mnemonic.value)

  val renderValue: A => ReactNode = id => {
    val a = lookup(id)
    if (a.fold(_ => true, _.live :: Live))
      a.mnemonic.value
    else
      <.span(Style.cfg.deadMnemonic, a.mnemonic.value)
  }

  val static = ISubsetEditor.StaticProps[A](preprocess, renderValue, reqtypemap.keys)

  val component = ISubsetEditor.Component(static)

  def editor($: CompState.Access[S]): SimpleEditor2[(Option[K], ApplicableReqTypes), ApplicableReqTypes] =
    Editor { ei =>
      val (id, value) = ei.data

      def cbh(cb: ei.CBH): Callback =
        ei.editable.fold(Logger(_ log s"Can't interpret ApplicableReqTypesEditor callback."))(_(cb))

      val mode: M =
        id match {
          case Some(k) =>
            @inline def * = stateFor(k)
            def setIO(s: EditState[A]): Callback = $ modState *.set(Some(s))

            $.state.runNow().get(k) match {

              case None =>
                ViewMode(
                  value     = value,
                  startEdit = ei.editable.map(_ => setIO(EditState.init(value, UnivEq.emptySet))))

              case Some(es) =>
                EditMode[A](
                  state      = es,
                  update     = setIO,
                  finishEdit = _.fold[Callback](
                                 $ modState *.set(None))(
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