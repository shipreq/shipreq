package shipreq.webapp.client.project.app.pages.config_old.fields

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.scalajs.react._
import vdom.html_<^._
import japgolly.univeq.UnivEq
import monocle.Lens
import scalacss.ScalaCssReact._
import shipreq.base.util.{IMap, Optics}
import shipreq.webapp.base.data._
import shipreq.webapp.base.lib.LoggerJs
import shipreq.webapp.client.project.app.Style
import shipreq.webapp.client.project.app.pages.config_old.shared.{FieldSet => _, _}
import shipreq.webapp.client.project.widgets.ISubsetEditor
import Field.ApplicableReqTypes
import ISubsetEditor._

private[fields] object AppReqTypesEditor {
  type A = ReqTypeId
  type M = Mode[A]
  type K = FieldId
  type S = Map[K, EditState[A]]

  def initialState(fs: FieldSet): S =
    UnivEq.emptyMap

  final def stateFor(k: K): Lens[S, Option[EditState[A]]] =
    Optics.mapValue(k)
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

  val preprocess: TraversableOnce[A] => TraversableOnce[A] =
    i => MutableArray(i).sortBy(lookup(_).mnemonic.value).iterator

  val renderValue: A => VdomNode = id => {
    val a = lookup(id)
    if (a.fold(_ => true, _.live is Live))
      a.mnemonic.value
    else
      <.span(Style.cfg.deadMnemonic, a.mnemonic.value)
  }

  val static = ISubsetEditor.StaticProps[A](preprocess, renderValue, reqtypemap.keys)

  val component = ISubsetEditor.Component(static)

  def editor($: StateAccessPure[S]): SimpleEditor2[(Option[K], ApplicableReqTypes), ApplicableReqTypes] =
    Editor { ei =>
      val (id, value) = ei.data

      def cbh(cb: ei.CBH): Callback =
        ei.editable.fold(LoggerJs(_ log s"Can't interpret ApplicableReqTypesEditor callback."))(_(cb))

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