package shipreq.webapp.client.app.ui.cfg.fields

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._, MonocleReact._
import japgolly.scalajs.react.extra.OnUnmount
import monocle.macros.Lenser
import monocle.Optional
import monocle.std.option.some
import scala.annotation.tailrec
import scala.language.reflectiveCalls
import scalajs.js.{undefined, UndefOr, UndefOrOps, Array => JsArray}
import scalajs.js.JSConverters._
import scalaz.effect.IO
import scalaz.{Maybe, Memo, \&/, -\/, \/-}
import scalaz.std.AllInstances._
import scalaz.syntax.equal._
import scalaz.syntax.bind.ToBindOps
import scalaz.syntax.foldable1._

import shipreq.prop.CycleDetector
import shipreq.prop.util.Multimap
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._, DataImplicits._
import shipreq.webapp.base.delta.Partition
import shipreq.webapp.base.data.Validators.{field => V}
import shipreq.webapp.base.protocol.DeletionAction._
import shipreq.webapp.base.protocol.FieldProtocol
import shipreq.webapp.base.protocol.Routines.FieldCrud
import shipreq.webapp.base.UiText.FieldNames
import shipreq.webapp.client.ClientData
import shipreq.webapp.client.app.ui.{RowDetailButton, ShowDeletedToggler}
import shipreq.webapp.client.lib.{FailureIO, SuccessIO, CrudIO}
import shipreq.webapp.client.lib.ui._
import shipreq.webapp.client.protocol.ClientProtocol
import shipreq.webapp.client.util.DND
import FieldProtocol.Delta

object CfgFields {
  case class Props(cp: ClientProtocol, remote: FieldCrud.Remote, clientData: ClientData, showDeleted: Boolean) {
    def component: ReactComponentU_ = MainTable.Component(this)
  }
}

import CfgFields.Props

private[fields] object MainTable {

  val nameE      = Editors.textInputEditor.applyValidator(V.nameS)
  val refkeyE    = Editors.textInputEditor.applyValidator(V.keyS)
  val mandatoryE = Editors.checkboxEditor.imap(Mandatory).strengthL[V.S]

  val text_fields = FieldSet4[CustomField.Text](
    _.name, _.key.value, _.mandatory, _.reqTypes)(
    ("", "", Mandatory.Not, ISubset.All()))

  val text_stores = NewAndSavedStores.fields(text_fields).keyedBy[CustomField.Id]

  case class State(showDeleted: Boolean,
                   text_state: text_stores.State)
  object State {
    private[this] def l = Lenser[State]
    val _showDeleted = l(_.showDeleted)
    val _text_state  = l(_.text_state)
  }

  type S  = State
  type ST = ReactST[IO, S, Unit]
  val  ST = ReactS.FixT[IO, S]

  val text_storesS = text_stores.contramap(State._text_state)

  def storesForType(t: CustomFieldType): NewAndSavedStores[S, CustomField.Id, _ <: CustomField, _] =
    t match {
      case CustomFieldType.Text => text_storesS
    }

  val customFieldStores = CustomFieldType.values.list map storesForType toStream

  def initialState(p: Props): S = {
    val textFields = Seq.newBuilder[CustomField.Text]
    p.clientData.project.fields.data.customFields.values.foreach {
      case f: CustomField.Text => textFields += f
    }
    State(p.showDeleted,
      text_state = text_stores.initState(_.initStateS(textFields.result(), _.id)))
  }

  val deltaFns =
    new RemoteDeltaListener.StateFns[S, Field.Id, Delta](
      (s, i) => i match {
        case _: StaticField => s
        case j: CustomField.Id => customFieldStores.foldLeft(s)((t, f) => f.s.remove(j)(t))
      },
      (s, _, d) => d match {
        case Delta(-\/(_: StaticField     ), _) => s
        case Delta(\/-(f: CustomField.Text), _) => text_storesS.s.set(f.id, f)(s)
      })

  val Component =
    ReactComponentB[Props]("Cfg: Fields")
      .getInitialState(initialState)
      .backend(new Backend(_))
      .render(_.backend.render)
      .configure(RemoteDeltaListener(Delta).install(deltaFns, Partition.Fields, _.clientData))
      .build

  def validatorState(s: S, k: Option[CustomField.Id]): V.S = {
    val customFieldStream = customFieldStores.flatMap(_.s.getAllP(s))
    (customFieldStream, k)
  }

  // ===================================================================================================================
  final class Backend(c: BackendScope[Props, S]) extends OnUnmount {

    def validatorState(k: Option[CustomField.Id]): S => V.S =
      MainTable.validatorState(_, k)

    val headerRow = CfgTable.header(List(
      FieldNames.name,
      FieldNames.fieldType,
      FieldNames.fieldRefKey,
      FieldNames.mandatory,
      FieldNames.applicableReqTypes))

    def render =
      <.div(
        ShowDeletedToggler(c.state.showDeleted, c runState ST.modT(State._showDeleted.modify(b => !b))),
        <.table(
          headerRow,
          <.tbody(renderFields)
        ))

    def renderFields: TagMod = {
      val order = c.props.clientData.project.fields.data.order

      // TODO add to scalajs-react ?
      val array = new JsArray[ReactNode]()
      order.foreach(f => renderField(f).foreach(array push _))
      array
    }

    def renderField(f: Field.Id): UndefOr[ReactTag] = f match {
      case c: CustomField.Id => undefined // TODO customFieldStores.map(_.s.g)
      case s: StaticField    => renderStaticField(s)
    }

    def renderStaticField(f: StaticField): ReactTag =
      renderRow(
        name      = f.name,
        ftype     = f.fieldType.name,
        refkey    = renderKeyO(f.keyO),
        mandatory = Editors.staticCheckbox(Mandatory from f.mandatory),
        reqtypes  = "TODO"
      )(^.key := f.name)

    def renderKeyO(k: Option[FieldRefKey]): TagMod =
      k.fold("-")(_.value)

//    def renderCustomField(f: CustomField): ReactTag =
//      renderRow(
//        name      = f.name,
//        ftype     = f.fieldType.name,
//        refkey    = renderKeyO(f.keyO),
//        mandatory = Editors.staticCheckbox(Mandatory from f.mandatory),
//        reqtypes  = "TODO"
//      )(^.key := f.id.value)

    def renderRow(name: TagMod, ftype: TagMod, refkey: TagMod, mandatory: TagMod, reqtypes: TagMod): ReactTag =
      <.tr(
        <.td(name),
        <.td(ftype),
        <.td(refkey),
        <.td(mandatory),
        <.td(reqtypes))

    // -----------------------------------------------------------------------------------------------------------------
    // Subtype

    /*
    abstract class SubtypeRenderer[F <: Field, I, B, D, V](
      final val editor: Editor[(V.S, I), B, IO, S, D, IO[Unit], V],
      final val stores: NewAndSavedStores[S, Id, T, I]) {

      val editable = editor.editableByRowStatus(c)

      val deletion =
        Persistence.asyncDeletionS(stores.s)(_.alive, crudIO._deleteIO, c runState _)

      def ei(s: S, r: stores.s.Row): editor.Input = {
        val a = (validatorState(r.p.id.some)(s), r.i)
        EditorI(a, "", editable(r.status))
      }

      def ei(s: S, r: stores.n.Row): editor.Input = {
        val a = (validatorState(None)(s), r.i)
        EditorI(a, "", editable(r.status))
      }

      def renderNew  (s: S, r: stores.n.Row): ReactElement
      def renderAlive(s: S, indent: Indenter, key: String)(r: stores.s.Row): ReactElement
      def renderDead (s: S, indent: Indenter, key: String)(rs: RowStatus, t: T): ReactElement

      val unusedField: ReactNode = "-"

      def rowTemplate(s: S, oid: UndefOr[Id], rs: RowStatus, key: String)(name: ReactNode, refkey: ReactNode, mutexChildren: ReactNode, desc: ReactNode)(ctrls: => TagMod): ReactElement = {
        val focus = oid.map(id =>
          RowDetailButton.Props.forRow(id)(s.detailRow.map(_.id), c _modStateIO setDetail))
        <.tr(
          ^.key := key,
          ^.classSet1(UI.rowStatusRowClass(rs), "focusrow" -> focus.exists(_.isActive)),
          <.td(^.cls := "name", name),
          <.td(refkey),
          <.td(mutexChildren),
          <.td(^.cls := "desc", desc),
          <.td(
            focus.map(_.component),
            UI.rowStatusCtrls(rs, ctrls)))
      }

      def newRowTemplate(s: S, rs: RowStatus)(name: ReactNode, refkey: ReactNode, mutexChildren: ReactNode, desc: ReactNode): ReactElement =
        rowTemplate(s, undefined, rs, "new")(name, refkey, mutexChildren, desc)(abortNewButton)

      def renderRow(s: S, row: stores.s.Row): F = F { (keyp, indent) =>
        val tag = row.p
        def key = s"$keyp.${tag.id.value}"
        tag.alive match {
          case Alive => renderAlive(s, indent, key)(row)
          case Dead  => renderDead (s, indent, key)(row.status, tag)
        }
      }

      def all(s: S): Stream[(Id, F)] =
        stores.s.getAll(s).map(row => row.p.id -> renderRow(s, row))

      def newRow(s: S): Option[ReactElement] =
        stores.n.get(s).map(renderNew(s, _))
    }
    */
  }
}