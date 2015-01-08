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
import scalaz.{OneAnd, Maybe, Memo, \&/, -\/, \/-}
import scalaz.std.AllInstances._
import scalaz.syntax.equal._
import scalaz.syntax.bind.ToBindOps
import scalaz.syntax.foldable1._

import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._, DataImplicits._
import shipreq.webapp.base.delta.Partition
import shipreq.webapp.base.data.Validators.{field => V}
import shipreq.webapp.base.protocol.{DeletionAction, FieldProtocol}
import shipreq.webapp.base.protocol.Routines.FieldCrud
import shipreq.webapp.base.UiText.FieldNames
import shipreq.webapp.client.ClientData
import shipreq.webapp.client.app.ui.{RowDetailButton, ShowDeletedToggler}
import shipreq.webapp.client.lib.{FailureIO, SuccessIO}
import shipreq.webapp.client.lib.ui._
import shipreq.webapp.client.protocol.ClientProtocol
import shipreq.webapp.client.util.DND
import Field.ApplicableReqTypes
import FieldProtocol.Delta
import DeletionAction._

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
  val reqtypesE  = Editors.constSimpleEditor[ApplicableReqTypes](<.span("TODO")).strengthL[V.S]

  val text_fields = FieldSet4[CustomField.Text](
    _.name, _.key.value, _.mandatory, _.reqTypes)(
    ("", "", Mandatory.Not, ISubset.All()))

  val text_stores = NewAndSavedStores.fields(text_fields).keyedBy[CustomField.Id]

  case class State(showDeleted: Boolean,
                   text_state: text_stores.State) {

//    lazy val customFieldStream: Stream[CustomField] =
//      customFieldStores.flatMap(_.s.getAllP(this).map(f => f: CustomField))

    lazy val customFields =
      customFieldStores.foldLeft(CustomField.IdAccess.emptyIMap)(_ ++ _.s.getAllP(this))
  }

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

  val rowIdFromEditorInput: ((V.S, Any)) => Option[CustomField.Id] = _._1._2

  // ===================================================================================================================
  final class Backend(c: BackendScope[Props, S]) extends OnUnmount {

    val clientData = c.props.clientData

    object protocol {
      import FieldProtocol._, CfgAction._
      val remote = c.props.remote
      val cp     = c.props.cp

      private def call(a: CfgAction): (SuccessIO, FailureIO) => IO[Unit] =
        (s, f) => cp.call(remote)(a, clientData.update(_) >> s.io, f)

      def createIO(v: FieldProtocol.Values) =
        call(Create(v))

      def updateValuesIO(i: CustomField.Id, v: FieldProtocol.Values) =
        call(UpdateValues(i, v))

//      val updateOrderIO: (Field.Id, Position, SuccessIO, FailureIO) => IO[Unit] =
//        (i, p, s, f) => call(UpdateOrder(i, p), s, f)

      def deleteIO(i: Field.Id, a: DeletionAction) =
        call(Delete(i, a))
    }

    // TODO staticDeletion doesn't handle failure (or lock row)
    val staticDeletion = new Deletion[StaticField, Field.Id](
      _ => Alive, protocol.deleteIO(_, _)(SuccessIO.nop, FailureIO.nop))

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
      val order = clientData.project.fields.data.order

      var content = order.toStream
        .flatMap(_.foldId[Stream[Field]](s => Stream(s), c.state.customFields.get(_).toStream))

      if (!c.state.showDeleted)
        content = content.filter(Field.filterAlive)

      content.toReactNodeArray(renderField)

      // TODO add to scalajs-react ?
//      val array = new JsArray[ReactNode]()
//      order.foreach(f => renderField(f).foreach(array push _))
//      array
    }

    def renderField(gf: Field): ReactElement = gf match {
      case f: CustomField.Text => text_renderer.render(c.state, f.id)
      case s: StaticField      => renderStaticField(s)
    }

//    def renderField(fid: Field.Id): UndefOr[ReactElement] = fid match {
//      case id: CustomField.Id =>
//        c.state.customFields.get(id).orUndefined.map {
//          case f: CustomField.Text => text_renderer.render(c.state, f.id)
//        }
//      case s: StaticField    => renderStaticField(s)
//    }

    def renderStaticField(f: StaticField): ReactElement =
      renderRow(RowStatus.Sync)(
        name      = f.name,
        ftype     = f.fieldType,
        refkey    = renderKeyO(f.keyO),
        mandatory = Editors.staticCheckbox(Mandatory from f.mandatory),
        reqtypes  = renderApReqTypes(f.reqTypes),
        ctrls     = (f.deletable ≟ Deletable) ?= staticDeletion.button(f, SoftDel)
      )(^.key := f.name)

    def renderKeyO(k: Option[FieldRefKey]): TagMod =
      k.fold("-")(_.value)

    def renderApReqTypes(a: ApplicableReqTypes): TagMod = {
      def fmt(prefix: String, rs: OneAnd[Set, ReqType.Id]) =
        (rs.head #:: rs.tail.toStream)
          .flatMap(clientData.project.reqType(_).toStream)
          //.filter(ReqType.filterAlive) // TODO should render with strike-through
          .map(_.mnemonic.value)
          .sorted
          .mkString(prefix, ", ", ".")
      a match {
        case ISubset.Only(rs) => fmt("Only: ", rs)
        case ISubset.Not(rs)  => fmt("Not: ", rs)
        case ISubset.All()    => "All."
      }
    }

//    def renderCustomField(f: CustomField): ReactTag =
//      renderRow(
//        name      = f.name,
//        ftype     = f.fieldType.name,
//        refkey    = renderKeyO(f.keyO),
//        mandatory = Editors.staticCheckbox(Mandatory from f.mandatory),
//        reqtypes  = "TODO"
//      )(^.key := f.id.value)

    def renderRow(rs: RowStatus)
                 (name: TagMod, ftype: FieldType, refkey: TagMod, mandatory: TagMod, reqtypes: TagMod, ctrls: => TagMod): ReactTag =
      <.tr(
        ^.cls := UI.rowStatusRowClass(rs),
        <.td(name),
        <.td(ftype.name),
        <.td(refkey),
        <.td(mandatory),
        <.td(reqtypes),
        <.td(UI.rowStatusCtrls(rs, ctrls)))

    // -----------------------------------------------------------------------------------------------------------------
    // Subtype

    val unusedField: ReactNode = "-"

    abstract class SubtypeRenderer[T <: CustomField, I, B, D, V](
      final val editor: Editor[(V.S, I), B, IO, S, D, IO[Unit], V],
      final val stores: NewAndSavedStores[S, CustomField.Id, T, I]) {

      val editable = editor.editableByRowStatus(c)

      val deletion =
        Persistence.asyncDeletionS(stores.s)(_.alive, protocol.deleteIO, c runState _)

      def ei(s: S, r: stores.s.Row): editor.Input = {
        val a = (validatorState(r.p.id.some)(s), r.i)
        EditorI(a, "", editable(r.status))
      }

      def ei(s: S, r: stores.n.Row): editor.Input = {
        val a = (validatorState(None)(s), r.i)
        EditorI(a, "", editable(r.status))
      }

//      def renderNew  (s: S, r: stores.n.Row): ReactElement
      def renderAlive(s: S, r: stores.s.Row): ReactElement
      def renderDead (s: S, rs: RowStatus, t: T): ReactElement

//      def rowTemplate(s: S, oid: UndefOr[CustomField.Id], rs: RowStatus, key: String)
//                     (name: ReactNode, refkey: ReactNode, mutexChildren: ReactNode, desc: ReactNode)
//                     (ctrls: => TagMod): ReactElement = {
//        val focus = oid.map(id =>
//          RowDetailButton.Props.forRow(id)(s.detailRow.map(_.id), c _modStateIO setDetail))
//        <.tr(
//          ^.key := key,
//          ^.classSet1(UI.rowStatusRowClass(rs), "focusrow" -> focus.exists(_.isActive)),
//          <.td(^.cls := "name", name),
//          <.td(refkey),
//          <.td(mutexChildren),
//          <.td(^.cls := "desc", desc),
//          <.td(
//            focus.map(_.component),
//            UI.rowStatusCtrls(rs, ctrls)))
//      }

//      def newRowTemplate(s: S, rs: RowStatus)(name: ReactNode, refkey: ReactNode, mutexChildren: ReactNode, desc: ReactNode): ReactElement =
//        rowTemplate(s, undefined, rs, "new")(name, refkey, mutexChildren, desc)(abortNewButton)

      def render(s: S, id: CustomField.Id) = {
        val row = stores.s.get(id)(s)
        val tag = row.p
        tag.alive match {
          case Alive => renderAlive(s, row)
          case Dead  => renderDead (s, row.status, tag)
        }
      }

//      def newRow(s: S): Option[ReactElement] =
//        stores.n.get(s).map(renderNew(s, _))
    } // SubtypeRenderer

    // -----------------------------------------------------------------------------------------------------------------
    // Text field

    val text_editor = {
      @inline def stores = text_storesS
      val toValues  = FieldProtocol.TextFieldValues.apply _
      val toValuesT = toValues.tupled
      val saveFn    = Persistence.asyncSaveNS2(V.text map toValuesT, stores, protocol.createIO)(protocol.updateValuesIO,
        SaveNeed.cmpToExtract(t => toValues(t.name, t.key, t.mandatory, t.reqTypes)),
        _.id, validatorState, c runState _)
      Editor.merge4S(text_fields, nameE, refkeyE, mandatoryE, reqtypesE).tupleI.zoomU[S]
        .applyRowUpdateAndRevert(stores)(rowIdFromEditorInput)
        .applyOnEditFinishedK(saveFn)(rowIdFromEditorInput)
    }

    val text_renderer = new SubtypeRenderer(text_editor, text_storesS) {
//      override def renderNew(s: S, row: stores.n.Row): ReactElement = {
//        val (name, mutexChildren, desc) = editor render ei(s, row)
//        newRowTemplate(s, row.status)(name, unusedField, mutexChildren, desc)
//      }
      override def renderAlive(s: S, row: stores.s.Row): ReactElement = {
        val (name, refkey, mandatory, reqtypes) = editor render ei(s, row)
        val f = row.p
        renderRow(row.status)(
          name      = name,
          ftype     = f.fieldType,
          refkey    = refkey,
          mandatory = mandatory,
          reqtypes  = reqtypes,
          ctrls     = deletion.button(f.id, SoftDel)
        )(^.key := f.id.value)
      }

      override def renderDead(s: S, rs: RowStatus, f: CustomField.Text): ReactElement =
        renderRow(rs)(
          name      = f.name,
          ftype     = f.fieldType,
          refkey    = f.key.value,
          mandatory = Editors.staticCheckbox(Mandatory from f.mandatory),
          reqtypes  = renderApReqTypes(f.reqTypes),
          ctrls     = deletion.button(f.id, Restore)
        )(^.key := f.id.value)
    }

  }
}