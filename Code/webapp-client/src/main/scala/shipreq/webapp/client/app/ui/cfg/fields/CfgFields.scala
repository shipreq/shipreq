package shipreq.webapp.client.app.ui.cfg.fields

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._, MonocleReact._
import japgolly.scalajs.react.extra.OnUnmount
import monocle.macros.Lenser
import scala.language.reflectiveCalls
import scalajs.js.{undefined, UndefOr, Any => JsAny}
import scalaz.effect.IO
import scalaz.{Equal, Maybe, -\/, \/-, \/}
import scalaz.syntax.bind.ToBindOps
import scalaz.syntax.equal._

import shipreq.base.util.ScalaExt._
import shipreq.base.util.Util
import shipreq.webapp.base.data._
import shipreq.webapp.base.delta.Partition
import shipreq.webapp.base.data.Validators.{field => V}
import shipreq.webapp.base.protocol.{DeletionAction, FieldProtocol}
import shipreq.webapp.base.protocol.Routines.FieldCrud
import shipreq.webapp.base.UiText, UiText.FieldNames
import shipreq.webapp.client.ClientData
import shipreq.webapp.client.app.ui.{SelectInvoke, SelectOne, ShowDeletedToggler}
import shipreq.webapp.client.lib.{ConsoleIO, FailureIO, SuccessIO}
import shipreq.webapp.client.lib.ui.{FieldSet => _, _}
import shipreq.webapp.client.protocol.ClientProtocol
import shipreq.webapp.client.util.{Refreshable, DND}
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

  val text_fields = FieldSet4[CustomField.Text](_.name, _.key.value, _.mandatory, _.reqTypes)(
                                               ("", "", Mandatory.Not, ISubset.All()))

  val tag_fields = FieldSet3[CustomField.Tag](_.tagId.some, _.mandatory, _.reqTypes)(
                                             (None, Mandatory.Not, ISubset.All()))

  val text_stores = NewAndSavedStores.fields(text_fields).keyedBy[CustomField.Id]
  val tag_stores  = NewAndSavedStores.fields(tag_fields ).keyedBy[CustomField.Id]

  /** The type of options in the combobox, from which users can create new fields. */
  type NewSelType = StaticField \/ CustomFieldType

  case class State(showDeleted     : Boolean,
                   text_state      : text_stores.State,
                   tag_state       : tag_stores.State,
                   newFieldTypeSel : NewSelType,
                   appReqTypeStates: AppReqTypesEditor.S,
                   dnd             : DND.Parent.PState[Field]) {

    lazy val customFields =
      customFieldStores.foldLeft(CustomField.IdAccess.emptyIMap)(_ ++ _.s.getAllP(this))

    lazy val tagFieldTags =
      tag_stores.s.getAllP(tag_state).map(_.tagId).toSet
  }

  object State {
    private[this] def l = Lenser[State]
    val _showDeleted      = l(_.showDeleted)
    val _text_state       = l(_.text_state)
    val _tag_state        = l(_.tag_state)
    val _newFieldTypeSel  = l(_.newFieldTypeSel)
    val _appReqTypeStates = l(_.appReqTypeStates)
    val _dnd              = l(_.dnd)

    @inline final def _appReqTypeState(k: Field.Id) =
      _appReqTypeStates ^|-> AppReqTypesEditor._stateFor(k)
  }

  type S  = State
  type ST = ReactST[IO, S, Unit]
  val  ST = ReactS.FixT[IO, S]

  val text_storesS = text_stores.contramap(State._text_state)
  val tag_storesS  = tag_stores .contramap(State._tag_state)

  def storesForType(t: CustomFieldType): NewAndSavedStores[S, CustomField.Id, _ <: CustomField, _] =
    t match {
      case CustomFieldType.Text => text_storesS
      case CustomFieldType.Tag  => tag_storesS
    }

  val customFieldStores = CustomFieldType.values.list map storesForType toStream

  def initialState(p: Props): S = {
    val textFields = Seq.newBuilder[CustomField.Text]
    val tagFields  = Seq.newBuilder[CustomField.Tag]
    val fs         = p.clientData.project.fields.data
    fs.customFields.values.foreach {
      case f: CustomField.Text => textFields += f
      case f: CustomField.Tag  => tagFields  += f
    }
    State(
      showDeleted     = p.showDeleted,
      text_state      = text_stores.initState(_.initStateS(textFields.result(), _.id)),
      tag_state       = tag_stores .initState(_.initStateS(tagFields .result(), _.id)),
      newFieldTypeSel = \/-(CustomFieldType.Text),
      AppReqTypesEditor initialState fs,
      DND.Parent.initialState)
  }

  val fieldDeltaListener = new DeltaListener.OneByOne[S, Field.Id, Delta](
      (s, i) => {
        val s2 = i match {
          case _: StaticField => s
          case j: CustomField.Id => customFieldStores.foldLeft(s)((t, f) => f.s.remove(j)(t))
        }
        clearAppReqTypesEditorState(i)(s2)
      },
      (s, i, d) => {
        val s2 = d match {
          case Delta(-\/(_: StaticField     ), _) => s
          case Delta(\/-(f: CustomField.Text), _) => text_storesS.s.set(f.id, f)(s)
          case Delta(\/-(f: CustomField.Tag ), _) => tag_storesS .s.set(f.id, f)(s)
        }
        clearAppReqTypesEditorState(i)(s2)
      })

  def clearAppReqTypesEditorState(id: Field.Id): S => S =
    State._appReqTypeState(id).set(Maybe.empty)

  val Component =
    ReactComponentB[Props]("Cfg: Fields")
      .getInitialState(initialState)
      .backend(new Backend(_))
      .render(_.backend.dyn.value.render)
      .configure(
        DeltaListener.apply  [Props, S, Backend](_.clientData, fieldDeltaListener.handler(Partition.Fields)) compose
        DeltaListener.refresh[Props, S, Backend](_.clientData, _.backend.dyn.refresh(()))(
          Partition.CustomReqTypes, // Refreshes AppReqTypesEditor
          Partition.Tags          ) // Refreshes TagSelector
      )
      .build

  def validatorStateS(s: S, k: Option[CustomField.Id]): V.S = {
    val customFieldStream = customFieldStores.flatMap(_.s.getAllP(s))
    (customFieldStream, k)
  }

  val rowIdFromEditorInput: ((V.S, Any)) => Option[CustomField.Id] = _._1._2

  // ===================================================================================================================
  final class Backend(val $: BackendScope[Props, S]) extends OnUnmount {

    lazy val dyn = Refreshable.thunk(new DynBackend(this, $.props.clientData.project)).asVar

    val nameE      = Editors.textInputEditor.applyValidator(V.nameS)
    val refkeyE    = Editors.textInputEditor.applyValidator(V.keyS)
    val mandatoryE = Editors.checkboxEditor.imap(Mandatory).strengthL[V.S]

    object protocol {
      import FieldProtocol._, CfgAction._
      val remote = $.props.remote
      val cp     = $.props.cp

      private def call(a: CfgAction): (SuccessIO, FailureIO) => IO[Unit] =
        (s, f) => cp.call(remote)(a, $.props.clientData.update(_) >> s.io, f)

      def createIO(v: FieldProtocol.Values) =
        call(Create(v))

      def updateValuesIO(i: CustomField.Id, v: FieldProtocol.Values) =
        call(UpdateValues(i, v))

      def updateOrderIO(i: Field.Id, p: Position) =
        call(UpdateOrder(i, p))

      def deleteIO(i: Field.Id, a: DeletionAction) =
        call(Delete(i, a))
    }

    // TODO staticDeletion doesn't handle failure (or lock row)
    val staticDeletion = new Deletion[StaticField](
      protocol.deleteIO(_, _)(SuccessIO.nop, FailureIO.nop))

    def validatorState(k: Option[CustomField.Id]): S => V.S =
      validatorStateS(_, k)

    val dndState = $.focusStateL(State._dnd)

    val headerRow = CfgTable.header(List(
      FieldNames.dndDragHandleHeader,
      FieldNames.name,
      FieldNames.fieldType,
      FieldNames.fieldRefKey,
      FieldNames.mandatory,
      FieldNames.applicableReqTypes))

    def fieldOrder = $.props.clientData.project.fields.data.order
  }

  // ===================================================================================================================
  // Certain vals here depend on parts of Project beyond .fields

  final class DynBackend(backend: Backend, project: Project) {
    import backend.{dyn => _, _}

    val appReqTypesEditor = new AppReqTypesEditor(project.customReqTypes.data.values)
    val tagSelector       = new TagSelector(project.tags.data)

    val reqtypesE  = appReqTypesEditor.editor($ focusStateL State._appReqTypeStates).cmapA[(V.S, ApplicableReqTypes)](_.map1(_._2))
    val tagSelE    = tagSelector.editor.applyValidator(V.tagIdS)

    object newFieldControl {
      import SelectOne.Choice

      def name: NewSelType => String =
        _.fold(_.name, _.name)

      val Component = SelectInvoke.Component[NewSelType]("NewField")

      def apply() = {
        val s = $.state

        var choices = Vector.empty[Choice[NewSelType]]

        def addChoice(value: NewSelType, label: String): Unit =
          choices :+= Choice(
            value    = value,
            label    = label,
            disabled = false)

        // Add static fields
        val missingStaticFields: Set[StaticField] =
          (StaticField.values.list.toSet /: fieldOrder)((q, i) => i.foldId(q - _, _ => q))
        missingStaticFields.foreach(f =>
          addChoice(-\/(f), f.name))

        // Add custom field types
        val allowNewCustomFieldType: CustomFieldType => Boolean = {
          case CustomFieldType.Text => true
          case CustomFieldType.Tag  =>
            project.tags.data.values.toStream
              .filter(TagInTree.filterAlive)
              .exists(t => !s.tagFieldTags.contains(t.id))
        }
        CustomFieldType.values.foreach(t =>
          if (allowNewCustomFieldType(t))
            addChoice(\/-(t), t.name))

        def staticInvoke(f: StaticField): IO[Unit] =
          protocol.updateOrderIO(f, None)(SuccessIO.nop, FailureIO.nop) // TODO no failure handling

        def customInvoke(t: CustomFieldType): IO[Unit] =
          IO($ modStateIO storesForType(t).n.enableEdit).join

        def onInvoke: Option[IO[Unit]] =
          Some(s.newFieldTypeSel.fold(staticInvoke, customInvoke))

        Component(SelectInvoke.Props(
            SelectOne.Props(
              s.newFieldTypeSel,
              choices.sortBy(_.label),
              Some($ _setStateL State._newFieldTypeSel)
            ),
            onInvoke, UiText.Cfg.startNewButton,
            customFieldStores.exists(_.n.editing(s))))
      }

      val abortNew: S => S =
        customFieldStores.map(_.n.remove).reduce(_ compose _)

      val abortButton =
        UI.abortNewButton($ modStateIO abortNew)
    }

    def render =
      <.div(
        newFieldControl(),
        ShowDeletedToggler($.state.showDeleted, $ runState ST.modT(State._showDeleted.modify(b => !b))),
        <.table(
          headerRow,
          <.tbody(renderNewField, renderFields)
        ))

    def renderNewField: Option[ReactElement] =
      customFieldRenderers.map(_ renderNewO $.state).flatMap(_.toStream).headOption

    def renderFields: TagMod = {
      var content = fieldOrder.toStream
        .flatMap(_.foldId[Stream[Field]](s => Stream(s), $.state.customFields.get(_).toStream))
      if (!$.state.showDeleted)
        content = content.filter(Field.filterAlive)
      content.toReactNodeArray(renderField)
    }

    // TODO orderIO doesn't handle failure (or lock row)
    def orderIO(from: Field, to: Field): IO[Unit] = {
      // TODO performance: .toList.toVector -> position
      val id       = from.fieldId
      val newOrder = DND.move(id, to.fieldId)(fieldOrder.toList).toVector
      val pos      = Util.position(newOrder, id)
      protocol.updateOrderIO(id, pos)(SuccessIO.nop, FailureIO.nop)
    }

    val renderField: Field => ReactElement = {
      implicit val fieldEquivalence = Equal.equalBy((_: Field).fieldId)
      f => DraggableFieldRow.set(key = f.fold[JsAny](_.name, _.id.value))(DND.Parent.cProps2(dndState, f, orderIO))
    }

    val DraggableFieldRow = DND.Child.dndItemComponent[Field]((outerAttr, dragHandle, f) =>
      renderField2(f, dragHandle)(outerAttr))

    def renderField2(gf: Field, dragHandle: ReactTag): ReactTag = gf match {
      case f: CustomField => rendererForType(f.fieldType).render($.state, dragHandle, f.id)
      case s: StaticField => renderStaticField(s, dragHandle)
    }

    def renderStaticField(f: StaticField, dragHandle: ReactTag): ReactTag =
      renderRow(RowStatus.Sync)(
        dragHandle = dragHandle,
        name       = f.name,
        refkey     = renderKeyO(f.keyO),
        mandatory  = Editors.staticCheckbox(Mandatory from f.mandatory),
        reqtypes   = appReqTypesEditor.renderReadOnly(f.reqTypes),
        ctrls      = (f.deletable ≟ Deletable) ?= staticDeletion.button(f, SoftDel)
      )(f.fieldType)

    def renderKeyO(k: Option[FieldRefKey]): TagMod =
      k.fold("-")(_.value)

    def renderRow(rs: RowStatus)(dragHandle: UndefOr[ReactTag], name: TagMod, refkey: TagMod, mandatory: TagMod,
                                 reqtypes: TagMod, ctrls: => TagMod)(implicit ftype: FieldType): ReactTag =
      <.tr(^.cls := UI.rowStatusRowClass(rs),
        <.td(^.cls := "dndh", dragHandle),
        <.td(^.cls := "name", name),
        <.td(ftype.name),
        <.td(^.cls := "key", refkey),
        <.td(mandatory),
        <.td(reqtypes),
        <.td(UI.rowStatusCtrls(rs, ctrls)))

    // -----------------------------------------------------------------------------------------------------------------
    // Subtype

    val unusedField: ReactNode = "-"

    abstract class SubtypeRenderer[T <: CustomField, I, B, D, V](
      final val editor: Editor[(V.S, I), B, IO, S, D, IO[Unit], V],
      final val stores: NewAndSavedStores[S, CustomField.Id, T, I]) {

      val editable = editor.editableByRowStatus($)

      val deletion = Persistence.asyncDeletionS(stores.s)(protocol.deleteIO, $ runState _)

      def fieldType: CustomFieldType
      @inline final implicit def ifieldType: FieldType = fieldType

      def ei(s: S, r: stores.s.Row): editor.Input = {
        val a = (validatorState(r.p.id.some)(s), r.i)
        EditorI(a, "", editable(r.status))
      }

      def ei(s: S, r: stores.n.Row): editor.Input = {
        val a = (validatorState(None)(s), r.i)
        EditorI(a, "", editable(r.status))
      }

      def renderNew  (s: S, r: stores.n.Row): ReactElement
      def renderAlive(s: S, dragHandle: ReactTag, r: stores.s.Row): ReactTag
      def renderDead (s: S, dragHandle: ReactTag, rs: RowStatus, t: T): ReactTag

      def renderNewRow(rs: RowStatus)(name: TagMod, refkey: TagMod, mandatory: TagMod, reqtypes: TagMod): ReactElement = {
        val r = renderRow(rs)(undefined, name, refkey, mandatory, reqtypes, newFieldControl.abortButton)
        r(^.cls := "new")
      }

      def render(s: S, dragHandle: ReactTag, id: CustomField.Id): ReactTag = {
        val row = stores.s.get(id)(s)
        val tag = row.p
        tag.alive match {
          case Alive => renderAlive(s, dragHandle, row)
          case Dead  => renderDead (s, dragHandle, row.status, tag)(^.cls := "dead")
        }
      }

      def renderNewO(s: S): Option[ReactElement] =
        stores.n.get(s).map(renderNew(s, _))

    } // SubtypeRenderer

    // -----------------------------------------------------------------------------------------------------------------
    // Text field

    val text_editor = {
      @inline def stores = text_storesS
      val toValues  = FieldProtocol.TextFieldValues.apply _
      val toValuesT = toValues.tupled
      val validator = V.textField map toValuesT
      val saveFn    = Persistence.asyncSaveNS2(validator, stores, protocol.createIO)(protocol.updateValuesIO,
        SaveNeed.cmpToExtract(t => toValues(t.name, t.key, t.mandatory, t.reqTypes)),
        _.id, validatorState, $ runState _)
      Editor.merge4S(text_fields, nameE, refkeyE, mandatoryE, reqtypesE).tupleI.zoomU[S]
        .applyRowUpdateAndRevert(stores)(rowIdFromEditorInput)
        .applyOnEditFinishedK(saveFn)(rowIdFromEditorInput)
    }

    val text_renderer = new SubtypeRenderer(text_editor, text_storesS) {
      override def fieldType = CustomFieldType.Text

      override def renderNew(s: S, row: stores.n.Row): ReactElement = {
        val (name, refkey, mandatory, reqtypes) = editor render ei(s, row)
        renderNewRow(row.status)(
          name      = name,
          refkey    = refkey,
          mandatory = mandatory,
          reqtypes  = reqtypes)
      }

      override def renderAlive(s: S, dragHandle: ReactTag, row: stores.s.Row): ReactTag = {
        val (name, refkey, mandatory, reqtypes) = editor render ei(s, row)
        val f = row.p
        renderRow(row.status)(
          dragHandle = dragHandle,
          name       = name,
          refkey     = refkey,
          mandatory  = mandatory,
          reqtypes   = reqtypes,
          ctrls      = deletion.button(f.id, SoftDel))
      }

      override def renderDead(s: S, dragHandle: ReactTag, rs: RowStatus, f: CustomField.Text): ReactTag =
        renderRow(rs)(
          dragHandle = dragHandle,
          name       = f.name,
          refkey     = f.key.value,
          mandatory  = Editors.staticCheckbox(Mandatory from f.mandatory),
          reqtypes   = appReqTypesEditor.renderReadOnly(f.reqTypes),
          ctrls      = deletion.button(f.id, Restore))
    }

    // -----------------------------------------------------------------------------------------------------------------
    // Tag field

    val tag_editor = {
      @inline def stores = tag_storesS
      val toValues  = FieldProtocol.TagFieldValues.apply _
      val toValuesT = toValues.tupled
      val validator = V.tagField map toValuesT
      val saveFn    = Persistence.asyncSaveNS2(validator, stores, protocol.createIO)(protocol.updateValuesIO,
        SaveNeed.cmpToExtract(t => toValues(t.tagId, t.mandatory, t.reqTypes)),
        _.id, validatorState, $ runState _)
      Editor.merge3S(tag_fields, tagSelE, mandatoryE, reqtypesE).tupleI.zoomU[S]
        .applyRowUpdateAndRevert(stores)(rowIdFromEditorInput)
        .applyOnEditFinishedK(saveFn)(rowIdFromEditorInput)
    }

    val tag_renderer = new SubtypeRenderer(tag_editor, tag_storesS) {
      override def fieldType = CustomFieldType.Tag

      override def renderNew(s: S, row: stores.n.Row): ReactElement = {
        val (name, mandatory, reqtypes) = editor render ei(s, row)
        renderNewRow(row.status)(
          name      = name,
          refkey    = unusedField,
          mandatory = mandatory,
          reqtypes  = reqtypes)
      }

      override def renderAlive(s: S, dragHandle: ReactTag, row: stores.s.Row): ReactTag = {
        val (name, mandatory, reqtypes) = editor render ei(s, row)
        val f = row.p
        renderRow(row.status)(
          dragHandle = dragHandle,
          name       = name,
          refkey     = unusedField,
          mandatory  = mandatory,
          reqtypes   = reqtypes,
          ctrls      = deletion.button(f.id, SoftDel)
        )
      }

      override def renderDead(s: S, dragHandle: ReactTag, rs: RowStatus, f: CustomField.Tag): ReactTag =
        renderRow(rs)(
          dragHandle = dragHandle,
          name       = f.name(project.tags.data),
          refkey     = unusedField,
          mandatory  = Editors.staticCheckbox(Mandatory from f.mandatory),
          reqtypes   = appReqTypesEditor.renderReadOnly(f.reqTypes),
          ctrls      = deletion.button(f.id, Restore)
        )
    }

    // -----------------------------------------------------------------------------------------------------------------

    def rendererForType(t: CustomFieldType): SubtypeRenderer[_ <: CustomField, _, _, _, _] =
      t match {
        case CustomFieldType.Text => text_renderer
        case CustomFieldType.Tag  => tag_renderer
      }
    val customFieldRenderers = CustomFieldType.values.list map rendererForType toStream
  }
}
