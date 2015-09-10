package shipreq.webapp.client.app.ui.cfg.fields

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._, MonocleReact._
import japgolly.scalajs.react.extra._
import monocle.macros.Lenses
import scala.language.reflectiveCalls
import scalajs.js.{undefined, UndefOr, Any => JsAny}
import scalaz.{Equal, -\/, \/-, \/}
import scalaz.syntax.bind.ToBindOps
import scalaz.syntax.equal._

import shipreq.base.util._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.Validators.{field => V}
import shipreq.webapp.base.event.{DeletionAction, HardDel, SoftDel, Restore}
import shipreq.webapp.base.protocol.FieldCrud
import shipreq.webapp.base.UiText, UiText.FieldNames
import shipreq.webapp.client.app.state.{ClientData, ChangeListener}
import shipreq.webapp.client.app.ui._
import shipreq.webapp.client.data.DataReusability._
import shipreq.webapp.client.lib.{FilterDead, ConsoleCB, TCB}
import shipreq.webapp.client.lib.ui.{FieldSet => _, _}
import shipreq.webapp.client.protocol.ClientProtocol
import shipreq.webapp.client.util.{Disabled, Enabled, DND, On}
import Field.ApplicableReqTypes

object CfgFields {
  case class Props(cp: ClientProtocol, remote: FieldCrud.Fn.Instance, clientData: ClientData, filterDead: FilterDead) {
    def component: ReactComponentU_ = MainTable.Component(this)
  }
  implicit val reusability = Reusability.caseClass[Props]
}

import CfgFields.Props

private[fields] object MainTable {

  val text_fields = FieldSet4[CustomField.Text](_.name, _.key.value, _.mandatory, _.reqTypes)(
                                               ("", "", Mandatory.Not, ISubset.All()))

  val tag_fields = FieldSet3[CustomField.Tag](_.tagId.some, _.mandatory, _.reqTypes)(
                                             (None, Mandatory.Not, ISubset.All()))

  val impl_fields = FieldSet3[CustomField.Implication](_.reqTypeId.some, _.mandatory, _.reqTypes)(
                                                      (None, Mandatory.Not, ISubset.All()))

  val text_stores = NewAndSavedStores.fields(text_fields).keyedBy[CustomFieldId]
  val impl_stores = NewAndSavedStores.fields(impl_fields).keyedBy[CustomFieldId]
  val tag_stores  = NewAndSavedStores.fields(tag_fields ).keyedBy[CustomFieldId]

  /** The type of options in the combobox, from which users can create new fields. */
  type NewSelType = StaticField \/ CustomFieldType

  @Lenses
  case class State(filterDead      : FilterDead,
                   text_state      : text_stores.State,
                   impl_state      : impl_stores.State,
                   tag_state       : tag_stores.State,
                   newFieldTypeSel : NewSelType,
                   appReqTypeStates: AppReqTypesEditor.S,
                   dnd             : DND.Parent.PState[Field]) {

    lazy val customFields =
      customFieldStores.foldLeft(CustomField.IdAccess.emptyIMap)(_ ++ _.s.getAllP(this))

    lazy val tagFieldTagIds =
      tag_stores.s.getAllP(tag_state).map(_.tagId).toSet

    lazy val implFieldReqTypeIds =
      impl_stores.s.getAllP(impl_state).map(_.reqTypeId).toSet
  }

  object State {
    @inline final def _appReqTypeState(k: FieldId) =
      appReqTypeStates ^|-> AppReqTypesEditor.stateFor(k)
  }

  type S  = State
  type ST = ReactST[CallbackTo, S, Unit]
  val  ST = ReactS.FixCB[S]

  val text_storesS = text_stores.contramap(State.text_state)
  val impl_storesS = impl_stores.contramap(State.impl_state)
  val tag_storesS  = tag_stores .contramap(State.tag_state)

  def storesForType(t: CustomFieldType): NewAndSavedStores[S, CustomFieldId, _ <: CustomField, _] =
    t match {
      case CustomFieldType.Text        => text_storesS
      case CustomFieldType.Implication => impl_storesS
      case CustomFieldType.Tag         => tag_storesS
    }

  val customFieldStores = CustomFieldType.values.toStream map storesForType

  def initialState(p: Props): S = {
    val textFields = Seq.newBuilder[CustomField.Text]
    val implFields = Seq.newBuilder[CustomField.Implication]
    val tagFields  = Seq.newBuilder[CustomField.Tag]
    val fs         = p.clientData.project.config.fields
    fs.customFields.values.foreach {
      case f: CustomField.Text        => textFields += f
      case f: CustomField.Implication => implFields += f
      case f: CustomField.Tag         => tagFields  += f
    }
    State(
      filterDead      = p.filterDead,
      text_state      = text_stores.initState(_.initStateS(textFields.result(), _.id)),
      impl_state      = impl_stores.initState(_.initStateS(implFields.result(), _.id)),
      tag_state       = tag_stores .initState(_.initStateS(tagFields .result(), _.id)),
      newFieldTypeSel = \/-(CustomFieldType.Text),
      AppReqTypesEditor initialState fs,
      DND.Parent.initialState)
  }

  val customFieldChangeListener = ChangeListener.oneByOne[S, CustomFieldId, CustomField](
      _.customFieldTypes, _.config.fields.customFields.get)(
      (s, i) => {
        val s2 = customFieldStores.foldLeft(s)((t, f) => f.s.remove(i)(t))
        clearAppReqTypesEditorState(i)(s2)
      },
      (s, i, d) => {
        val s2 = d match {
          case f: CustomField.Text        => text_storesS.s.set(f.id, f)(s)
          case f: CustomField.Implication => impl_storesS.s.set(f.id, f)(s)
          case f: CustomField.Tag         => tag_storesS .s.set(f.id, f)(s)
        }
        clearAppReqTypesEditorState(i)(s2)
      })

  def clearAppReqTypesEditorState(id: FieldId): S => S =
    State._appReqTypeState(id).set(None)

  val Component =
    ReactComponentB[Props]("Cfg: Fields")
      .initialState_P(initialState)
      .renderBackend[Backend]
      .configure(
        customFieldChangeListener.install(_.clientData),
        ChangeListener.refreshWhen(c =>
          c.fieldOrder
          || c.staticFields // TODO should this trigger a clearAppReqTypesEditorState(i)?
          || c.customReqTypes.nonEmpty // Refreshes AppReqTypesEditor and reqTypeSelector
          || c.tags.nonEmpty)          // Refreshes tagSelector
          .install(_.clientData)
      )
      .build

  def validatorStateS(s: S, k: Option[CustomFieldId]): V.S = {
    val customFieldStream = customFieldStores.flatMap(_.s.getAllP(s))
    (customFieldStream, k)
  }

  val rowIdFromEditorInput: ((V.S, Any)) => Option[CustomFieldId] = _._1._2

  val onWhenMandatory = On <=> Mandatory

  def staticMandatoryCheckbox(m: Mandatory) =
    Editors staticCheckbox (onWhenMandatory from m)

  // ===================================================================================================================
  final class Backend(val $: BackendScope[Props, S]) extends OnUnmount {

    val projectPx = Px.bs($).propsA(_.clientData.project)
    val projectBackend = projectPx.map(new ProjectBackend(this, _))
    val protocol = Px.bs($).propsA.map(p => ProtocolBackend(p.cp, p.remote, p.clientData))

    val nameE      = Editors.textInputEditor.applyValidator(V.nameS)
    val refkeyE    = Editors.textInputEditor.applyValidator(V.keyS)
    val mandatoryE = Editors.checkboxEditor.imap(onWhenMandatory).strengthL[V.S]

    def render(s: S): ReactElement =
      projectBackend.value().render(s)

    def validatorState(k: Option[CustomFieldId]): S => V.S =
      validatorStateS(_, k)

    val dndState = $.zoomL(State.dnd)

    val headerRow = CfgTable.header(List(
      FieldNames.dndDragHandleHeader,
      FieldNames.name,
      FieldNames.fieldType,
      FieldNames.fieldRefKey,
      FieldNames.mandatory,
      FieldNames.applicableReqTypes))

  }

  // ===================================================================================================================
  case class ProtocolBackend(cp: ClientProtocol, remote: FieldCrud.Fn.Instance, cd: ClientData) {
    import FieldCrud._, CfgAction._

    private def call(a: CfgAction): (TCB.Success, TCB.Failure) => Callback =
      (s, f) => cp.call(remote)(a,
        s << cd.applyEvents(_),
        cp.consumeGenericFailure(_) >> f)

    def createIO(v: Values) =
      call(Create(v))

    def updateValuesIO(i: CustomFieldId, v: Values) =
      call(UpdateValues(i, v))

    def updateOrderIO(i: FieldId, p: Position) =
      call(UpdateOrder(i, p))

    def deleteIO(i: FieldId, a: DeletionAction) =
      call(Delete(i, a))

    // TODO staticDeletion doesn't handle failure (or lock row)
    val staticDeletion = new Deletion[StaticField](
      deleteIO(_, _)(TCB.Success.nop, TCB.Failure.nop))
  }
  // implicit val protocolBackendReusability = Reusability.caseClass[ProtocolBackend]

  // ===================================================================================================================
  final class ProjectBackend(backend: Backend, project: Project) {
    import backend.{projectBackend => _, _}

    val fieldOrder = project.config.fields.order

    val appReqTypesEditor = new AppReqTypesEditor(project.config.customReqTypes.values)
    val tagSelector       = SelectOneStartNone.tag(project.config.tags)
    val reqTypeSelector   = SelectOneStartNone.reqType(project.config.reqTypes)

    val reqtypesE = appReqTypesEditor.editor($ zoomL State.appReqTypeStates)
                      .cmapA[(V.S, ApplicableReqTypes)](_.map1(_._2))

    object newFieldControl {
      import SelectOne.Choice

      def name: NewSelType => String =
        _.fold(_.name, _.name)

      val Component = SelectInvoke.Component[NewSelType]("NewField")

      def apply(s: S) = {
        def choice(value: NewSelType, label: String): Choice[NewSelType] =
          Choice(value, label, Enabled)

        def customFieldChoice(t: CustomFieldType) =
          choice(\/-(t), t.name)

        var choices = NonEmptyVector one customFieldChoice(CustomFieldType.Text)

        @inline def add(choice: Choice[NewSelType]): Unit =
          choices :+= choice

        // Add static fields
        val missingStaticFields: Set[StaticField] =
          (StaticField.values.whole.toSet /: fieldOrder)((q, i) => i.foldId(q - _, _ => q))
        for (f <- missingStaticFields)
          add(choice(-\/(f), f.name))

        // Add custom field types
        val allowNewCustomFieldType: CustomFieldType => Boolean = {
          case CustomFieldType.Text        => false // Already added as proof of non-emptyness
          case CustomFieldType.Tag         => project.config.tags.values.toStream
                                                .filter(TagInTree.filterLive)
                                                .exists(t => !s.tagFieldTagIds.contains(t.id))
          case CustomFieldType.Implication => project.config.reqTypes
                                                .filter(_.live :: Live)
                                                .exists(r => !s.implFieldReqTypeIds.contains(r.reqTypeId))
        }
        for (t <- CustomFieldType.values.whole if allowNewCustomFieldType(t))
          add(customFieldChoice(t))

        def staticInvoke(f: StaticField): Callback =
          Callback.byName(
            protocol.value().updateOrderIO(f, None)(TCB.Success.nop, TCB.Failure.nop)) // TODO no failure handling

        def customInvoke(t: CustomFieldType): Callback =
          Callback.byName($ modState storesForType(t).n.enableEdit)

        def onInvoke: Option[Callback] =
          Some(s.newFieldTypeSel.fold(staticInvoke, customInvoke))

        Component(SelectInvoke.Props(
            SelectOne.Props(
              s.newFieldTypeSel,
              choices.sortBy(_.label),
              Some($ _setStateL State.newFieldTypeSel)
            ),
            onInvoke, UiText.Cfg.startNewButton,
            Disabled <~ customFieldStores.exists(_.n.editing(s))))
      }

      val abortNew: S => S =
        customFieldStores.map(_.n.remove).reduce(_ compose _)

      val abortButton =
        UI.abortNewButton($ modState abortNew)
    }

    val filterDeadCheckbox = Checkbox.filterDead_$($ zoomL State.filterDead)

    def render(s: S) =
      <.div(
        newFieldControl(s),
        filterDeadCheckbox(),
        <.table(
          headerRow,
          <.tbody(renderNewField(s), renderFields(s))
        ))

    def renderNewField(s: S): Option[ReactElement] =
      customFieldRenderers.map(_ renderNewO s).flatMap(_.toStream).headOption

    def renderFields(s: S): TagMod = {
      var content = fieldOrder.toStream
        .flatMap(_.foldId[Stream[Field]](s => Stream(s), s.customFields.get(_).toStream))
      content = s.filterDead(content)(_.live)
      content.toReactNodeArray(renderField)
    }

    // TODO orderIO doesn't handle failure (or lock row)
    def orderIO(from: Field, to: Field): Callback = {
      val id       = from.fieldId
      val newOrder = DND.moveE(id, to.fieldId)(fieldOrder)
      val pos      = Position.get(newOrder, id)
      protocol.value().updateOrderIO(id, pos)(TCB.Success.nop, TCB.Failure.nop)
    }

    val renderField: Field => ReactElement = {
      implicit val fieldEquivalence = Equal.equalBy((_: Field).fieldId)
      f => DraggableFieldRow.set(key = f.fold[JsAny](_.name, _.id.value))(DND.Parent.cProps2(dndState, f, orderIO))
    }

    val DraggableFieldRow = DND.Child.dndItemComponent[Field]((outerAttr, dragHandle, f) =>
      renderField2(f, dragHandle)(outerAttr))

    def renderField2(gf: Field, dragHandle: ReactTag): ReactTag = gf match {
      case f: CustomField => rendererForType(f.fieldType).render($.state.runNow(), dragHandle, f.id)
      case s: StaticField => renderStaticField(s, dragHandle)
    }

    def renderStaticField(f: StaticField, dragHandle: ReactTag): ReactTag =
      renderRow(RowStatus.Sync)(
        dragHandle = dragHandle,
        name       = f.name,
        refkey     = renderKeyO(f.keyO),
        mandatory  = staticMandatoryCheckbox(f.mandatory),
        reqtypes   = appReqTypesEditor.renderReadOnly(f.reqTypes),
        ctrls      = (f.deletable ≟ Deletable) ?= protocol.value().staticDeletion.button(f, SoftDel)
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
      final val editor: Editor[(V.S, I), B, CallbackTo, S, D, Callback, V],
      final val stores: NewAndSavedStores[S, CustomFieldId, T, I]) {

      val editable = editor.editableByRowStatus($)

      val _deletion = protocol.map(p => Persistence.asyncDeletionS(stores.s)(p.deleteIO, $ runState _))
      def deletion = _deletion.value()

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

      def renderNew (s: S, r: stores.n.Row): ReactElement
      def renderLive(s: S, dragHandle: ReactTag, r: stores.s.Row): ReactTag
      def renderDead(s: S, dragHandle: ReactTag, rs: RowStatus, t: T): ReactTag

      def renderNewRow(rs: RowStatus)(name: TagMod, refkey: TagMod, mandatory: TagMod, reqtypes: TagMod): ReactElement = {
        val r = renderRow(rs)(undefined, name, refkey, mandatory, reqtypes, newFieldControl.abortButton)
        r(^.cls := "new")
      }

      def render(s: S, dragHandle: ReactTag, id: CustomFieldId): ReactTag = {
        val row = stores.s.get(id)(s)
        val tag = row.p
        tag.live match {
          case Live => renderLive(s, dragHandle, row)
          case Dead => renderDead(s, dragHandle, row.status, tag)(^.cls := "dead")
        }
      }

      def renderNewO(s: S): Option[ReactElement] =
        stores.n.get(s).map(renderNew(s, _))

    } // SubtypeRenderer

    // -----------------------------------------------------------------------------------------------------------------
    // Text field

    val text_editor = {
      @inline def stores = text_storesS
      val toValues  = FieldCrud.TextFieldValues.apply _
      val toValuesT = toValues.tupled
      val validator = V.textField map toValuesT
      val saveFn    =
        protocol.map(p =>
          Persistence.asyncSaveNS2(validator, stores, p.createIO)(p.updateValuesIO,
          SaveNeed.cmpToExtract(t => toValues(t.name, t.key, t.mandatory, t.reqTypes)),
          _.id, validatorState, $ runState _)
        ).extract
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

      override def renderLive(s: S, dragHandle: ReactTag, row: stores.s.Row): ReactTag = {
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
          mandatory  = staticMandatoryCheckbox(f.mandatory),
          reqtypes   = appReqTypesEditor.renderReadOnly(f.reqTypes),
          ctrls      = deletion.button(f.id, Restore))
    }

    // -----------------------------------------------------------------------------------------------------------------
    // Tag field

    val tag_editor = {
      @inline def stores = tag_storesS
      val tagSelE   = tagSelector.editor.applyValidator(V.tagField.tagIdS)
      val toValues  = FieldCrud.TagFieldValues.apply _
      val toValuesT = toValues.tupled
      val validator = V.tagField.all map toValuesT
      val saveFn    =
        protocol.map(p =>
          Persistence.asyncSaveNS2(validator, stores, p.createIO)(p.updateValuesIO,
          SaveNeed.cmpToExtract(t => toValues(t.tagId, t.mandatory, t.reqTypes)),
          _.id, validatorState, $ runState _)
        ).extract
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

      override def renderLive(s: S, dragHandle: ReactTag, row: stores.s.Row): ReactTag = {
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
          name       = UI mustA f.name(project.config.tags), // TODO is this a Must or an Issue?
          refkey     = unusedField,
          mandatory  = staticMandatoryCheckbox(f.mandatory),
          reqtypes   = appReqTypesEditor.renderReadOnly(f.reqTypes),
          ctrls      = deletion.button(f.id, Restore)
        )
    }

    // -----------------------------------------------------------------------------------------------------------------
    // Tag field

    val impl_editor = {
      @inline def stores = impl_storesS
      val reqTypeSelE = reqTypeSelector.editor.applyValidator(V.implField.reqTypeIdS)
      val toValues    = FieldCrud.ImplicationFieldValues.apply _
      val toValuesT   = toValues.tupled
      val validator   = V.implField.all map toValuesT
      val saveFn      =
        protocol.map(p =>
          Persistence.asyncSaveNS2(validator, stores, p.createIO)(p.updateValuesIO,
          SaveNeed.cmpToExtract(t => toValues(t.reqTypeId, t.mandatory, t.reqTypes)),
          _.id, validatorState, $ runState _)
        ).extract
      Editor.merge3S(impl_fields, reqTypeSelE, mandatoryE, reqtypesE).tupleI.zoomU[S]
        .applyRowUpdateAndRevert(stores)(rowIdFromEditorInput)
        .applyOnEditFinishedK(saveFn)(rowIdFromEditorInput)
    }

    val impl_renderer = new SubtypeRenderer(impl_editor, impl_storesS) {
      override def fieldType = CustomFieldType.Implication

      override def renderNew(s: S, row: stores.n.Row): ReactElement = {
        val (name, mandatory, reqtypes) = editor render ei(s, row)
        renderNewRow(row.status)(
          name      = name,
          refkey    = unusedField,
          mandatory = mandatory,
          reqtypes  = reqtypes)
      }

      override def renderLive(s: S, dragHandle: ReactTag, row: stores.s.Row): ReactTag = {
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

      override def renderDead(s: S, dragHandle: ReactTag, rs: RowStatus, f: CustomField.Implication): ReactTag =
        renderRow(rs)(
          dragHandle = dragHandle,
          name       = UI mustA f.name(project.config.customReqTypes), // TODO is this a Must or an Issue?
          refkey     = unusedField,
          mandatory  = staticMandatoryCheckbox(f.mandatory),
          reqtypes   = appReqTypesEditor.renderReadOnly(f.reqTypes),
          ctrls      = deletion.button(f.id, Restore)
        )
    }

    // -----------------------------------------------------------------------------------------------------------------

    def rendererForType(t: CustomFieldType): SubtypeRenderer[_ <: CustomField, _, _, _, _] =
      t match {
        case CustomFieldType.Text        => text_renderer
        case CustomFieldType.Tag         => tag_renderer
        case CustomFieldType.Implication => impl_renderer
      }
    val customFieldRenderers = CustomFieldType.values.toStream map rendererForType
  }
}
