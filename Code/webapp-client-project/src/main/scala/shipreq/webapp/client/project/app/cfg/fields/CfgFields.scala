package shipreq.webapp.client.project.app.cfg.fields

import japgolly.microlibs.nonempty._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra._
import monocle.macros.Lenses
import scala.language.reflectiveCalls
import scalacss.ScalaCssReact._
import scalajs.js.{UndefOr, undefined}
import scalaz.{-\/, Equal, \/, \/-}
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.DataValidators.{field => V}
import shipreq.webapp.base.protocol.{FieldCrud, ServerSideProcInvoker}
import shipreq.webapp.base.event.VerifiedEvent
import shipreq.webapp.base.ui.BaseStyles
import shipreq.webapp.base.ui.semantic.Table
import shipreq.webapp.base.UiText
import shipreq.webapp.client.project.app.Style
import shipreq.webapp.client.project.app.cfg.shared.{FieldSet => _, _}
import shipreq.webapp.client.project.app.state.{ChangeListener, Global}
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.lib.DND
import shipreq.webapp.client.project.widgets._
import Field.ApplicableReqTypes
import UiText.FieldNames

object CfgFields {
  final case class Props(remote    : ServerSideProcInvoker[FieldCrud.CfgAction, ErrorMsg, VerifiedEvent.Seq],
                         global    : Global,
                         filterDead: StateSnapshot[FilterDead]) {

    def component = MainTable.Component(this)
  }

  implicit val reusability = Reusability.derive[Props]
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
  case class State(text_state      : text_stores.State,
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
    val fs         = p.global.unsafeProject().config.fields
    fs.customFields.values.foreach {
      case f: CustomField.Text        => textFields += f
      case f: CustomField.Implication => implFields += f
      case f: CustomField.Tag         => tagFields  += f
    }
    State(
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
    ScalaComponent.builder[Props]("Cfg: Fields")
      .initialStateFromProps(initialState)
      .renderBackend[Backend]
      .configure(customFieldChangeListener.install(_.global))
      .configure(
        ChangeListener.refreshWhen(c =>
          c.fieldOrder
          || c.staticFields // TODO should this trigger a clearAppReqTypesEditorState(i)?
          || c.customReqTypes.nonEmpty // Refreshes AppReqTypesEditor and reqTypeSelector
          || c.tags.nonEmpty)          // Refreshes tagSelector
          .install(_.global)
      )
      .build

  def validatorStateS(s: S, k: Option[CustomFieldId]): V.State = {
    lazy val data = customFieldStores.flatMap(_.s.getAllP(s))
    V.State(k, () => data)
  }

  val rowIdFromEditorInput: ((V.State, Any)) => Option[CustomFieldId] = _._1.subject

  val onWhenMandatory = On <=> Mandatory

  def staticMandatoryCheckbox(m: Mandatory) =
    Editors staticCheckbox (onWhenMandatory from m)

  // ===================================================================================================================
  final class Backend(val $: BackendScope[Props, S]) extends OnUnmount {

    val projectPx = Px.props($).map(_.global.unsafeProject()).withReuse.autoRefresh
    val projectBackend = projectPx.map(new ProjectBackend(this, _))
    val protocol = Px.props($).withReuse.autoRefresh.map(p => ProtocolBackend(p.remote))

    val nameE      = Editors.textInputEditor.applyStatefulValidator(V.name.unnamedFn)
    val refkeyE    = Editors.textInputEditor.applyStatefulValidator(V.key.unnamedFn)
    val mandatoryE = Editors.checkboxEditor.imap(onWhenMandatory).strengthL[V.State]

    def render(p: Props, s: S): VdomElement =
      projectBackend.value().render(p.filterDead.value, s)

    def validatorState(k: Option[CustomFieldId]): S => V.State =
      validatorStateS(_, k)

    val dndState = $.zoomStateL(State.dnd)

    val headerRow = CfgTable.header(List(
      FieldNames.dndDragHandleHeader,
      FieldNames.name,
      FieldNames.fieldType,
      FieldNames.fieldRefKey,
      FieldNames.mandatory,
      FieldNames.applicableReqTypes))
  }

  // ===================================================================================================================
  final case class ProtocolBackend(remote: ServerSideProcInvoker[FieldCrud.CfgAction, ErrorMsg, VerifiedEvent.Seq]) {
    import FieldCrud._

    private def call(a: CfgAction): (TCB.Success, TCB.Failure) => Callback =
      (s, f) => remote(a, _ => s, _ => f)

    def createIO(v: Values) =
      call(CfgAction.Create(v))

    def updateValuesIO(i: CustomFieldId, v: Values) =
      call(CfgAction.UpdateValues(i, v))

    def updateOrderIO(i: FieldId, p: Position) =
      call(CfgAction.UpdateOrder(i, p))

    def deleteIO(i: FieldId, a: DeletionAction) =
      call(a match {
        case Delete  => CfgAction.Delete(i)
        case Restore => CfgAction.Restore(i)
      })

    // TODO staticDeletion doesn't handle failure (or lock row)
    val staticDeletion = new Deletion[StaticField](
      deleteIO(_, _)(TCB.Success.nop, TCB.Failure.nop))
  }
  // implicit val protocolBackendReusability = Reusability.derive[ProtocolBackend]

  // ===================================================================================================================
  final class ProjectBackend(backend: Backend, project: Project) {
    import backend.{projectBackend => _, _}

    val fieldOrder = project.config.fields.order

    val appReqTypesEditor = new AppReqTypesEditor(project.config.reqTypes.custom.values)
    val tagSelector       = SelectOneStartNone.tag(project.config.tags)
    val reqTypeSelector   = SelectOneStartNone.reqType(project.config.reqTypes.all.whole)

    val reqtypesE = appReqTypesEditor.editor($ zoomStateL State.appReqTypeStates)
                      .cmapA[(V.State, ApplicableReqTypes)](_.map1(_.subject))

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
          case CustomFieldType.Tag         => project.config.tags.tree.values.toStream
                                                .filter(TagInTree.filterLive)
                                                .exists(t => !s.tagFieldTagIds.contains(t.id))
          case CustomFieldType.Implication => project.config.reqTypes.all.whole
                                                .filter(_.live is Live)
                                                .exists(r => !s.implFieldReqTypeIds.contains(r.reqTypeId))
        }
        for (t <- CustomFieldType.values.whole if allowNewCustomFieldType(t))
          add(customFieldChoice(t))

        def staticInvoke(f: StaticField): Callback =
          Callback.byName(
            protocol.value().deleteIO(f, Restore)(TCB.Success.nop, TCB.Failure.nop)) // TODO no failure handling

        def customInvoke(t: CustomFieldType): Callback =
          Callback.byName($ modState storesForType(t).n.enableEdit)

        def onInvoke: Option[Callback] =
          Some(s.newFieldTypeSel.fold(staticInvoke, customInvoke))

        Component(SelectInvoke.Props(
            SelectOne.Props(
              s.newFieldTypeSel,
              choices.sortBy(_.label),
              Some($ modState State.newFieldTypeSel.set(_))
            ),
            onInvoke, UiText.Cfg.startNewButton,
            Disabled when customFieldStores.exists(_.n.editing(s))))
      }

      val abortNew: S => S =
        customFieldStores.map(_.n.remove).reduce(_ compose _)

      val abortButton =
        abortNewButton($ modState abortNew)
    }

    def render(fd: FilterDead, s: S) =
      <.div(
        BaseStyles.containerFull, Style.cfg.fields,
        <.div(^.display.flex,
          <.div(^.flex := "1", newFieldControl(s)),
          <.div(FilterDeadButton.Component(StateSnapshot(fd)((v, cb) => $.props.flatMap(_.filterDead.setStateOption(v, cb)))))),
        Table.celledCompactUnstackable(
          headerRow,
          <.tbody(renderNewField(s).whenDefined, renderFields(fd, s))))

    def renderNewField(s: S): Option[VdomElement] =
      customFieldRenderers.map(_ renderNewO s).flatMap(_.toStream).headOption

    def renderFields(fd: FilterDead, s: S): TagMod = {
      var content = fieldOrder.toStream
        .flatMap(_.foldId[Stream[Field]](s => Stream(s), s.customFields.get(_).toStream))
      content = fd(content)(_ live project.config)
      content.toVdomArray(renderField)
    }

    // TODO orderIO doesn't handle failure (or lock row)
    def orderIO(from: Field, to: Field): Callback = {
      val id       = from.fieldId
      val newOrder = DND.moveE(id, to.fieldId)(fieldOrder)
      val pos      = RelPos.get(newOrder, id)
      protocol.value().updateOrderIO(id, pos)(TCB.Success.nop, TCB.Failure.nop)
    }

    val renderField: Field => VdomElement = {
      implicit val fieldEquivalence = Equal.equalBy((_: Field).fieldId)
      f => DraggableFieldRow.withKey(f.fold[Key](_.name, _.id.value))(DND.Parent.cProps2(dndState, f, orderIO))
    }

    val DraggableFieldRow = DND.Child.dndItemComponent[Field]((outerAttr, dragHandle, f) =>
      renderField2(f, dragHandle)(outerAttr))

    def renderField2(gf: Field, dragHandle: VdomTag): VdomTag = gf match {
      case f: CustomField => rendererForType(f.fieldType).render($.state.runNow(), dragHandle, f.id)
      case s: StaticField => renderStaticField(s, dragHandle)
    }

    def renderStaticField(f: StaticField, dragHandle: VdomTag): VdomTag =
      renderRow(RowStatus.Sync)(
        dragHandle = dragHandle,
        name       = f.name,
        refkey     = renderKeyO(f.keyO),
        mandatory  = staticMandatoryCheckbox(f.mandatory),
        reqtypes   = appReqTypesEditor.renderReadOnly(f.reqTypes),
        ctrls      = protocol.value().staticDeletion.button(f, Delete).when(f.deletable is Deletable)
      )(f.fieldType)

    def renderKeyO(k: Option[FieldRefKey]): TagMod =
      k.fold("-")(_.value)

    def renderRow(rs: RowStatus)(dragHandle: UndefOr[VdomTag], name: TagMod, refkey: TagMod, mandatory: TagMod,
                                 reqtypes: TagMod, ctrls: => TagMod)(implicit ftype: FieldType): VdomTag =
      <.tr(^.cls := rowStatusRowClass(rs),
        <.td(dragHandle.whenDefined),
        <.td(^.cls := "name", name),
        <.td(ftype.name),
        <.td(^.cls := "key", refkey),
        <.td(mandatory),
        <.td(reqtypes),
        <.td(rowStatusCtrls(rs, ctrls)))

    // -----------------------------------------------------------------------------------------------------------------
    // Subtype

    val unusedField: VdomNode = "-"

    abstract class SubtypeRenderer[T <: CustomField, I, B, D, V](
      final val editor: Editor[(V.State, I), B, CallbackTo, S, D, Callback, V],
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

      def renderNew (s: S, r: stores.n.Row): VdomElement
      def renderLive(s: S, dragHandle: VdomTag, r: stores.s.Row, deleteButton: TagMod): VdomTag
      def renderDead(s: S, dragHandle: VdomTag, rs: RowStatus, t: T, restoreButton: TagMod): VdomTag

      def renderNewRow(rs: RowStatus)(name: TagMod, refkey: TagMod, mandatory: TagMod, reqtypes: TagMod): VdomElement = {
        val r = renderRow(rs)(undefined, name, refkey, mandatory, reqtypes, newFieldControl.abortButton)
        r(^.cls := "new")
      }

      def render(s: S, dragHandle: VdomTag, id: CustomFieldId): VdomTag = {
        val row = stores.s.get(id)(s)
        val cf = row.p
        val cfg = project.config
        cf.live(cfg) match {
          case Live =>
            renderLive(s, dragHandle, row, deletion.button(cf.id, Delete))
          case Dead =>
            val restoreButton: TagMod =
              if (cf recoverable cfg)
                deletion.button(cf.id, Restore)
              else
                EmptyVdom
            renderDead(s, dragHandle, row.status, cf, restoreButton)(^.cls := "dead")
        }
      }

      def renderNewO(s: S): Option[VdomElement] =
        stores.n.get(s).map(renderNew(s, _))

    } // SubtypeRenderer

    // -----------------------------------------------------------------------------------------------------------------
    // Text field

    val text_editor = {
      @inline def stores = text_storesS
      val toValues  = FieldCrud.TextFieldValues.apply _
      val toValuesT = toValues.tupled
      val validator = V.textField.andThen(_ mapValid toValuesT)
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

      override def renderNew(s: S, row: stores.n.Row): VdomElement = {
        val (name, refkey, mandatory, reqtypes) = editor render ei(s, row)
        renderNewRow(row.status)(
          name      = name,
          refkey    = refkey,
          mandatory = mandatory,
          reqtypes  = reqtypes)
      }

      override def renderLive(s: S, dragHandle: VdomTag, row: stores.s.Row, deleteButton: TagMod): VdomTag = {
        val (name, refkey, mandatory, reqtypes) = editor render ei(s, row)
        val f = row.p
        renderRow(row.status)(
          dragHandle = dragHandle,
          name       = name,
          refkey     = refkey,
          mandatory  = mandatory,
          reqtypes   = reqtypes,
          ctrls      = deleteButton)
      }

      override def renderDead(s: S, dragHandle: VdomTag, rs: RowStatus, f: CustomField.Text, restoreButton: TagMod): VdomTag =
        renderRow(rs)(
          dragHandle = dragHandle,
          name       = f.name,
          refkey     = f.key.value,
          mandatory  = staticMandatoryCheckbox(f.mandatory),
          reqtypes   = appReqTypesEditor.renderReadOnly(f.reqTypes),
          ctrls      = restoreButton)
    }

    // -----------------------------------------------------------------------------------------------------------------
    // Tag field

    val tag_editor = {
      @inline def stores = tag_storesS
      val tagSelE   = tagSelector.editor.applyStatefulValidator(V.tagField.tagId.unnamedFn)
      val toValues  = FieldCrud.TagFieldValues.apply _
      val toValuesT = toValues.tupled
      val validator = V.tagField.all.andThen(_ mapValid toValuesT)
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

      override def renderNew(s: S, row: stores.n.Row): VdomElement = {
        val (name, mandatory, reqtypes) = editor render ei(s, row)
        renderNewRow(row.status)(
          name      = name,
          refkey    = unusedField,
          mandatory = mandatory,
          reqtypes  = reqtypes)
      }

      override def renderLive(s: S, dragHandle: VdomTag, row: stores.s.Row, deleteButton: TagMod): VdomTag = {
        val (name, mandatory, reqtypes) = editor render ei(s, row)
        val f = row.p
        renderRow(row.status)(
          dragHandle = dragHandle,
          name       = name,
          refkey     = unusedField,
          mandatory  = mandatory,
          reqtypes   = reqtypes,
          ctrls      = deleteButton)
      }

      override def renderDead(s: S, dragHandle: VdomTag, rs: RowStatus, f: CustomField.Tag, restoreButton: TagMod): VdomTag =
        renderRow(rs)(
          dragHandle = dragHandle,
          name       = f.name(project.config.tags.tree),
          refkey     = unusedField,
          mandatory  = staticMandatoryCheckbox(f.mandatory),
          reqtypes   = appReqTypesEditor.renderReadOnly(f.reqTypes),
          ctrls      = restoreButton)
    }

    // -----------------------------------------------------------------------------------------------------------------
    // Tag field

    val impl_editor = {
      @inline def stores = impl_storesS
      val reqTypeSelE = reqTypeSelector.editor.applyStatefulValidator(V.implField.reqTypeId.unnamedFn)
      val toValues    = FieldCrud.ImplicationFieldValues.apply _
      val toValuesT   = toValues.tupled
      val validator   = V.implField.all.andThen(_ mapValid toValuesT)
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

      override def renderNew(s: S, row: stores.n.Row): VdomElement = {
        val (name, mandatory, reqtypes) = editor render ei(s, row)
        renderNewRow(row.status)(
          name      = name,
          refkey    = unusedField,
          mandatory = mandatory,
          reqtypes  = reqtypes)
      }

      override def renderLive(s: S, dragHandle: VdomTag, row: stores.s.Row, deleteButton: TagMod): VdomTag = {
        val (name, mandatory, reqtypes) = editor render ei(s, row)
        val f = row.p
        renderRow(row.status)(
          dragHandle = dragHandle,
          name       = name,
          refkey     = unusedField,
          mandatory  = mandatory,
          reqtypes   = reqtypes,
          ctrls      = deleteButton)
      }

      override def renderDead(s: S, dragHandle: VdomTag, rs: RowStatus, f: CustomField.Implication, restoreButton: TagMod): VdomTag =
        renderRow(rs)(
          dragHandle = dragHandle,
          name       = f.name(project.config.reqTypes),
          refkey     = unusedField,
          mandatory  = staticMandatoryCheckbox(f.mandatory),
          reqtypes   = appReqTypesEditor.renderReadOnly(f.reqTypes),
          ctrls      = restoreButton)
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
