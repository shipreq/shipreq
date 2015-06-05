package shipreq.webapp.client.app.ui.cfg.fields

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._, MonocleReact._
import japgolly.scalajs.react.extra.{Px, OnUnmount}
import monocle.macros.Lenses
import scala.language.reflectiveCalls
import scalajs.js.{undefined, UndefOr, Any => JsAny}
import scalaz.effect.IO
import scalaz.{Equal, -\/, \/-, \/}
import scalaz.syntax.bind.ToBindOps
import scalaz.syntax.equal._

import shipreq.base.util.ScalaExt._
import shipreq.base.util.{NonEmptySet, NonEmptyVector, Util, ISubset}
import shipreq.webapp.base.data._
import shipreq.webapp.base.delta.Partition
import shipreq.webapp.base.data.Validators.{field => V}
import shipreq.webapp.base.protocol.{DeletionAction, FieldProtocol}
import shipreq.webapp.base.protocol.Routines.FieldCrud
import shipreq.webapp.base.UiText, UiText.FieldNames
import shipreq.webapp.client.ClientData
import shipreq.webapp.client.app.ui._
import shipreq.webapp.client.data.DataReusability._
import shipreq.webapp.client.lib.{FilterDead, ConsoleIO, FailureIO, SuccessIO}
import shipreq.webapp.client.lib.ui.{FieldSet => _, _}
import shipreq.webapp.client.protocol.ClientProtocol
import shipreq.webapp.client.util.{Disabled, Enabled, DND, On}
import Field.ApplicableReqTypes
import FieldProtocol.Delta
import DeletionAction._

object CfgFields {
  case class Props(cp: ClientProtocol, remote: FieldCrud.Remote, clientData: ClientData, filterDead: FilterDead) {
    def component: ReactComponentU_ = MainTable.Component(this)
  }
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
  type ST = ReactST[IO, S, Unit]
  val  ST = ReactS.FixT[IO, S]

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
    val fs         = p.clientData.project.fields.data
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

  val fieldDeltaListener = new DeltaListener.OneByOne[S, FieldId, Delta](
      (s, i) => {
        val s2 = i match {
          case _: StaticField => s
          case j: CustomFieldId => customFieldStores.foldLeft(s)((t, f) => f.s.remove(j)(t))
        }
        clearAppReqTypesEditorState(i)(s2)
      },
      (s, i, d) => {
        val s2 = d match {
          case Delta(-\/(_: StaticField            ), _) => s
          case Delta(\/-(f: CustomField.Text       ), _) => text_storesS.s.set(f.id, f)(s)
          case Delta(\/-(f: CustomField.Implication), _) => impl_storesS.s.set(f.id, f)(s)
          case Delta(\/-(f: CustomField.Tag        ), _) => tag_storesS .s.set(f.id, f)(s)
        }
        clearAppReqTypesEditorState(i)(s2)
      })

  def clearAppReqTypesEditorState(id: FieldId): S => S =
    State._appReqTypeState(id).set(None)

  val Component =
    ReactComponentB[Props]("Cfg: Fields")
      .getInitialState(initialState)
      .backend(new Backend(_))
      .render(_.backend.render)
      .configure(
        DeltaListener.apply  [Props, S, Backend, TopNode](_.clientData, fieldDeltaListener.handler(Partition.Fields)) compose
        DeltaListener.refresh[Props, S, Backend, TopNode](_.clientData, NonEmptySet(
          Partition.CustomReqTypes,  // Refreshes AppReqTypesEditor and reqTypeSelector
          Partition.Tags))           // Refreshes tagSelector
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

    val pxProject = Px.thunkM($.props.clientData.project)
    lazy val backend2 = pxProject.map(new DynBackend(this, _))

    val nameE      = Editors.textInputEditor.applyValidator(V.nameS)
    val refkeyE    = Editors.textInputEditor.applyValidator(V.keyS)
    val mandatoryE = Editors.checkboxEditor.imap(onWhenMandatory).strengthL[V.S]

    def render: ReactElement = {
      pxProject.refresh()
      backend2.value().render
    }

    object protocol {
      import FieldProtocol._, CfgAction._
      val remote = $.props.remote
      val cp     = $.props.cp

      private def call(a: CfgAction): (SuccessIO, FailureIO) => IO[Unit] =
        (s, f) => cp.call(remote)(a, $.props.clientData.update(_) >> s.io, f)

      def createIO(v: FieldProtocol.Values) =
        call(Create(v))

      def updateValuesIO(i: CustomFieldId, v: FieldProtocol.Values) =
        call(UpdateValues(i, v))

      def updateOrderIO(i: FieldId, p: Position) =
        call(UpdateOrder(i, p))

      def deleteIO(i: FieldId, a: DeletionAction) =
        call(Delete(i, a))
    }

    // TODO staticDeletion doesn't handle failure (or lock row)
    val staticDeletion = new Deletion[StaticField](
      protocol.deleteIO(_, _)(SuccessIO.nop, FailureIO.nop))

    def validatorState(k: Option[CustomFieldId]): S => V.S =
      validatorStateS(_, k)

    val dndState = $.focusStateL(State.dnd)

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
    import backend.{backend2 => _, _}

    val appReqTypesEditor = new AppReqTypesEditor(project.customReqTypes.data.values)
    val tagSelector       = SelectOneStartNone.tag(project.tags.data)
    val reqTypeSelector   = SelectOneStartNone.reqType(project.reqTypes)

    val reqtypesE = appReqTypesEditor.editor($ focusStateL State.appReqTypeStates)
                      .cmapA[(V.S, ApplicableReqTypes)](_.map1(_._2))

    object newFieldControl {
      import SelectOne.Choice

      def name: NewSelType => String =
        _.fold(_.name, _.name)

      val Component = SelectInvoke.Component[NewSelType]("NewField")

      def apply() = {
        val s = $.state

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
          case CustomFieldType.Tag         => project.tags.data.values.toStream
                                                .filter(TagInTree.filterLive)
                                                .exists(t => !s.tagFieldTagIds.contains(t.id))
          case CustomFieldType.Implication => project.reqTypes
                                                .filter(_.live :: Live)
                                                .exists(r => !s.implFieldReqTypeIds.contains(r.reqTypeId))
        }
        for (t <- CustomFieldType.values.whole if allowNewCustomFieldType(t))
          add(customFieldChoice(t))

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
              Some($ _setStateL State.newFieldTypeSel)
            ),
            onInvoke, UiText.Cfg.startNewButton,
            Disabled <~ customFieldStores.exists(_.n.editing(s))))
      }

      val abortNew: S => S =
        customFieldStores.map(_.n.remove).reduce(_ compose _)

      val abortButton =
        UI.abortNewButton($ modStateIO abortNew)
    }

    val filterDeadCheckbox = Checkbox.filterDead_$($ focusStateL State.filterDead)

    def render =
      <.div(
        newFieldControl(),
        filterDeadCheckbox(),
        <.table(
          headerRow,
          <.tbody(renderNewField, renderFields)
        ))

    def renderNewField: Option[ReactElement] =
      customFieldRenderers.map(_ renderNewO $.state).flatMap(_.toStream).headOption

    def renderFields: TagMod = {
      var content = fieldOrder.toStream
        .flatMap(_.foldId[Stream[Field]](s => Stream(s), $.state.customFields.get(_).toStream))
      content = $.state.filterDead(content)(_.live)
      content.toReactNodeArray(renderField)
    }

    // TODO orderIO doesn't handle failure (or lock row)
    def orderIO(from: Field, to: Field): IO[Unit] = {
      val id       = from.fieldId
      val newOrder = DND.move(id, to.fieldId)(fieldOrder)
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
        mandatory  = staticMandatoryCheckbox(f.mandatory),
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
      final val stores: NewAndSavedStores[S, CustomFieldId, T, I]) {

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
          case Dead => renderDead (s, dragHandle, row.status, tag)(^.cls := "dead")
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
      val toValues  = FieldProtocol.TagFieldValues.apply _
      val toValuesT = toValues.tupled
      val validator = V.tagField.all map toValuesT
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
          name       = UI mustA f.name(project.tags.data), // TODO is this a Must or an Issue?
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
      val toValues    = FieldProtocol.ImplicationFieldValues.apply _
      val toValuesT   = toValues.tupled
      val validator   = V.implField.all map toValuesT
      val saveFn      = Persistence.asyncSaveNS2(validator, stores, protocol.createIO)(protocol.updateValuesIO,
        SaveNeed.cmpToExtract(t => toValues(t.reqTypeId, t.mandatory, t.reqTypes)),
        _.id, validatorState, $ runState _)
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
          name       = UI mustA f.name(project.customReqTypes.data), // TODO is this a Must or an Issue?
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
