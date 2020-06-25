package shipreq.webapp.client.project.feature.editor

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.feature._
import shipreq.webapp.base.feature.clipboard.ClipboardData
import shipreq.webapp.base.text.ProjectText
import shipreq.webapp.base.ui.EditTheme
import shipreq.webapp.client.project.feature.RenderFeature
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.ProjectWidgets

object Feature {

  type AsyncError = ErrorMsg
  type AsyncState = AsyncFeature.Read.D0[AsyncError]

  @inline private def defaultEditorStyle =
    EditTheme.Style.default

  /** This is not safe for reusability because implementation calls `CallbackTo#runNow()`. */
  trait Editor[-Args, +Change] {

    /** impure */
    def render(p: Permission, as: AsyncState, args: Args): Option[VdomNode]

    def change[C >: Change]: CallbackTo[Editor.Change[C]]

    def clipboardData: Option[ClipboardData]

    def setPotentialValue(p: PotentialValue): Option[Callback]
  }

  object Editor {
    type Invalidity = shipreq.webapp.base.validation.Simple.Invalidity
    type Change[+A] = PotentialChange[Invalidity, A]
  }

  /** Id used for [[shipreq.webapp.base.feature.PreviewFeature]] */
  final case class PreviewId(row: RowKey, cell: FieldKey)
  object PreviewId {
    implicit def equality: UnivEq[PreviewId] = UnivEq.derive
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object State {
    type ForEditor[-A, +C] = Option[Editor[A, C]]
    type ForAnyEditor      = ForEditor[Nothing, Any]
    type ForFields         = Map[FieldKey, Editor[Nothing, Any]]
    type ForProject        = Map[RowKey, ForFields]

    def initForProject: ForProject =
      UnivEq.emptyMap

    final case class ForSpecificRow[-FK <: FieldKey](state: ForFields) extends AnyVal {
      @inline def get(f: FK): ForEditor[f.Args, f.Change] =
        f.cast2(state.get(f))
    }
  }

  val reusabilityStateForEditorAny: Reusability[State.ForAnyEditor] = {
    implicit def e = Reusability.never[Editor[Nothing, Any]] // ∵ Editor is not safe for reusability
    Reusability.option
  }
  implicit def reusabilityStateForEditor[A, C]: Reusability[State.ForEditor[A, C]] = reusabilityStateForEditorAny.narrow
  implicit val reusabilityStateForFields      : Reusability[State.ForFields      ] = Reusability.when(_.isEmpty)
  implicit val reusabilityStateForProject     : Reusability[State.ForProject     ] = Reusability.when(_.isEmpty)

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object Read {

    type ForAnyEditor = ForEditor[Nothing, Any]

    // Note: editor is package-private here because it's actually read & write, where as this class is read-only
    final case class ForEditor[-A, +C](private[editor] val editor: Option[Editor[A, C]],
                                       renderText                : Reusable[() => Option[String]],
                                       editability               : Permission,
                                       async                     : AsyncState) {

      def clipboardData: Option[ClipboardData] =
        editor match {
          case None    => renderText.value().map(ClipboardData.apply)
          case Some(e) => e.clipboardData
        }

      def isOpen: Boolean =
        editor.isDefined

      /** impure */
      def render(args: A): Option[VdomNode] =
        editor.flatMap(_.render(editability, async, args))

      /** impure */
      def renderOr[B](args: A)(b: => B)(implicit ev: VdomNode => B): B =
        render(args).fold(b)(ev)
    }

    object ForEditor {
      val doNothing: ForEditor[Any, Nothing] =
        apply(None, Reusable.always(() => None), Deny, None)
    }

    final case class ForFields[-FK <: FieldKey](_editor    : State.ForFields,
                                                renderText : RenderFeature.ToText.AnyCtx.ApplicableOption.ForField[RenderFeature.FieldKey],
                                                editability: Reusable[Editability.ForFields[FK]],
                                                async      : AsyncFeature.Read.D1[FieldKey, AsyncError]) {

      def editor: State.ForSpecificRow[FK] =
        State.ForSpecificRow(_editor)

      def apply(f: FK): ForEditor[f.Args, f.Change] =
        ForEditor(
          editor.get(f),
          Reusable.implicitly(renderText).withValue(() => renderText(f.forRender)),
          editability(f),
          async(f))
    }

    implicit class ForFieldsInvariantExt[FK <: FieldKey](private val self: ForFields[FK]) extends AnyVal {
      def widen[W >: FK <: FieldKey](implicit t: FieldKey.Type[FK]): ForFields[W] =
        ForFields[W](
          self._editor,
          self.renderText,
          self.editability.map(_.widen(t)),
          self.async)
    }

    type ForCodeGroup    = ForFields[FieldKey.ForCodeGroup ]
    type ForGenericReq   = ForFields[FieldKey.ForGenericReq]
    type ForReq          = ForFields[FieldKey.ForSomeReq   ]
    type ForUseCase      = ForFields[FieldKey.ForUseCase   ]
    type ForUseCaseSteps = ForFields[FieldKey.UseCaseStep  ]
    type ForManualIssues = ForFields[FieldKey.ManualIssue  ]

    final case class ForProject(state      : State.ForProject,
                                renderText : RenderFeature.ToText.AnyCtx.ApplicableOption.ForProject,
                                editability: Editability.ForProject,
                                async      : AsyncFeature.Read.D2[RowKey, FieldKey, AsyncError]) {

       private def forRow(r          : RowKey)
                         (renderText : RenderFeature.ForFields[ProjectText.Context, RenderFeature.FieldKey, Option[String]],
                          editability: Reusable[Editability.ForFields[r.FieldKey]]): ForFields[r.FieldKey] =
         ForFields(
           state.getOrElse(r, UnivEq.emptyMap),
           renderText,
           editability,
           async(r))

      def forCodeGroup(id: ReqCodeGroupId): ForCodeGroup =
        forRow(RowKey.CodeGroup(id))(
          renderText.forCodeGroupId(id).widen(None),
          Reusable.implicitly(editability.forCodeGroups(id)))

      def forGenericReq(id: GenericReqId): ForGenericReq =
         forRow(RowKey.GenericReq(id))(
           renderText.forGenericReq(id).widen(None),
           Reusable.implicitly(editability.forReqs(id)))

      def forReq(id: ReqId): ForReq =
        id.foldReqId(forGenericReq(_).widen, forUseCase(_).widen)

      def forUseCase(id: UseCaseId): ForUseCase =
        forRow(RowKey.UseCase(id))(
          renderText.forUseCase(id).widen(None),
          Reusable.implicitly(editability.forReqs(id)))

      lazy val forUseCaseSteps: ForUseCaseSteps =
        forRow(RowKey.UseCaseSteps)(
          renderText.forUseCaseSteps.widen(None),
          Reusable.implicitly(editability.forUseCaseSteps))

      lazy val forManualIssues: ForManualIssues =
        forRow(RowKey.ManualIssues)(
          renderText.forManualIssues.widen(None),
          Editability.forManualIssues)
    }

             val reusabilityForEditorAny   : Reusability[ForAnyEditor   ] = Reusability.derive
    implicit def reusabilityForEditor[A, C]: Reusability[ForEditor[A, C]] = reusabilityForEditorAny.narrow
    implicit val reusabilityForCodeGroup   : Reusability[ForCodeGroup   ] = Reusability.derive
    implicit val reusabilityForGenericReq  : Reusability[ForGenericReq  ] = Reusability.derive
    implicit val reusabilityForReq         : Reusability[ForReq         ] = Reusability.derive
    implicit val reusabilityForUseCase     : Reusability[ForUseCase     ] = Reusability.derive
    implicit val reusabilityForUseCaseSteps: Reusability[ForUseCaseSteps] = Reusability.derive
    implicit val reusabilityForProject     : Reusability[ForProject     ] = Reusability.derive
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object Write {

    final case class ForEditor(newEditor: Reusable[NewEditor], async: AsyncFeature.Write.D0[AsyncError]) {

      def startEdit(state           : Read.ForAnyEditor,
                    pxProjectWidgets: Reusable[Px[ProjectWidgets.AnyCtx]],
                    filterDead      : FilterDead,
                    hooks           : NewEditor.Hooks = NewEditor.Hooks.empty,
                    potentialValue  : Option[PotentialValue] = None): Option[Callback] =
        startEditWithArgs(
          state,
          FreeOption(NewEditor.CreationArgs(pxProjectWidgets, defaultEditorStyle, filterDead, potentialValue, hooks)))

      private[Feature] def startEditWithArgs(state: Read.ForAnyEditor,
                                             args : FreeOption[NewEditor.CreationArgs]): Option[Callback] =
        if (state.editability.is(Deny) || state.editor.isDefined)
          None
        else
          args.fold(None, a => Some(newEditor.create(a)))
    }

    object ForEditor {
      val doNothing: ForEditor = {
        val ne = Reusable.byRef(NewEditor.doNothing.create).map(NewEditor.apply)
        apply(ne, AsyncFeature.Write.D0.doNothing)
      }
    }

    sealed trait ForFieldsInterface[-FK <: FieldKey] {
      val async: AsyncFeature.Write.D1[FieldKey, AsyncError]
      def apply(field: FK): ForEditor
    }

    type ForFields[-FK <: FieldKey] = Reusable[ForFieldsInterface[FK]]

    implicit class ForFieldsInvariantExt[FK <: FieldKey](private val self: ForFields[FK]) extends AnyVal {
      def widen[W >: FK <: FieldKey](implicit t: FieldKey.Type[FK]): ForFields[W] =
        self.map(orig => new ForFieldsInterface[W] {
          val newFn = t.widenFn[W, ForEditor](orig.apply)(ForEditor.doNothing)
          override def apply(f: W) = newFn(f)
          override val async = orig.async
        })
    }

    type ForCodeGroup    = ForFields[FieldKey.ForCodeGroup ]
    type ForGenericReq   = ForFields[FieldKey.ForGenericReq]
    type ForReq          = ForFields[FieldKey.ForSomeReq   ]
    type ForUseCase      = ForFields[FieldKey.ForUseCase   ]
    type ForUseCaseSteps = ForFields[FieldKey.UseCaseStep  ]
    type ForManualIssues = ForFields[FieldKey.ManualIssue  ]

    /** Create only one instance; reusability is byRef */
    final case class ForProject(static      : NewEditor.Static,
                                stateAccess : StateAccessPure[State.ForProject],
                                async       : AsyncFeature.Write.D2[RowKey, FieldKey, AsyncError]) {

      private val reuseFromRow  : Reusability[(ForProject, RowKey)          ] = implicitly
      private val reuseFromField: Reusability[(ForProject, RowKey, FieldKey)] = implicitly

      private def instanceForRow(row: RowKey): ForFieldsInterface[row.FieldKey] =
        new ForFieldsInterface[row.FieldKey] {
          val rowAccess = stateAccess zoomStateL Optics.innerMap(row)
          val rowEditors = NewEditor.forRow(static, row)

          override val async =
            ForProject.this.async(row)

          override def apply(field: row.FieldKey): ForEditor = {
            val asyncCell = async(field)

            def newEditor: NewEditor = {
              val stateAccess: StateAccessPure[State.ForAnyEditor] = rowAccess zoomStateL Optics.mapValue(field)
              val ctx = NewEditor.Ctx[field.Args, field.Change](field.cast2(stateAccess), asyncCell)
              rowEditors(field)(ctx)
            }

            val reuseKey = reuseFromField.reusable((ForProject.this, row, field))
            ForEditor(reuseKey.map(_ => newEditor), asyncCell)
          }
        }

      private def forRow(row: RowKey): ForFields[row.FieldKey] = {
        val reuseKey = reuseFromRow.reusable((this, row: RowKey))
        reuseKey.map(_ => instanceForRow(row))
      }

      def forCodeGroup(id: ReqCodeGroupId): ForCodeGroup =
        forRow(RowKey.CodeGroup(id))

      def forGenericReq(id: GenericReqId): ForGenericReq =
        forRow(RowKey.GenericReq(id))

      def forReq(id: ReqId): ForReq =
        id.foldReqId(forGenericReq(_).widen, forUseCase(_).widen)

      def forUseCase(id: UseCaseId): ForUseCase =
        forRow(RowKey.UseCase(id))

      lazy val forUseCaseSteps: ForUseCaseSteps =
        forRow(RowKey.UseCaseSteps)

      lazy val forManualIssues: ForManualIssues =
        forRow(RowKey.ManualIssues)

      @inline def toReadWrite(r: Read.ForProject): ReadWrite.ForProject =
        ReadWrite.ForProject(r, this)
    }

    implicit val reusabilityForEditor: Reusability[ForEditor] = Reusability.derive
    implicit val reusabilityForProject: Reusability[ForProject] = Reusability.byRef
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object ReadWrite {

    type ForAnyEditor = ForEditor[Nothing, Any]

    final case class ForEditor[-A, +C](read        : Read.ForEditor[A, C],
                                       write       : Write.ForEditor,
                                       creationArgs: FreeOption[NewEditor.CreationArgs]) {

      def asyncFeature  = write.async
      def asyncState    = read.async
      def clipboardData = read.clipboardData

      /** impure */
      @inline def render(args: A): Option[VdomNode] =
        read.render(args)

      /** impure */
      @inline def renderOr[B](args: A)(b: => B)(implicit ev: VdomNode => B): B =
        read.renderOr(args)(b)(ev)

      /** 1) Renders the editor if open, or a given view otherwise.
        * 2) Modifies the parent vdom so that
        *    - double-clicking starts the editor
        *    - there is hover text with user instructions
        *    - colour changes on hover
        *
        * impure
        */
      def themedRenderOr(args: A)(view: => TagMod): TagMod =
        renderOr(args)(TagMod(EditTheme.editableInline(startEdit), view))

      /** Enable an editor so that the user can edit a portion of data.
        *
        * @return `None` if the underlying data isn't allowed to be edited.
        *         `None` if the editor is already active.
        *         `Some[Callback]` otherwise that, when invoked, will add an editor to state and UI.
        */
      def startEdit: Option[Callback] =
        write.startEditWithArgs(read, creationArgs)

      def onStart(cb: Callback): ForEditor[A, C] =
        copy(creationArgs = creationArgs.map(NewEditor.CreationArgs.onStart.modify(_ >> cb)))

      def onClose(cb: Callback): ForEditor[A, C] =
        copy(creationArgs = creationArgs.map(NewEditor.CreationArgs.onClose.modify(_ >> cb)))

      def withEditorStyle(s: EditTheme.Style): ForEditor[A, C] =
        copy(creationArgs = creationArgs.map(_.copy(editorStyle = s)))

      def withPotentialValue(p: PotentialValue): ForEditor[A, C] =
        copy(creationArgs = creationArgs.map(_.copy(potentialValue = Some(p))))

      val setPotentialValueFnIfAllowed: Option[PotentialValue => Option[Callback]] =
        SetValueDecision(read) match {
          case SetValueDecision.OpenAndReplace => Some(withPotentialValue(_).startEdit)
          case SetValueDecision.Replace        => Some(p => read.editor.flatMap(_.setPotentialValue(p)))
          case SetValueDecision.Ignore         => None
        }

      def setPotentialValue(p: PotentialValue): Option[Callback] =
        setPotentialValueFnIfAllowed.flatMap(_(p))

      def setPotentialValueAsync(getPV: AsyncCallback[PotentialValue]): Option[Callback] =
        setPotentialValueFnIfAllowed.map(set =>
          getPV.flatMap(pv =>
            set(pv).getOrEmpty.asAsyncCallback
          ).toCallback
        )
    }

    object ForEditor {
      def doNothing[A]: ForEditor[A, Nothing] =
        apply(
          Read.ForEditor.doNothing,
          Write.ForEditor.doNothing,
          FreeOption.empty)
    }

    final case class ForFields[-FK <: FieldKey](read: Read.ForFields[FK], write: Write.ForFields[FK]) {
      def asyncFeature = write.async
      def asyncState = read.async

      def apply(f: FK,
                pxProjectWidgets: Reusable[Px[ProjectWidgets.AnyCtx]],
                filterDead      : FilterDead): ForEditor[f.Args, f.Change] =
        ForEditor(
          read(f),
          write.apply(f),
          FreeOption(NewEditor.CreationArgs(pxProjectWidgets, defaultEditorStyle, filterDead, None, NewEditor.Hooks.empty)))
    }

    implicit class ForFieldsInvariantExt[FK <: FieldKey](private val self: ForFields[FK]) extends AnyVal {
      def widen[W >: FK <: FieldKey](implicit t: FieldKey.Type[FK]): ForFields[W] =
        ForFields(self.read.widen(t), self.write.widen(t))
    }

    type ForCodeGroup    = ForFields[FieldKey.ForCodeGroup ]
    type ForGenericReq   = ForFields[FieldKey.ForGenericReq]
    type ForReq          = ForFields[FieldKey.ForSomeReq   ]
    type ForUseCase      = ForFields[FieldKey.ForUseCase   ]
    type ForUseCaseSteps = ForFields[FieldKey.UseCaseStep  ]
    type ForManualIssues = ForFields[FieldKey.ManualIssue  ]

    final case class ForProject(read: Read.ForProject, write: Write.ForProject) {

      def forCodeGroup(id: ReqCodeGroupId): ForCodeGroup =
        ForFields(read.forCodeGroup(id), write.forCodeGroup(id))

      def forGenericReq(id: GenericReqId): ForGenericReq =
        ForFields(read.forGenericReq(id), write.forGenericReq(id))

      def forReq(id: ReqId): ForReq =
        id.foldReqId(forGenericReq(_).widen, forUseCase(_).widen)

      def forUseCase(id: UseCaseId): ForUseCase =
        ForFields(read.forUseCase(id), write.forUseCase(id))

      lazy val forUseCaseSteps: ForUseCaseSteps =
        ForFields(read.forUseCaseSteps, write.forUseCaseSteps)

      lazy val forManualIssues: ForManualIssues =
        ForFields(read.forManualIssues, write.forManualIssues)

      def asyncFeature = write.async
      def asyncState = read.async
    }

             val reusabilityForAnyEditor   : Reusability[ForAnyEditor   ] = Reusability.derive
    implicit def reusabilityForEditor[A, C]: Reusability[ForEditor[A, C]] = reusabilityForAnyEditor.narrow
    implicit val reusabilityForCodeGroup   : Reusability[ForCodeGroup   ] = Reusability.derive
    implicit val reusabilityForGenericReq  : Reusability[ForGenericReq  ] = Reusability.derive
    implicit val reusabilityForReq         : Reusability[ForReq         ] = Reusability.derive
    implicit val reusabilityForUseCase     : Reusability[ForUseCase     ] = Reusability.derive
    implicit val reusabilityForUseCaseSteps: Reusability[ForUseCaseSteps] = Reusability.derive
    implicit val reusabilityForProject     : Reusability[ForProject     ] = Reusability.derive
  }
}
