package shipreq.webapp.client.project.feature.editor

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.MonocleReact._
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.feature._
import shipreq.webapp.base.ui.EditTheme
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.ProjectWidgets

object Feature {

  type AsyncError = ErrorMsg
  type AsyncState = AsyncFeature.Read.D0[AsyncError]

  /** This is not safe for reusability because implementation calls `CallbackTo#runNow()`. */
  trait Editor[-Args, +Change] {

    /** impure */
    def render(p: Permission, as: AsyncState, args: Args): Option[VdomElement]

    /** impure */
    def change(args: Args): Editor.Change[Change]
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

    final case class ForEditor[-A, +C](editor: Option[Editor[A, C]], editability: Permission, async: AsyncState) {
      /** impure */
      def render(args: A): Option[VdomElement] =
        editor.flatMap(_.render(editability, async, args))

      /** impure */
      def renderOr[B](args: A)(b: => B)(implicit ev: VdomElement => B): B =
        render(args).fold(b)(ev)
    }

    object ForEditor {
      val doNothing: ForEditor[Any, Nothing] =
        apply(None, Deny, None)
    }

    final case class ForFields[-FK <: FieldKey](_editor    : State.ForFields,
                                                editability: Reusable[Editability.ForFields[FK]],
                                                async      : AsyncFeature.Read.D1[FieldKey, AsyncError]) {

      def editor: State.ForSpecificRow[FK] =
        State.ForSpecificRow(_editor)

      def apply(f: FK): ForEditor[f.Args, f.Change] =
        ForEditor(editor.get(f), editability(f), async(f))
    }

    implicit class ForFieldsInvariantExt[FK <: FieldKey](private val self: ForFields[FK]) extends AnyVal {
      def widen[W >: FK <: FieldKey](implicit t: FieldKey.Type[FK]): ForFields[W] =
        self.copy(editability = self.editability.map(_.widen(t)))
    }

    type ForCodeGroup    = ForFields[FieldKey.ForCodeGroup ]
    type ForGenericReq   = ForFields[FieldKey.ForGenericReq]
    type ForReq          = ForFields[FieldKey.ForSomeReq   ]
    type ForUseCase      = ForFields[FieldKey.ForUseCase   ]
    type ForUseCaseSteps = ForFields[FieldKey.UseCaseStep  ]

    final case class ForProject(state      : State.ForProject,
                                editability: Editability.ForProject,
                                async      : AsyncFeature.Read.D2[RowKey, FieldKey, AsyncError]) {

       private def forRow(r: RowKey)(e: Reusable[Editability.ForFields[r.FieldKey]]): ForFields[r.FieldKey] =
         ForFields(state.getOrElse(r, UnivEq.emptyMap), e, async(r))

      def forCodeGroup(id: ReqCodeGroupId): ForCodeGroup =
        forRow(RowKey.CodeGroup(id))(Reusable implicitly editability.forCodeGroups(id))

      def forGenericReq(id: GenericReqId): ForGenericReq =
         forRow(RowKey.GenericReq(id))(Reusable implicitly editability.forReqs(id))

      def forReq(id: ReqId): ForReq =
        id.foldReqId(forGenericReq(_).widen, forUseCase(_).widen)

      def forUseCase(id: UseCaseId): ForUseCase =
        forRow(RowKey.UseCase(id))(Reusable implicitly editability.forReqs(id))

      lazy val forUseCaseSteps: ForUseCaseSteps =
        forRow(RowKey.UseCaseSteps)(Reusable implicitly editability.forUseCaseSteps)
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
                    hooks           : NewEditor.Hooks = NewEditor.Hooks.empty): Option[Callback] =
        startEditWithArgs(
          state,
          FreeOption(NewEditor.CreationArgs(pxProjectWidgets, hooks)))

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

      /** impure */
      @inline def render(args: A): Option[VdomElement] =
        read.render(args)

      /** impure */
      @inline def renderOr[B](args: A)(b: => B)(implicit ev: VdomElement => B): B =
        read.renderOr(args)(b)(ev)

      /** impure */
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

      def asyncFeature = write.async
      def asyncState = read.async
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

      def apply(f: FK, pxProjectWidgets: Reusable[Px[ProjectWidgets.AnyCtx]]): ForEditor[f.Args, f.Change] =
        ForEditor(
          read(f),
          write.apply(f),
          FreeOption(NewEditor.CreationArgs(pxProjectWidgets, NewEditor.Hooks.empty)))
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
