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

object Feature {

  type AsyncError = ErrorMsg
  type AsyncState = AsyncFeature.Read.D0[AsyncError]

  /** This is not safe for reusability because implementation calls `CallbackTo#runNow()`. */
  trait Editor[+Change] {
    def render(p: Permission, a: AsyncState): Option[VdomElement]
    def change(): Editor.Change[Change]
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
    type ForEditor[+C] = Option[Editor[C]]
    type ForFields     = Map[FieldKey, Editor[Any]]
    type ForProject    = Map[RowKey, ForFields]

    def initForProject: ForProject =
      UnivEq.emptyMap

    final case class ForSpecificRow[-FK <: FieldKey](state: ForFields) extends AnyVal {
      @inline def get(f: FK): ForEditor[f.Change] =
        f.cast2(state.get(f))
    }
  }

  val reusabilityStateForEditorAny: Reusability[State.ForEditor[Any]] = {
    implicit def e = Reusability.never[Editor[Any]] // ∵ Editor is not safe for reusability
    Reusability.option
  }
  implicit def reusabilityStateForEditor[A]: Reusability[State.ForEditor[A]] = reusabilityStateForEditorAny.narrow
  implicit val reusabilityStateForFields   : Reusability[State.ForFields   ] = Reusability.when(_.isEmpty)
  implicit val reusabilityStateForProject  : Reusability[State.ForProject  ] = Reusability.when(_.isEmpty)

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object Read {

    final case class ForEditor[+C](editor: Option[Editor[C]], editability: Permission, async: AsyncState) {
      def render(): Option[VdomElement] =
        editor.flatMap(_.render(editability, async))

      def renderOr[A](a: => A)(implicit ev: VdomElement => A): A =
        render().fold(a)(ev)
    }

    object ForEditor {
      val doNothing: ForEditor[Nothing] =
        apply(None, Deny, None)
    }

    final case class ForFields[-FK <: FieldKey](_editor    : State.ForFields,
                                                editability: Reusable[Editability.ForFields[FK]],
                                                async      : AsyncFeature.Read.D1[FieldKey, AsyncError]) {

      def editor: State.ForSpecificRow[FK] =
        State.ForSpecificRow(_editor)

      def apply(f: FK): ForEditor[f.Change] =
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

      def forCodeGroup(id: ReqCodeId): ForCodeGroup =
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

             val reusabilityForEditorAny   : Reusability[ForEditor[Any] ] = Reusability.caseClass
    implicit def reusabilityForEditor[A]   : Reusability[ForEditor[A]   ] = reusabilityForEditorAny.narrow
    implicit val reusabilityForCodeGroup   : Reusability[ForCodeGroup   ] = Reusability.caseClass
    implicit val reusabilityForGenericReq  : Reusability[ForGenericReq  ] = Reusability.caseClass
    implicit val reusabilityForReq         : Reusability[ForReq         ] = Reusability.caseClass
    implicit val reusabilityForUseCase     : Reusability[ForUseCase     ] = Reusability.caseClass
    implicit val reusabilityForUseCaseSteps: Reusability[ForUseCaseSteps] = Reusability.caseClass
    implicit val reusabilityForProject     : Reusability[ForProject     ] = Reusability.caseClass
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object Write {

    final case class ForEditor(newEditor: Reusable[NewEditor], async: AsyncFeature.Write.D0[AsyncError]) {
      def startEdit(state: Read.ForEditor[Any], h: NewEditor.Hooks = NewEditor.Hooks.empty): Option[Callback] =
        if (state.editability.is(Deny) || state.editor.isDefined)
          None
        else
          Some(newEditor.create(h))
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
          override def apply(field: W) = newFn(field)
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
              val stateAccess: StateAccessPure[State.ForEditor[Any]] = rowAccess zoomStateL Optics.mapValue(field)
              val ctx = NewEditor.Ctx[field.Change](field.cast2(stateAccess), asyncCell)
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

      def forCodeGroup(id: ReqCodeId): ForCodeGroup =
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

    implicit val reusabilityForEditor : Reusability[ForEditor ] = Reusability.caseClass
    implicit val reusabilityForProject: Reusability[ForProject] = Reusability.byRef
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object ReadWrite {

    final case class ForEditor[+C](read: Read.ForEditor[C], write: Write.ForEditor, hooks: NewEditor.Hooks) {
      @inline def render(): Option[VdomElement] =
        read.render()

      @inline def renderOr[A](a: => A)(implicit ev: VdomElement => A): A =
        read.renderOr(a)(ev)

      def themedRenderOr(view: => TagMod): TagMod =
        renderOr(TagMod(EditTheme.editableInline(startEdit), view))

      /** Enable an editor so that the user can edit a portion of data.
        *
        * @return `None` if the underlying data isn't allowed to be edited.
        *         `None` if the editor is already active.
        *         `Some[Callback]` otherwise that, when invoked, will add an editor to state and UI.
        */
      def startEdit: Option[Callback] =
        write.startEdit(read, hooks)

      def onStart(cb: Callback): ForEditor[C] =
        copy(hooks = NewEditor.Hooks.onStart.modify(_ >> cb)(hooks))

      def onClose(cb: Callback): ForEditor[C] =
        copy(hooks = NewEditor.Hooks.onClose.modify(_ >> cb)(hooks))

      def asyncFeature = write.async
      def asyncState = read.async
    }

    object ForEditor {
      val doNothing: ForEditor[Nothing] =
        apply(Read.ForEditor.doNothing, Write.ForEditor.doNothing, NewEditor.Hooks.empty)
    }

    final case class ForFields[-FK <: FieldKey](read: Read.ForFields[FK], write: Write.ForFields[FK]) {
      def asyncFeature = write.async
      def asyncState = read.async

      def apply(field: FK): ForEditor[field.Change] =
        ForEditor(read(field), write.apply(field), NewEditor.Hooks.empty)

      def optional(field: Option[FK]): ForEditor[Any] =
        field.fold[ForEditor[Any]](ForEditor.doNothing)(apply(_))
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

      def forCodeGroup(id: ReqCodeId): ForCodeGroup =
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

             val reusabilityForEditorAny   : Reusability[ForEditor[Any] ] = Reusability.caseClass
    implicit def reusabilityForEditor[A]   : Reusability[ForEditor[A]   ] = reusabilityForEditorAny.narrow
    implicit val reusabilityForCodeGroup   : Reusability[ForCodeGroup   ] = Reusability.caseClass
    implicit val reusabilityForGenericReq  : Reusability[ForGenericReq  ] = Reusability.caseClass
    implicit val reusabilityForReq         : Reusability[ForReq         ] = Reusability.caseClass
    implicit val reusabilityForUseCase     : Reusability[ForUseCase     ] = Reusability.caseClass
    implicit val reusabilityForUseCaseSteps: Reusability[ForUseCaseSteps] = Reusability.caseClass
    implicit val reusabilityForProject     : Reusability[ForProject     ] = Reusability.caseClass
  }
}
