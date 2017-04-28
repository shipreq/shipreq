package shipreq.webapp.client.project.feature.editor

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.MonocleReact._
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.client.base.feature._
import shipreq.webapp.client.project.lib.DataReusability._

/** Provides the ability for users to edit the project.
  *
  * Everything here is available for various data scopes:
  * - for the whole project
  * - for rows (eg. a single Req, see [[RowKey]])
  * - for cells (eg. the title of UC-3)
  *
  * The feature is also sliced into various usage/lifecycle scopes:
  * - [[EditorFeature.Read]] - Read-only access to state. Can render existing editors.
  * - [[EditorFeature.Write]] - Write-only access. Requires that state be supplied to use. Can start new editors.
  * - [[EditorFeature.Props]] - Read/write access. The main DSL.
  *
  * Usage: Top-Most Component
  * =========================
  *
  * Add [[EditorFeature.State.ForProject]] to the top-most component's state.
  *
  * Initialise it with [[EditorFeature.State.initForProject]].
  *
  * In the component backend, add `val editorFeature = EditorFeature.Write.ForProject(…)`.
  * It's important that you only create one of these as it affects Reusability.
  *
  * In the render method, combine `editorFeature` and state to create a [[EditorFeature.Props.ForProject]] and pass it
  * to children.
  *
  * Usage: Components
  * =================
  *
  * Request an instance of `EditorFeature.Props.ForXxx` in component props.
  *
  * Supply row and cell keys until arriving at [[EditorFeature.Props.ForCell]]. Then:
  * - use `.renderOr()` to render the editor or a read-only view if the editor is closed.
  * - wire up `.startEdit()` to whatever event handler can start editing.
  */
object EditorFeature {

  type AsyncError = String
  type AsyncState = AsyncFeature.ReadOnly.D0[AsyncError]

  /** This is not safe for reusability because the implementation calls `CallbackTo#runNow()`. */
  trait Editor {
    def render(p: Permission, a: AsyncState): Option[VdomElement]
  }

  object Editor {
    implicit val reusability: Reusability[Editor] =
      Reusability.never // ∵ Editor is not safe for reusability
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object State {
    type ForCell    = Option[Editor]
    type ForRow     = Map[CellKey, Editor]
    type ForProject = Map[RowKey, ForRow]

    def initForCell   : ForCell    = None
    def initForRow    : ForRow     = UnivEq.emptyMap
    def initForProject: ForProject = UnivEq.emptyMap
  }

  implicit val reusabilityStateForCell   : Reusability[State.ForCell   ] = Reusability.option
  implicit val reusabilityStateForRow    : Reusability[State.ForRow    ] = Reusability.mapSameOrEmpty
  implicit val reusabilityStateForProject: Reusability[State.ForProject] = Reusability.mapSameOrEmpty

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object Read {

    final case class ForCell(editor: Option[Editor], editability: Permission, async: AsyncState) {
      def render(): Option[VdomElement] =
        editor.flatMap(_.render(editability, async))

      def renderOr[A](a: => A)(implicit ev: VdomElement => A): A =
        render().fold(a)(ev)
    }

    final case class ForRow[R <: RowKey, E <: Editability.ForRow[R]](editor     : State.ForRow,
                                                                     editability: E,
                                                                     async      : AsyncFeature.ReadOnly.D1[CellKey, AsyncError]) {
      def apply(c: R#CellKeyConstraint): ForCell =
        ForCell(editor.get(c), editability(c), async(c))
    }

    type ForReq          = ForRow[RowKey.Req              , Editability.ForReq]
    type ForReqCodeGroup = ForRow[RowKey.ReqCodeGroup     , Editability.ForReqCodeGroup]
    type ForUseCaseSteps = ForRow[RowKey.UseCaseSteps.type, Editability.ForUseCaseSteps]

    final case class ForProject(state: State.ForProject,
                                editability: Editability.ForProject,
                                async: AsyncFeature.ReadOnly.D2[RowKey, CellKey, AsyncError]) {

       private def forRow[R <: RowKey, E <: Editability.ForRow[R]](r: R, e: E): ForRow[R, E] =
         ForRow(state.getOrElse(r, UnivEq.emptyMap), e, async(r))

       def forReq(id: ReqId): ForReq =
         forRow(RowKey.Req(id), editability.forReqs(id))

       def forReqCodeGroup(id: ReqCodeId): ForReqCodeGroup =
         forRow(RowKey.ReqCodeGroup(id), editability.forReqCodeGroups(id))

       lazy val forUseCaseSteps: ForUseCaseSteps =
         forRow(RowKey.UseCaseSteps, editability.forUseCaseSteps)
    }

    implicit val reusabilityForCell        : Reusability[ForCell        ] = Reusability.caseClass
    implicit val reusabilityForReq         : Reusability[ForReq         ] = Reusability.caseClass
    implicit val reusabilityForReqCodeGroup: Reusability[ForReqCodeGroup] = Reusability.caseClass
    implicit val reusabilityForUseCaseSteps: Reusability[ForUseCaseSteps] = Reusability.caseClass
    implicit val reusabilityForProject     : Reusability[ForProject     ] = Reusability.caseClass
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object Write {

    /**
      * @tparam Input The type of input required by the parent row/cell combination, to create a new editor.
      *               Usually `Unit` or the preview-id. See [[MakeNewEditorCmd]].
      */
    final case class ForCell[-Input](justStartEdit: Reusable[(Input, Callback) => Option[Callback]]) extends AnyVal {
      def startEdit(state: Read.ForCell, input: => Input, cb: Callback): Option[Callback] =
        if (state.editability.is(Deny) || state.editor.isDefined)
          None
        else
          justStartEdit(input, cb)
    }


    type ForRow[R <: RowKey, P] = Reusable[ForRowInterface[R, P]]

    sealed trait ForRowInterface[R <: RowKey, P] {
      def apply[C <: R#CellKeyConstraint](cellKey: C)(implicit ev: MakeNewEditorCmd[R, C]): ForCell[ev.Input[P]]
    }

    type ForReq         [P] = ForRow[RowKey.Req,               P]
    type ForReqCodeGroup[P] = ForRow[RowKey.ReqCodeGroup,      P]
    type ForUseCaseSteps[P] = ForRow[RowKey.UseCaseSteps.type, P]

    /** Create only one instance; reusability is byRef */
    final case class ForProject[P](static      : Static[P],
                                   stateAccess : StateAccessPure[State.ForProject],
                                   asyncFeature: AsyncFeature.Feature.D2[RowKey, CellKey, AsyncError]) {

      private val reusabilityThisAndRow: Reusability[(ForProject[P], RowKey)] = implicitly
      private val reusabilityThisRowCell: Reusability[(ForProject[P], RowKey, CellKey)] = implicitly

      private def forRow[R <: RowKey](row: R): ForRow[R, P] = {
        val rowAccess = stateAccess zoomStateL Optics.innerMap(row)
        val rowAsync = asyncFeature(row)

        val forRow = new ForRowInterface[R, P] {
          override def apply[C <: R#CellKeyConstraint](cell: C)(implicit ev: MakeNewEditorCmd[R, C]): ForCell[ev.Input[P]] = {
            type Input = ev.Input[P]
            val fn1 = ev[P](row, cell)
            def fn2: (Input, Callback) => Option[Callback] =
              (input, cb) => fn1(input).map(cmd =>
                new StartNewEditor(
                  static,
                  rowAccess zoomStateL Optics.mapValue(cell),
                  rowAsync(cell),
                  cmd)(cb)
              )
            val reuseKey = Reusable.explicitly((ForProject.this, row: RowKey, cell: CellKey))(reusabilityThisRowCell)
            ForCell[Input](reuseKey.map(_ => fn2))
          }
        }

        val reuseKey = Reusable.explicitly((this, row: RowKey))(reusabilityThisAndRow)
        reuseKey.map(_ => forRow)
      }

       def forReq(id: ReqId): ForReq[P] =
         forRow(RowKey.Req(id))

       def forReqCodeGroup(id: ReqCodeId): ForReqCodeGroup[P] =
         forRow(RowKey.ReqCodeGroup(id))

       lazy val forUseCaseSteps: ForUseCaseSteps[P] =
         forRow(RowKey.UseCaseSteps)
    }

    private val _reusabilityForCell      : Reusability[ForCell[Nothing]] = Reusability.caseClass
    implicit def reusabilityForCell   [I]: Reusability[ForCell      [I]] = _reusabilityForCell.narrow
    implicit def reusabilityForProject[P]: Reusability[ForProject   [P]] = Reusability.byRef
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object Props {

    final case class ForCell[-Input](read: Read.ForCell, write: Write.ForCell[Input]) {
      @inline def render(): Option[VdomElement] =
        read.render()

      @inline def renderOr[A](a: => A)(implicit ev: VdomElement => A): A =
        read.renderOr(a)(ev)

      /** Enable an editor so that the user can edit a portion of data.
        *
        * @return `None` if the underlying data isn't allowed to be edited.
        *         `None` if the editor is already active.
        *         `Some[Callback]` otherwise that, when invoked, will add an editor to state and UI.
        */
      def startEdit(input: => Input, cb: Callback = Callback.empty): Option[Callback] =
        write.startEdit(read, input, cb)
    }

    final case class ForRow[R <: RowKey, ReadForRow, P](read: ReadForRow, write: Write.ForRow[R, P])

    type ForReq         [P] = ForRow[RowKey.Req              , Read.ForReq,          P]
    type ForReqCodeGroup[P] = ForRow[RowKey.ReqCodeGroup     , Read.ForReqCodeGroup, P]
    type ForUseCaseSteps[P] = ForRow[RowKey.UseCaseSteps.type, Read.ForUseCaseSteps, P]

    final case class ForProject[P](read: Read.ForProject, write: Write.ForProject[P]) {

      def forReq(id: ReqId): ForReq[P] =
        ForRow(read.forReq(id), write.forReq(id))

      def forReqCodeGroup(id: ReqCodeId): ForReqCodeGroup[P] =
        ForRow(read.forReqCodeGroup(id), write.forReqCodeGroup(id))

      lazy val forUseCaseSteps: ForUseCaseSteps[P] =
        ForRow(read.forUseCaseSteps, write.forUseCaseSteps)
    }

    private val _reusabilityForCell           : Reusability[ForCell  [Nothing]] = Reusability.caseClass
    implicit def reusabilityForCell        [I]: Reusability[ForCell        [I]] = _reusabilityForCell.narrow
    implicit def reusabilityForReq         [P]: Reusability[ForReq         [P]] = Reusability.caseClass
    implicit def reusabilityForReqCodeGroup[P]: Reusability[ForReqCodeGroup[P]] = Reusability.caseClass
    implicit def reusabilityForUseCaseSteps[P]: Reusability[ForUseCaseSteps[P]] = Reusability.caseClass
    implicit def reusabilityForProject     [P]: Reusability[ForProject     [P]] = Reusability.caseClass
  }
}
