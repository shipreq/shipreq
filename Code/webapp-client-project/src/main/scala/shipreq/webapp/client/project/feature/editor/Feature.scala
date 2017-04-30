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

object Feature {

  type AsyncError = String
  type AsyncState = AsyncFeature.Read.D0[AsyncError]

  /** This is not safe for reusability because the implementation calls `CallbackTo#runNow()`. */
  trait Editor {
    def render(p: Permission, a: AsyncState): Option[VdomElement]
  }

  object Editor {
    implicit val reusability: Reusability[Editor] =
      Reusability.never // ∵ Editor is not safe for reusability
  }

  /** Id used for [[shipreq.webapp.client.project.feature.PreviewFeature]] */
  final case class PreviewId(row: RowKey, cell: CellKey)
  object PreviewId {
    implicit def equality: UnivEq[PreviewId] = UnivEq.derive
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object State {
    type ForCell    = Option[Editor]
    type ForRow     = Map[CellKey, Editor]
    type ForProject = Map[RowKey, ForRow]

    def initForProject: ForProject =
      UnivEq.emptyMap
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

    object ForCell {
      def doNothing: ForCell =
        apply(None, Deny, None)
    }

    final case class ForRow[R <: RowKey, E <: Editability.ForRow[R]](editor     : State.ForRow,
                                                                     editability: E,
                                                                     async      : AsyncFeature.Read.D1[CellKey, AsyncError]) {
      def apply(c: R#CellKeyConstraint): ForCell =
        ForCell(editor.get(c), editability(c), async(c))
    }

    type ForReq          = ForRow[RowKey.Req              , Editability.ForReq]
    type ForReqCodeGroup = ForRow[RowKey.ReqCodeGroup     , Editability.ForReqCodeGroup]
    type ForUseCaseSteps = ForRow[RowKey.UseCaseSteps.type, Editability.ForUseCaseSteps]

    final case class ForProject(state: State.ForProject,
                                editability: Editability.ForProject,
                                async: AsyncFeature.Read.D2[RowKey, CellKey, AsyncError]) {

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

    final case class ForCell(justStartEdit: Reusable[Callback => Option[Callback]],
                             async        : AsyncFeature.Write.D0[AsyncError]) {

      def startEdit(state: Read.ForCell, cb: Callback): Option[Callback] =
        if (state.editability.is(Deny) || state.editor.isDefined)
          None
        else
          justStartEdit(cb)
    }

    object ForCell {
      val doNothing: ForCell =
        ForCell(Reusable.fn(_ => None), AsyncFeature.Write.D0.doNothing)
    }

    type ForRow[R <: RowKey] = Reusable[ForRowInterface[R]]

    sealed trait ForRowInterface[R <: RowKey] {
      val async: AsyncFeature.Write.D1[CellKey, AsyncError]
      def apply(cell: R#CellKeyConstraint): ForCell
    }

    type ForReq          = ForRow[RowKey.Req]
    type ForReqCodeGroup = ForRow[RowKey.ReqCodeGroup]
    type ForUseCaseSteps = ForRow[RowKey.UseCaseSteps.type]

    /** Create only one instance; reusability is byRef */
    final case class ForProject(static      : Static,
                                stateAccess : StateAccessPure[State.ForProject],
                                async       : AsyncFeature.Write.D2[RowKey, CellKey, AsyncError]) {

      private val reusabilityThisRow: Reusability[(ForProject, RowKey)] = implicitly
      private val reusabilityThisRowCell: Reusability[(ForProject, RowKey, CellKey)] = implicitly

      private def forRow[R <: RowKey](row: R): ForRow[R] = {
        val rowAccess = stateAccess zoomStateL Optics.innerMap(row)
        val rowCmds = NewEditorCmd.make(row)

        val forRow = new ForRowInterface[R] {
          override val async =
            ForProject.this.async(row)

          override def apply(cell: R#CellKeyConstraint): ForCell =
            rowCmds(cell) match {

              case Some(newEditorCmd) =>
                val asyncCell = async(cell)
                def fn: Callback => Option[Callback] =
                  cb => Some(new StartNewEditor(
                    static,
                    rowAccess zoomStateL Optics.mapValue(cell),
                    asyncCell,
                    newEditorCmd)
                    .create(cb))
                val reuseKey = Reusable.explicitly((ForProject.this, row: RowKey, cell: CellKey))(reusabilityThisRowCell)
                ForCell(reuseKey.map(_ => fn), asyncCell)

              case None =>
                ForCell.doNothing
            }

        }

        val reuseKey = Reusable.explicitly((this, row: RowKey))(reusabilityThisRow)
        reuseKey.map(_ => forRow)
      } // def forRow

       def forReq(id: ReqId): ForReq =
         forRow(RowKey.Req(id))

       def forReqCodeGroup(id: ReqCodeId): ForReqCodeGroup =
         forRow(RowKey.ReqCodeGroup(id))

       lazy val forUseCaseSteps: ForUseCaseSteps =
         forRow(RowKey.UseCaseSteps)

      @inline def toReadWrite(r: Read.ForProject): ReadWrite.ForProject =
        ReadWrite.ForProject(r, this)
    }

    implicit val reusabilityForCell   : Reusability[ForCell   ] = Reusability.caseClass
    implicit val reusabilityForProject: Reusability[ForProject] = Reusability.byRef
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object ReadWrite {

    final case class ForCell(read: Read.ForCell, write: Write.ForCell) {
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
      def startEdit: Option[Callback] =
        startEdit(Callback.empty)

      def startEdit(cb: Callback): Option[Callback] =
        write.startEdit(read, cb)

      def asyncFeature = write.async
      def asyncState = read.async
    }

    object ForCell {
      def doNothing: ForCell =
        apply(Read.ForCell.doNothing, Write.ForCell.doNothing)
    }

    final case class ForRow[R <: RowKey, ReadForRow <: Read.ForRow[R, _]](read: ReadForRow, write: Write.ForRow[R]) {
      def asyncFeature = write.async
      def asyncState = read.async

      def apply(cell: R#CellKeyConstraint): ForCell =
        ForCell(read(cell), write.apply(cell))

      def apply(cell: Option[R#CellKeyConstraint]): ForCell =
        cell.fold(ForCell.doNothing)(apply(_))
    }

    type ForReq          = ForRow[RowKey.Req              , Read.ForReq]
    type ForReqCodeGroup = ForRow[RowKey.ReqCodeGroup     , Read.ForReqCodeGroup]
    type ForUseCaseSteps = ForRow[RowKey.UseCaseSteps.type, Read.ForUseCaseSteps]

    final case class ForProject(read: Read.ForProject, write: Write.ForProject) {

      def forReq(id: ReqId): ForReq =
        ForRow(read.forReq(id), write.forReq(id))

      def forReqCodeGroup(id: ReqCodeId): ForReqCodeGroup =
        ForRow(read.forReqCodeGroup(id), write.forReqCodeGroup(id))

      lazy val forUseCaseSteps: ForUseCaseSteps =
        ForRow(read.forUseCaseSteps, write.forUseCaseSteps)

      def asyncFeature = write.async
      def asyncState = read.async
    }

    implicit val reusabilityForCell        : Reusability[ForCell        ] = Reusability.caseClass
    implicit val reusabilityForReq         : Reusability[ForReq         ] = Reusability.caseClass
    implicit val reusabilityForReqCodeGroup: Reusability[ForReqCodeGroup] = Reusability.caseClass
    implicit val reusabilityForUseCaseSteps: Reusability[ForUseCaseSteps] = Reusability.caseClass
    implicit val reusabilityForProject     : Reusability[ForProject     ] = Reusability.caseClass
  }
}
