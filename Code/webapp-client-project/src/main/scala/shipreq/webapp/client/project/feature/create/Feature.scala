package shipreq.webapp.client.project.feature.create

import japgolly.microlibs.utils.Memo
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.Iso
import monocle.macros.Lenses
import scala.reflect.ClassTag
import shipreq.base.util._
import shipreq.webapp.base.feature._
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.protocol.websocket.{CreateContentCmd, ManualIssueCmd}
import shipreq.webapp.base.util.CallbackHelpers._
import shipreq.webapp.client.project.app.state.NewEvents
import shipreq.webapp.client.project.lib.DataReusability._

object Feature {

  type AsyncError = ErrorMsg
  type AsyncState = AsyncFeature.Read.D0[AsyncError]

  /** Editability is not checked here, it's the responsibility of the `Feature` API.
    */
  trait Editor[-Args, +Value] {
    def render(as: AsyncState, args: Args): VdomElement
    def value(args: Args): Editor.Value[Value]

    type State
    val stateType: ClassTag[State]
    val state: State
    def withState(state: State): Editor[Args, Value]

    final override def hashCode =
      state.##

    final override def equals(obj: Any) =
      obj match {
        case t: Editor[_, _] => this eq t
        case _               => false
      }
  }

  object Editor {
    type Invalidity = shipreq.webapp.base.validation.lib.Simple.Invalidity
    type Value[+A] = Invalidity \/ A

    implicit def univEq[A, V]: UnivEq[Editor[A, V]] =
      UnivEq.force

    implicit val reusability: Reusability[Editor[Nothing, Any]] =
      Reusability.byRef
  }

  /** Id used for [[shipreq.webapp.base.feature.PreviewFeature]] */
  final case class PreviewId(row: RowKey, cell: FieldKey)

  object PreviewId {
    implicit def univEq: UnivEq[PreviewId] = UnivEq.derive
    implicit val reusability: Reusability[PreviewId] = Reusability.byRefOrUnivEq
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object State {
    type ForEditor[-A, +V] = Option[Editor[A, V]]

    final case class ForFields[-FK <: FieldKey](untyped: Map[FieldKey, Editor[Nothing, Any]]) {
      def isEmpty = untyped.isEmpty

      def apply(f: FK): ForEditor[f.Args, f.Value] =
        f.cast2(untyped.get(f))

      def -(f: FK): ForFields[FK] =
        ForFields(untyped - f)
    }

    object ForFields {
      val untyped = Iso((_: ForFields[FieldKey]).untyped)(apply)
      val empty = apply(UnivEq.emptyMap)
    }

    @Lenses
    final case class ForProject(untyped: Map[RowKey, ForFields[FieldKey]], selectionHistory: Vector[RowKey]) {

      @elidable(elidable.INFO)
      override def toString: String = {
        def fs(s: State.ForFields[FieldKey]) = s.untyped.iterator.map { case (f, e) => s"\n                FieldKey.$f -> ${e.state},"}.mkString
        val u = untyped.iterator.map { case (r, vs) => s"\n              RowKey.$r -> ${fs(vs)}"}.mkString
        s"""
          |CreateFeature.State.ForProject(
          |  untyped = Map($u)
          |  history = $selectionHistory)
          |""".stripMargin.trim
      }

      def apply(r: RowKey): ForFields[r.FieldKey] =
        untyped.get(r) match {
          case Some(f) => f
          case None    => ForFields.empty
        }
    }

    object ForProject {
      val init: ForProject =
        ForProject(UnivEq.emptyMap, Vector.empty)
    }

    @nowarn("cat=unused")
    private implicit def reusabilityMap[K, V]: Reusability[Map[K, V]] =
      Reusability.byRef

    private val _reusabilityF: Reusability[ForFields[Nothing]] =
      Reusability.byRef || Reusability.derive

    implicit def reusabilityF[FK <: FieldKey]: Reusability[ForFields[FK]] =
      _reusabilityF.narrow

    implicit val reusabilityP: Reusability[ForProject] =
      Reusability.byRef || Reusability.derive
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object Read {

    /** An instance of this implies that Editability has already established.
      */
    final case class ForEditor[-A, +V](editor: Editor[A, V], async: AsyncState) {

      def render(args: A): VdomElement =
        editor.render(async, args)

      def value(args: A): Editor.Value[V] =
        editor.value(args)
    }

    final case class ForFields[-FK <: FieldKey](state      : State.ForFields[FK],
                                                editability: Editability.ForFields[FK],
                                                async      : AsyncFeature.Read.D0[AsyncError]) {

      def apply(f: FK)(newEditor: => Editor[f.Args, f.Value]): Permission.DeniedOr[ForEditor[f.Args, f.Value]] =
        editability(f)(
          ForEditor(state(f).getOrElse(newEditor), async))
    }

    final case class ForProject(state      : State.ForProject,
                                editability: Editability.ForProject,
                                async      : AsyncFeature.Read.D0[AsyncError]) {
      def apply(r: RowKey): ForFields[r.FieldKey] =
        ForFields(state(r), editability(r), async)
    }

    implicit def univEqE[A, V]: UnivEq[ForEditor[A, V]] =
      UnivEq.derive

    private val _reusabilityF: Reusability[ForFields[Nothing]] =
      Reusability.byRef || Reusability.derive

    implicit def reusabilityF[FK <: FieldKey]: Reusability[ForFields[FK]] =
      _reusabilityF.narrow

    implicit val reusabilityP: Reusability[ForProject] =
      Reusability.byRef || Reusability.derive
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object Write {

    final case class ForRow[-FK <: FieldKey, -Cmd](rowAccess : Reusable[StateAccessPure[State.ForFields[FieldKey]]],
                                                   rowEditors: Reusable[NewEditor.ForFields[FK]],
                                                   async     : AsyncFeature.Write.D0[AsyncError],
                                                   ssp       : Reusable[ServerSideProcInvoker[Cmd, ErrorMsg, NewEvents]]) {

      private def _startEditor(field: FK): Editor[field.Args, field.Value] = {
        val stateAccess: StateAccessPure[State.ForEditor[Nothing, Any]] =
          rowAccess.value.zoomStateL(State.ForFields.untyped ^|-> Optics.mapValue(field))

        val ctx = NewEditor.Ctx[field.Args, field.Value](field.cast2(stateAccess))
        rowEditors(field)(ctx)
      }

      // TODO Fix in Scala 3
      UnivEq[FieldKey] // Proof that UnivEq.force below is ok
      private val startEditorMemo: FK => Any =
        Memo[FK, Any](_startEditor(_))(UnivEq.force)

      def startEditor(field: FK): Editor[field.Args, field.Value] =
        startEditorMemo(field).asInstanceOf[Editor[field.Args, field.Value]]

      /** Send a request to the server to create the content for this row. */
      def create(cmd      : Cmd,
                 onSuccess: NewEvents => Callback = _ => Callback.empty): Callback =
        async(ssp(cmd).rightFlatTap(onSuccess(_).asAsyncCallback))

      def clearState(field: FK): Callback =
        rowAccess.modState(_ - field)
    }

    final case class ForProject(stateAccess         : Reusable[StateAccessPure[State.ForProject]],
                                async               : AsyncFeature.Write.D0[AsyncError],
                                sspCreateContent    : Reusable[ServerSideProcInvoker[CreateContentCmd, ErrorMsg, NewEvents]],
                                sspCreateManualIssue: Reusable[ServerSideProcInvoker[ManualIssueCmd, ErrorMsg, NewEvents]],
                               ) {

      private type SSP[-A] = Reusable[ServerSideProcInvoker[A, ErrorMsg, NewEvents]]

      private val foldCmd = RowKey.FoldCmd[SSP](
        codeGroup   = _ => sspCreateContent,
        genericReq  = _ => sspCreateContent,
        useCase     = _ => sspCreateContent,
        manualIssue = _ => sspCreateManualIssue,
      )

      private def _apply(row: RowKey): ForRow[row.FieldKey, row.Cmd] = {
        val rrow = Reusable.implicitly(row)

        val stateAccess2 =
          Reusable.ap(stateAccess, rrow)((stateAccess, row) =>
            stateAccess.zoomStateL(State.ForProject.untyped ^|-> Optics.mapValueEmpty(row, State.ForFields.empty)(_.isEmpty)))

        ForRow[row.FieldKey, row.Cmd](
          stateAccess2,
          rrow.withValue(NewEditor.forRow(row)),
          async,
          foldCmd(row))
      }
      // TODO Fix in Scala 3
      private val applyMemo: RowKey => Any =
        Memo[RowKey, Any](_apply(_))

      def apply(row: RowKey): ForRow[row.FieldKey, row.Cmd] =
        applyMemo(row).asInstanceOf[ForRow[row.FieldKey, row.Cmd]]


      @inline def toReadWrite(r: Read.ForProject): ReadWrite.ForProject =
        ReadWrite.ForProject(r, this)
    }

    private val _reusabilityR: Reusability[ForRow[Nothing, Nothing]] =
      Reusability.byRef || Reusability.derive

    implicit def reusabilityR[FK <: FieldKey, Cmd]: Reusability[ForRow[FK, Cmd]] =
      _reusabilityR.narrow

    implicit val reusabilityP: Reusability[ForProject] =
      Reusability.byRef || Reusability.derive
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object ReadWrite {
    type ForEditor[-A, +V] = Read.ForEditor[A, V]

    type ForManualIssueR = ForRow[FieldKey.ManualIssue.type, ManualIssueCmd]
    type ForManualIssueE = ForEditor[FieldKey.ManualIssue.Args, FieldKey.ManualIssue.Value]

    final case class ForRow[-FK <: FieldKey, -Cmd](read: Read.ForFields[FK], write: Write.ForRow[FK, Cmd]) {

      private def _apply(f: FK): Permission.DeniedOr[ForEditor[f.Args, f.Value]] =
        read(f)(write.startEditor(f))

      // TODO Fix in Scala 3
      UnivEq[FieldKey] // Proof that UnivEq.force below is ok
      private val applyMemo: FK => Any =
        Memo[FK, Any](_apply(_))(UnivEq.force)

      def apply(f: FK): Permission.DeniedOr[ForEditor[f.Args, f.Value]] =
        applyMemo(f).asInstanceOf[Permission.DeniedOr[ForEditor[f.Args, f.Value]]]

      /** Initiates a call to the server to create content for this row. */
      def create(cmd      : Cmd,
                 onSuccess: NewEvents => Callback = _ => Callback.empty): Callback =
        write.create(cmd, onSuccess)

      def clearState(field: FK): Callback =
        write.clearState(field)

      def asyncInProgress =
        AsyncFeature.isInProgress(read.async)
    }

    final case class ForProject(read: Read.ForProject, write: Write.ForProject) {
      def apply(r: RowKey): ForRow[r.FieldKey, r.Cmd] =
        ForRow(read(r), write(r))

      /** This preserves state of editors when users change the req type.
        *
        * See `ReqTableTest.new.state` for the scenario this enables.
        */
      def selectWithRetention(prevRowKey: Option[RowKey], rowKey: RowKey): AsyncCallback[Unit] = {
        type RowState = State.ForFields[FieldKey]

        val currentState       = read.state
        val historyWithoutSelf = currentState.selectionHistory.filterNot(r => prevRowKey.contains(r) || (r ==* rowKey)) ++ prevRowKey
        val newHistory         = historyWithoutSelf :+ rowKey

        var rowModifications: RowState => RowState =
          r => r

        // This is inefficient but it's also O(|reqTypes|) which is tiny
        for {
          srcRow                <- historyWithoutSelf
          srcState              <- currentState.untyped.get(srcRow)
          (srcField, srcEditor) <- srcState.untyped
          tgtField              <- rowKey.convertField(srcField)
        } {
          val tgtEditor = currentState(rowKey)(tgtField).getOrElse(write(rowKey).startEditor(tgtField))
          for (tgtState <- tgtEditor.stateType.unapply(srcEditor.state)) {
            val tgtEditor2 = tgtEditor.withState(tgtState)
            rowModifications = rowModifications.andThen(s => s.copy(s.untyped.updated(tgtField, tgtEditor2)))
          }
        }

        write.stateAccess.modStateAsync { s1 =>
          val s2 = s1.copy(selectionHistory = newHistory)
          val r1 = s2.untyped.getOrElse(rowKey, State.ForFields.empty)
          val r2 = rowModifications(r1)
          val s3 = s2.copy(s2.untyped.updated(rowKey, r2))
          s3
        }
      }
    }

    private val _reusabilityR: Reusability[ForRow[Nothing, Nothing]] =
      Reusability.byRef || Reusability.derive

    implicit def reusabilityR[FK <: FieldKey, Cmd]: Reusability[ForRow[FK, Cmd]] =
      _reusabilityR.narrow

    implicit val reusabilityP: Reusability[ForProject] =
      Reusability.byRef || Reusability.derive
  }
}
