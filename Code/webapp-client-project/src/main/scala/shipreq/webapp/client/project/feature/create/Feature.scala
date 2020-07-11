package shipreq.webapp.client.project.feature.create

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.Iso
import monocle.macros.Lenses
import scala.reflect.ClassTag
import scalaz.\/
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.feature._
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.protocol.websocket.{CreateContentCmd, ManualIssueCmd}
import shipreq.webapp.base.util.CallbackHelpers._
import shipreq.webapp.client.project.app.state.NewEvents

/** Nothing here has `Reusability` because:
  *
  * 1. `Editor` doesn't have `Reusability`
  * 2. `Read.ForEditor` has a `Editor` field which will always fail `Reusability` checks.
  * 3. Once State is created, it is never cleared or removed. This prevents reuse-on-empty being helpful.
  * 4. As a consequence, none of the `Read` classes can have `Reusability`
  * 5. As a consequence, none of the `ReadWrite` classes can have `Reusability`
  * 6. The `Write` classes are never passed around outside of `ReadWrite`, so `Reusability` is meaningless.
  */
object Feature {

  type AsyncError = ErrorMsg
  type AsyncState = AsyncFeature.Read.D0[AsyncError]

  /** This is not safe for Reusability because implementations call `CallbackTo#runNow()`.
    *
    * Editability is not checked here, it's the responsibility of the `Feature` API.
    */
  trait Editor[-Args, +Value] {
    def render(as: AsyncState, args: Args)(): VdomElement
    def value(args: Args): Editor.Value[Value]

    type State
    val stateType: ClassTag[State]
    val state: State
    def setState(state: State): Callback
  }

  object Editor {
    type Invalidity = shipreq.webapp.base.validation.Simple.Invalidity
    type Value[+A] = Invalidity \/ A
  }

  /** Id used for [[shipreq.webapp.base.feature.PreviewFeature]] */
  final case class PreviewId(row: RowKey, cell: FieldKey)
  object PreviewId {
    implicit def equality: UnivEq[PreviewId] = UnivEq.derive
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
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object Read {

    /** An instance of this implies that Editability has already established.
      */
    final class ForEditor[-A, +V](val editor: Editor[A, V], val async: AsyncState, emptyArgs: A) {

      /** impure */
      def render(args: A): VdomElement =
        editor.render(async, args)()

      /** impure */
      def value(): Editor.Value[V] =
        editor.value(emptyArgs)
    }

    final case class ForFields[-FK <: FieldKey](state      : State.ForFields[FK],
                                                editability: Editability.ForFields[FK],
                                                async      : AsyncFeature.Read.D0[AsyncError]) {

      def apply(f: FK)(newEditor: => Editor[f.Args, f.Value]): Permission.DeniedOr[ForEditor[f.Args, f.Value]] =
        editability(f)(
          new ForEditor(state(f).getOrElse(newEditor), async, NewEditorArgs.empty))
    }

    final case class ForProject(state      : State.ForProject,
                                editability: Editability.ForProject,
                                async      : AsyncFeature.Read.D0[AsyncError]) {
      def apply(r: RowKey): ForFields[r.FieldKey] =
        ForFields(state(r), editability(r), async)
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object Write {

    final case class ForRow[-FK <: FieldKey, -Cmd](rowAccess : StateAccessPure[State.ForFields[FieldKey]],
                                                   rowEditors: NewEditor.ForFields[FK],
                                                   async     : AsyncFeature.Write.D0[AsyncError],
                                                   ssp       : ServerSideProcInvoker[Cmd, ErrorMsg, NewEvents]) {

      def startEditor(field: FK): Editor[field.Args, field.Value] = {
        val stateAccess: StateAccessPure[State.ForEditor[Nothing, Any]] =
          rowAccess.zoomStateL(State.ForFields.untyped ^|-> Optics.mapValue(field))

        val ctx = NewEditor.Ctx[field.Args, field.Value](field.cast2(stateAccess))
        rowEditors(field)(ctx)
      }

      /** Send a request to the server to create the content for this row. */
      def create(cmd      : Cmd,
                 onSuccess: NewEvents => Callback = _ => Callback.empty): Callback =
        async(ssp(cmd).rightFlatTap(onSuccess(_).asAsyncCallback))

      def clearState(field: FK): Callback =
        rowAccess.modState(_ - field)
    }

    final case class ForProject(static              : NewEditor.Static,
                                stateAccess         : StateAccessPure[State.ForProject],
                                async               : AsyncFeature.Write.D0[AsyncError],
                                sspCreateContent    : ServerSideProcInvoker[CreateContentCmd, ErrorMsg, NewEvents],
                                sspCreateManualIssue: ServerSideProcInvoker[ManualIssueCmd, ErrorMsg, NewEvents],
                               ) {

      private type SSP[-A] = ServerSideProcInvoker[A, ErrorMsg, NewEvents]

      private val foldCmd = RowKey.FoldCmd[SSP](
        codeGroup   = _ => sspCreateContent,
        genericReq  = _ => sspCreateContent,
        useCase     = _ => sspCreateContent,
        manualIssue = _ => sspCreateManualIssue,
      )

      def apply(row: RowKey): ForRow[row.FieldKey, row.Cmd] =
        ForRow[row.FieldKey, row.Cmd](
          stateAccess.zoomStateL(State.ForProject.untyped ^|-> Optics.mapValueEmpty(row, State.ForFields.empty)(_.isEmpty)),
          NewEditor.forRow(static, row),
          async,
          foldCmd(row))

      @inline def toReadWrite(r: Read.ForProject): ReadWrite.ForProject =
        ReadWrite.ForProject(r, this)
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object ReadWrite {
    type ForEditor[-A, +V] = Read.ForEditor[A, V]

    type ForManualIssueR = ForRow[FieldKey.ManualIssue.type, ManualIssueCmd]
    type ForManualIssueE = ForEditor[FieldKey.ManualIssue.Args, FieldKey.ManualIssue.Value]

    final case class ForRow[-FK <: FieldKey, -Cmd](read: Read.ForFields[FK], write: Write.ForRow[FK, Cmd]) {
      def apply(f: FK): Permission.DeniedOr[ForEditor[f.Args, f.Value]] =
        read(f)(write.startEditor(f))

      /** Initiates a call to the server to create content for this row. */
      def create(cmd      : Cmd,
                 onSuccess: NewEvents => Callback = _ => Callback.empty): Callback =
        write.create(cmd, onSuccess)

      def clearState(field: FK): Callback =
        write.clearState(field)
    }

    final case class ForProject(read: Read.ForProject, write: Write.ForProject) {
      def apply(r: RowKey): ForRow[r.FieldKey, r.Cmd] =
        ForRow(read(r), write(r))

      /** This preserves state of editors when users change the req type.
        *
        * See `ReqTableTest.new.state` for the scenario this enables.
        */
      def selectWithRetention(rowKey: RowKey): Callback = {
        val currentState       = read.state
        val historyWithoutSelf = currentState.selectionHistory.filter(_ !=* rowKey)
        val newHistory         = historyWithoutSelf :+ rowKey

        def copyState(srcEditor: Editor[Nothing, Any],
                      tgtField : rowKey.FieldKey): Callback = {
          val tgtEditor = currentState(rowKey)(tgtField).getOrElse(write(rowKey).startEditor(tgtField))
          Callback.traverseOption(tgtEditor.stateType.unapply(srcEditor.state))(tgtEditor.setState)
        }

        var result = write.stateAccess.modState(_.copy(selectionHistory = newHistory))

        // This is inefficient but it's also O(|reqTypes|) which is tiny
        for {
          srcRow                <- historyWithoutSelf
          srcState              <- currentState.untyped.get(srcRow)
          (srcField, srcEditor) <- srcState.untyped
          tgtField              <- rowKey.convertField(srcField)
        }
          result = result >> copyState(srcEditor, tgtField)

        result
      }
    }
  }
}
