package shipreq.webapp.client.project.feature.create

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.MonocleReact._
import scalaz.\/
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.feature._
import shipreq.webapp.base.protocol.{CreateContentCmd, ManualIssueCmd, ServerSideProcInvoker}

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
  }

  object Editor {
    type Invalidity = shipreq.webapp.base.validation.Simple.Invalidity
    type Value[+A] = Invalidity \/ A

    def fromCallback[A, V](cb: CallbackTo[Editor[A, V]]): Editor[A, V] =
      new Editor[A, V] {
        override def render(as: AsyncState, args: A)() = cb.runNow().render(as, args)()
        override def value(args: A) = cb.runNow().value(args)
      }
  }

  /** Id used for [[shipreq.webapp.base.feature.PreviewFeature]] */
  final case class PreviewId(row: RowKey, cell: FieldKey)
  object PreviewId {
    implicit def equality: UnivEq[PreviewId] = UnivEq.derive
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object State {
    type ForEditor[-A, +V] = Option[Editor[A, V]]
    type ForFields         = Map[FieldKey, Editor[Nothing, Any]]
    type ForProject        = Map[RowKey, ForFields]

    def initForProject: ForProject =
      UnivEq.emptyMap

    final case class ForSpecificRow[-FK <: FieldKey](state: ForFields) extends AnyVal {
      @inline def get(f: FK): ForEditor[f.Args, f.Value] =
        f.cast2(state.get(f))
    }
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object Read {

    /** An instance of this implies that Editability has already established.
      */
    final case class ForEditor[-A, +V](editor: Editor[A, V], async: AsyncState) {
      /** impure */
      def render(args: A): VdomElement =
        editor.render(async, args)()

      /** impure */
      def value(args: A): Editor.Value[V] =
        editor.value(args)
    }

    final case class ForFields[-FK <: FieldKey](_state     : State.ForFields,
                                                editability: Editability.ForFields[FK],
                                                async      : AsyncFeature.Read.D0[AsyncError]) {
      def state: State.ForSpecificRow[FK] =
        State.ForSpecificRow(_state)

      def apply(f: FK)(newEditor: => Editor[f.Args, f.Value]): Permission.DeniedOr[ForEditor[f.Args, f.Value]] =
        editability(f)(
          ForEditor(state.get(f).getOrElse(newEditor), async))
    }

    final case class ForProject(state      : State.ForProject,
                                editability: Editability.ForProject,
                                async      : AsyncFeature.Read.D1[RowKey, AsyncError]) {
      def apply(r: RowKey): ForFields[r.FieldKey] =
        ForFields(state.getOrElse(r, UnivEq.emptyMap), editability(r), async(r))
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object Write {

    final case class ForRow[-FK <: FieldKey, -Cmd](rowAccess : StateAccessPure[State.ForFields],
                                                   rowEditors: NewEditor.ForFields[FK],
                                                   async     : AsyncFeature.Write.D0[AsyncError],
                                                   ssp       : ServerSideProcInvoker[Cmd, ErrorMsg, Any]) {
      def startEditor(field: FK): Editor[field.Args, field.Value] = {
        val stateAccess: StateAccessPure[State.ForEditor[Nothing, Any]] = rowAccess zoomStateL Optics.mapValue(field)
        val ctx = NewEditor.Ctx[field.Args, field.Value](field.cast2(stateAccess))
        rowEditors(field)(ctx)
      }

      /** Initiates a call to the server to create content for this row. */
      def create(cmd: Cmd, onSuccess: Callback = Callback.empty): Callback =
        async((s, f) => ssp(cmd, _ => s >> onSuccess, f))
    }

    final case class ForProject(static              : NewEditor.Static,
                                stateAccess         : StateAccessPure[State.ForProject],
                                async               : AsyncFeature.Write.D1[RowKey, AsyncError],
                                sspCreateContent    : ServerSideProcInvoker[CreateContentCmd, ErrorMsg, Any],
                                sspCreateManualIssue: ServerSideProcInvoker[ManualIssueCmd, ErrorMsg, Any],
                               ) {

      private type SSP[-A] = ServerSideProcInvoker[A, ErrorMsg, Any]

      private val foldCmd = RowKey.FoldCmd[SSP](
        codeGroup   = _ => sspCreateContent,
        genericReq  = _ => sspCreateContent,
        useCase     = _ => sspCreateContent,
        manualIssue = _ => sspCreateManualIssue,
      )

      def apply(row: RowKey): ForRow[row.FieldKey, row.Cmd] =
        ForRow[row.FieldKey, row.Cmd](
          stateAccess zoomStateL Optics.innerMap(row),
          NewEditor.forRow(static, row),
          async(row),
          foldCmd(row))

      @inline def toReadWrite(r: Read.ForProject): ReadWrite.ForProject =
        ReadWrite.ForProject(r, this)
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object ReadWrite {
    type ForEditor[-A, +V] = Read.ForEditor[A, V]

    final case class ForRow[-FK <: FieldKey, -Cmd](read: Read.ForFields[FK], write: Write.ForRow[FK, Cmd]) {
      def apply(f: FK): Permission.DeniedOr[ForEditor[f.Args, f.Value]] =
        read(f)(write.startEditor(f))

      /** Initiates a call to the server to create content for this row. */
      def create(cmd: Cmd, onSuccess: Callback = Callback.empty): Callback =
        write.create(cmd, onSuccess)
    }

    final case class ForProject(read: Read.ForProject, write: Write.ForProject) {
      def apply(r: RowKey): ForRow[r.FieldKey, r.Cmd] =
        ForRow(read(r), write(r))
    }
  }
}
