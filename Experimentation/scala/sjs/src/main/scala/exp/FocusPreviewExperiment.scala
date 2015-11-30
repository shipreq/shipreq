package exp

import monocle._
import monocle.macros.Lenses
import org.scalajs.dom, dom.ext.KeyCode
import shipreq.base.util.Util
import scalajs.js
import scalaz.Equal
import scalaz.std.anyVal.intInstance
import scalaz.std.string.stringInstance
import scalaz.syntax.equal._
import japgolly.scalajs.react._, vdom.prefix_<^._, MonocleReact._
import japgolly.scalajs.react.extra._

/**
  * Preview available:
  * - when editing and focused and (dirty or has been edited since receiving focus)
  *
  * Editor opens:
  * - when clicked
  * - when navigated to by KB
  *
  * Editor closes:
  * - on commit (enter)
  * - on abort (escape)
  * - when loses focus and there is no change
  */
object FocusPreviewExperiment {

//  import CompState._
//
//  @inline implicit def MonocleReactCompStateOpsDD2[$, S]($: $)(implicit ops: $ => ReadDirectWriteDirectOps[S]) =
//    new MonocleReactCompStateOps2[ReadDirectWriteDirectOps[S], S, Unit](ops($))
//
//  @inline implicit def MonocleReactCompStateOpsDC2[$, S]($: $)(implicit ops: $ => ReadDirectWriteCallbackOps[S]) =
//    new MonocleReactCompStateOps2[ReadDirectWriteCallbackOps[S], S, Callback](ops($))
//
//  @inline implicit def MonocleReactCompStateOpsCC2[$, S]($: $)(implicit ops: $ => ReadCallbackWriteCallbackOps[S]) =
//    new MonocleReactCompStateOps2[ReadCallbackWriteCallbackOps[S], S, Callback](ops($))
//
//  final class MonocleReactCompStateOps2[Ops <: WriteOpAux[S, W], S, W](private val $: Ops) extends AnyVal {
//    def setStateL[L[_, _, _, _], B](l: L[S, S, _, B])(b: B, cb: Callback = Callback.empty)(implicit L: SetterMonocle[L]): W =
//      $.modState(L.set(l)(b), cb)
//  }
//
//  object _ExternalVar {
//    def ss[S](s: S, $: CompState.WriteCallbackOps[S]): ExternalVar[S] =
//      new ExternalVar(s, $ setState _)
//  }
//  @inline implicit final class MonocleReactExternalVarObjOps(private val x: ExternalVar.type) extends AnyVal {
//    def at[S, A](lens: Lens[S, A])(s: S, $: CompState.WriteCallbackOps[S]): ExternalVar[A] =
//      new ExternalVar(lens get s, $ modState lens.set(_))
//  }
//
//  @inline implicit final class MonocleReactReusableVarObjOps(private val x: ReusableVar.type) extends AnyVal {
//    def at[S, A: Reusability](lens: Lens[S, A])(s: S, $: CompState.WriteCallbackOps[S]): ReusableVar[A] =
//      new ReusableVar[A](lens get s, ReusableFn($ modState lens.set(_)))
//  }

  def main(): Unit = {
    val tgt = dom.document.getElementById("target")
    ReactDOM.render(Table.Comp(), tgt)
  }

  // ===================================================================================================================

  /**
    * Preview available:
    * - when editing and focused and (dirty or has been edited since receiving focus)
    */
  object PreviewLogic {

    case class FocusData[+K](key: K, changedSinceFocus: Boolean)

    class PreviewStuff[S, E, K]($: CompState.WriteAccess[S],
                         focusLens: Lens[S, Option[FocusData[K]]]
                         //,isDirty: (S, E) => Boolean
                         )
                        (implicit EK  : Equal[K]){

      private val hasKey: K => FocusData[K] => Boolean =
        if (EK.equalIsNatural)
          k => _.key == k
        else
          k => fi => EK.equal(fi.key, k)

      def onFocus(k: K): Callback =
        $.modState(s =>
          if (focusLens.get(s) exists hasKey(k))
            s
          else
            focusLens.set(Some(FocusData(k, false)))(s))

      def onBlur(k: K): Callback =
        $.modState(s =>
          if (focusLens.get(s) exists hasKey(k))
            focusLens.set(None)(s)
          else
            s)

      def onEdit(k: K): Callback =
        $.modState(s =>
          if (!focusLens.get(s).exists(i => i.changedSinceFocus && hasKey(k)(i)))
            focusLens.set(Some(FocusData(k, true)))(s)
          else
            s)

      def showPreview(focusData: Option[FocusData[K]], isDirty: => Boolean): Boolean =
        focusData.exists(_.changedSinceFocus || isDirty)

      def forChild(k: K, fi: Option[FocusData[K]]): PreviewForChild[K] =
        new PreviewForChild[K] {
          override val focusData: Option[FocusData[K]] =
            fi.filter(hasKey(k))
          override def showPreview(isDirty: => Boolean): Boolean =
            PreviewStuff.this.showPreview(focusData, isDirty)
          override def onFocus = PreviewStuff.this onFocus k
          override def onBlur  = PreviewStuff.this onBlur  k
          override def onEdit  = PreviewStuff.this onEdit  k
        }
    }

    trait PreviewForChild[+K] {
      val focusData: Option[FocusData[K]]
      def showPreview(isDirty: => Boolean): Boolean
      def onFocus: Callback
      def onBlur: Callback
      def onEdit: Callback

      def editorMods[A](a0: A)(blur: (A, Callback) => A, focus: (A, Callback) => A, edit: (A, Callback) => A): A = {
        var a = a0
        a = blur(a, onBlur)
        a = focus(a, onFocus)
        a = edit(a, onEdit)
        a
      }
    }

  } // PreviewLogic


  /*

  provides means to:
  - start or focus an editor
  - update the edit state
  - clear the edit state
  - abort useless edit on blur

  provide to a child:
  - the last good value
  - the current edit value and a means to update it
  - onBlur hook to abort useless edits
  - cmd to start/focus the editor
  - whether an edit is useless

THIS IS CONFLATING

  - start an editor
  - update the edit state
  - clear the edit state
  - abort useless edit on blur

  provides means to:
  - update the edit state
  - clear the edit state

  provide to a child:
  - the last good value
  - the current edit value and a means to update it
  - onBlur hook to abort useless edits
  - cmd to start/focus the editor
  - whether an edit is useless

   */
  class EditorLogicStuasdasdasdasdasdff[S, V, E]($: CompState.WriteAccess[S],
                                     editLens: Lens[S, Option[E]],
                                     getValue: S => V,
                                     initEdit: V => E,
                                     tryToFocus: Callback){
//                                     isEditUseless: (V, E) => Boolean){

    def startEditor: Callback =
      $.modState(
        s => editLens.modify(_ orElse Some(initEdit(getValue(s))))(s),
        tryToFocus)

    def focus(s: S): Callback = {
      editLens.get(s) match {
        case None    => startEditor
        case Some(_) => tryToFocus
      }
    }

    def forChild(s: S) = {
      val el = editLens

      val value: V =
        getValue(s)

      val edit: ExternalVar[Option[E]] =
        ExternalVar.at(el)(s, $)

      val focusSelf: Callback =
        Callback byName focus(s)

      def onBlur(isEditUseless: (V, E) => Boolean): Callback =
          Callback.ifTrue(
            edit.value.exists(isEditUseless(value, _)),
            $.modState(el set None))
      }
  }
  class EditorLogicStuasdasdasdasdasdff2[S, V, E, K]($: CompState.WriteAccess[S],
                                     editLens: K => Lens[S, Option[E]],
                                     getValue: K => S => V,
                                     initEdit: V => E,
                                     tryToFocus: K => Callback){
    def apply(k: K) = {
      new EditorLogicStuasdasdasdasdasdff[S, V, E](
        $,
        editLens(k),
        getValue(k),
        initEdit,
        tryToFocus(k)
      )
    }
  }

  /**
    * Editor opens:
    * - when clicked
    * - when navigated to by KB <-- NOPE
    *
    * Editor closes:
    * - on commit (enter)
    * - on abort (escape)
    * - when loses focus and there is no change
    */
  class EditorLogicStuff[S, V, E, K]($: CompState.WriteAccess[S],
                                     editLens: K => Lens[S, Option[E]],
                                     getValue: (S, K) => V,
                                     initEdit: V => E,
                                     tryToFocus: K => Callback,
                                     isEditUseless: (V, E) => Boolean) {

    def startEditor(k: K): Callback =
      $.modState(
        s => editLens(k).modify(_ orElse Some(initEdit(getValue(s, k))))(s),
        tryToFocus(k))

    def focus(k: K)(s: S): Callback = {
      editLens(k).get(s) match {
        case None    => startEditor(k)
        case Some(_) => tryToFocus(k)
      }
    }

    def forChild(s: S, k: K) =
      new EditorStuffForChild[V, E] {
        val el = editLens(k)
        override val value: V =
          getValue(s, k)
        override val edit: ExternalVar[Option[E]] =
          //ExternalVar(el get s)($ modState el.set(_))
          ExternalVar.at(el)(s, $)
        override val focusSelf: Callback =
          Callback byName focus(k)(s)
        override val onBlur: Callback =
          Callback byName Callback.ifTrue(edit.value.exists(isEditUseless(value, _)), $.modState(el set None))
      }
  }

  trait EditorStuffForChild[V, E] {
    val value: V
    val edit: ExternalVar[Option[E]]

    /** Will start editor if required. */
    val focusSelf: Callback

    val onBlur: Callback

    def abort: Callback =
      edit set None

    def onChange(e: E): Callback =
      edit set Some(e)
  }

  type CB_OnSuccess = Callback

  /**
    * For a bunch of rows and columns:
    *
    * Each:
    * - entire row
    * - cell (i.e. row*col)
    * can have a status:
    * - locked
    * - failed
    *
    * A renderer for when locked.
    * A renderer for when failure has occurred.
    * The logic to wrap a remote call so that it
    * - locks
    * - on remote success, clears the status
    * - on remote success, clears the edit state <---------------- ??????
    * - on remote failure, sets the failure status
    * - is retryable by the failure status
    *
    * Out of scope
    * ============
    * Checking for no-ops (comparing edit value against saved value) to avoid calls.
    *
    * Usage
    * =====
    * When rendering a row, get the row status. -- R => RowStatus
    * When rendering a cell, get the cell status. -- R => C => CellStatus
    * Wrap a remote call -- ∀ f: RemoteFn. (f, f.INPUT, f.OUTPUT => CB, Failed[f.FAILURE] => CB) => CB ?
    * ---- Take 2. ((CBˢ, F => CBᶠ) => CB¹) => CB²
    */
  object RemoteDataStuff {
    import RemoteDataStuff_NoRows_JustCells.{Status, genericWrapRemoteCall, ParentState => CellStates}

    type RowStatus[+F] = Status[F]

    case class RowState[C, +F](rowStatus: Option[RowStatus[F]], cells: CellStates[C, F])

    type ParentState[R, C, +F] = Map[R, RowState[C, F]]
    def initParentState[R]: ParentState[R, Any, Any] = Map.empty

    class InBackendR[S, R, C, F]($: CompState.WriteAccess[S],
                                 stateLens: Lens[S, ParentState[R, C, F]]) {

      private val emptyRowState = RowState[C, F](None, Map.empty)
      private val rowState_rowStatus = monocle.macros.GenLens[RowState[C, F]](_.rowStatus)
      private val rowState_cells = monocle.macros.GenLens[RowState[C, F]](_.cells)

      private def rowState(r: R): Lens[S, RowState[C, F]] = {
        import monocle._, Monocle._
        stateLens ^|-> Lens[ParentState[R, C, F], RowState[C, F]](
          _.getOrElse(r, emptyRowState)
        )(
          n => _.updated(r, n)
        )
      }

      private def cellStatesLens(r: R): Lens[S, CellStates[C, F]] =
        rowState(r) ^|-> rowState_cells

      def getRowState(r: R)(s: S): RowState[C, F] =
        rowState(r).get(s)

      def getRowStatus(r: R)(s: S): Option[RowStatus[F]] =
        getRowState(r)(s).rowStatus

      def inBackendC(r: R) =
        new RemoteDataStuff_NoRows_JustCells.InBackend[S, C, F]($, cellStatesLens(r))

      def getRowAll(r: R)(s: S) = {
        val l = rowState(r)
        val rs = l get s
        (rs, new RemoteDataStuff_NoRows_JustCells.InBackend[S, C, F]($, l ^|-> rowState_cells))
      }

      def wrapRemoteCall(r: R, call: (CB_OnSuccess, F => Callback) => Callback): Callback = {
        val l = rowState(r) ^|-> rowState_rowStatus
        genericWrapRemoteCall[F]($ modState l.set(_), call)
      }
    }

  }

  object RemoteDataStuff_NoRows_JustCells {
    sealed trait Status[+F]
    case object Locked extends Status[Nothing]
    case class Failed[F](failure: F, retry: Callback, resumeEdit: Callback) extends Status[F] {
      def retryButton = <.button("Retry", ^.onClick --> retry)
      def resumeEditButton = <.button("Cancel", ^.onClick --> resumeEdit)
    }

    type ParentState[C, +F] = Map[C, Status[F]]
    def initParentState[C]: ParentState[C, Any] = Map.empty

    def renderLocked = <.div("LOCKED, MATE.")

    def genericWrapRemoteCall[F](setStatus: Option[Status[F]] => Callback,
                                 call: (CB_OnSuccess, F => Callback) => Callback): Callback = {
      val clearStatus = setStatus(None)
      def onSuccess = clearStatus
      def onFailure: F => Callback = f => setStatus(Some(Failed(f, Callback byName doIt, clearStatus)))
      lazy val doIt = call(onSuccess, onFailure) >> setStatus(Some(Locked))
      doIt
    }

    class InBackend[S, C, F]($: CompState.WriteAccess[S],
                             stateLens: Lens[S, ParentState[C, F]]
                             //,renderFailed: Failed[F] => ReactElement?
                            ) {
      private type M = ParentState[C, F]
      private def cellLens(c: C): Lens[S, Option[Status[F]]] = {
        import monocle._, Monocle._
        stateLens ^|-> at(c)
      }

      def getStatus(c: C)(s: S): Option[Status[F]] =
        cellLens(c) get s

      def wrapRemoteCall(c: C, call: (CB_OnSuccess, F => Callback) => Callback): Callback = {
        val l = cellLens(c)
        genericWrapRemoteCall[F]($ modState l.set(_), call)
      }

//      def forChild(s: S, c: C) =
//        new ForChild[C, F] {
//          override def status: Option[Status[F]] =
//            getStatus(c)(s)
//          override def wrapRemoteCall(call: (CB_OnSuccess, F => Callback) => Callback): Callback =
//            InBackend.this.wrapRemoteCall(c, call)
//        }
    }

//    trait ForChild[C, F] {
//      def status: Option[Status[F]]
//      def wrapRemoteCall(call: (CB_OnSuccess, F => Callback) => Callback): Callback
//    }

  }

  // ===================================================================================================================
  // ===================================================================================================================
  object Temp {
    def genericWrapRemoteCall[F](setStatus: Option[Status[F]] => Callback,
                                 call: (CB_OnSuccess, F => Callback) => Callback): Callback = {
      val clearStatus = setStatus(None)
      def onSuccess = clearStatus
      def onFailure: F => Callback = f => setStatus(Some(Failed(f, Callback byName doIt, clearStatus)))
      lazy val doIt = call(onSuccess, onFailure) >> setStatus(Some(Locked))
      doIt
    }

    sealed trait Status[+F]
    case object Locked extends Status[Nothing]
    case class Failed[F](failure: F, retry: Callback, resumeEdit: Callback) extends Status[F] {
      def retryButton = <.button("Retry", ^.onClick --> retry)
      def resumeEditButton = <.button("Cancel", ^.onClick --> resumeEdit)
    }

    object ZeroD {
      type State[+F] = Option[Status[F]]
      def initState: State[Nothing] = None

      class BackendZero[S, F]($: CompState.WriteAccess[S], stateLens: Lens[S, State[F]]) {
        def getStatus(s: S): Option[Status[F]] =
          stateLens get s
        def wrapRemoteCall(call: (CB_OnSuccess, F => Callback) => Callback): Callback =
          genericWrapRemoteCall[F]($ modState stateLens.set(_), call)
      }
    }

    object OneD {
      type State[C, +F] = Map[C, Status[F]]
      def initState[C]: State[C, Nothing] = Map.empty

      class BackendOne[S, C, F]($: CompState.WriteAccess[S], stateLens: Lens[S, State[C, F]]) {
        private type M = State[C, F]

        private def cellLens(c: C): Lens[S, Option[Status[F]]] = {
          import monocle._, Monocle._
          stateLens ^|-> at(c)
        }

        def apply(c: C) =
          new ZeroD.BackendZero[S, F]($, cellLens(c))

//        def getStatus(c: C)(s: S): Option[Status[F]] =
//          cellLens(c) get s
//
//        def wrapRemoteCall(c: C, call: (CB_OnSuccess, F => Callback) => Callback): Callback = {
//          val l = cellLens(c)
//          genericWrapRemoteCall[F]($ modState l.set(_), call)
//        }
      }
    }

    object TwoD {
      type RowStatus[+F] = Status[F]
      import OneD.{State => CellStates}

      case class RowState[C, +F](rowStatus: Option[RowStatus[F]], cells: CellStates[C, F])

      type State[R, C, +F] = Map[R, RowState[C, F]]
      def initState[R]: State[R, Any, Any] = Map.empty

      class BackendTwo[S, R, C, F]($: CompState.WriteAccess[S],
                                   stateLens: Lens[S, State[R, C, F]]) {

        private val emptyRowState = RowState[C, F](None, Map.empty)
        private val rowState_rowStatus = monocle.macros.GenLens[RowState[C, F]](_.rowStatus)
        private val rowState_cells = monocle.macros.GenLens[RowState[C, F]](_.cells)

        private def rowState(r: R): Lens[S, RowState[C, F]] = {
          import monocle._, Monocle._
          stateLens ^|-> Lens[State[R, C, F], RowState[C, F]](
            _.getOrElse(r, emptyRowState)
          )(
            n => _.updated(r, n)
          )
        }

        private def cellStatesLens(r: R): Lens[S, CellStates[C, F]] =
          rowState(r) ^|-> rowState_cells

        def getRowState(r: R)(s: S): RowState[C, F] =
          rowState(r).get(s)

        def getRowStatus(r: R)(s: S): Option[RowStatus[F]] =
          getRowState(r)(s).rowStatus

        def apply(r: R) =
          new OneD.BackendOne[S, C, F]($, cellStatesLens(r))

        def getRowAll(r: R)(s: S) = {
          val l = rowState(r)
          val rs = l get s
          (rs, new OneD.BackendOne[S, C, F]($, l ^|-> rowState_cells))
        }

        def wrapRemoteCall(r: R, call: (CB_OnSuccess, F => Callback) => Callback): Callback = {
          val l = rowState(r) ^|-> rowState_rowStatus
          genericWrapRemoteCall[F]($ modState l.set(_), call)
        }
      }


    }


  }
  // ===================================================================================================================
  // ===================================================================================================================

  // ===================================================================================================================
  import PreviewLogic._

  object Table {
    val sampleData = Vector[String](
      "blah [blah] #1",
      "blah",
      "blah blah",
      "[blah] blah [blah]")

    val Id = "FocusPreviewExperiment"

    @Lenses
    case class State(values: Vector[String],
                     editorStates: Map[Int, String],
                     focus: Option[FocusData[Int]],
                     remoteState: RemoteDataStuff_NoRows_JustCells.ParentState[Int, String]
                    )

    object State {
      import monocle._, Monocle._

      def forValue(i: Int): Optional[State, String] =
        State.values ^|-? index(i)

      def forRow(i: Int): Lens[State, Option[String]] =
        State.editorStates ^|-> at(i)
    }

    class Backend($: BackendScope[Unit, State]) {
      val FM = new PreviewStuff[State, String, Int]($, State.focus)
      val E = new EditorLogicStuff[State, String, String, Int]($, State.forRow, _.values(_), identity, tryToFocus, _ == _)
      val RS = new RemoteDataStuff_NoRows_JustCells.InBackend[State, Int, String]($, State.remoteState)

      def ref(i: Int) = Ref.to(Row.Comp, "row_" + i)

      def getRowComp(i: Int) =
        CallbackTo(ref(i)($).get)

      def getRowEditor(i: Int): CallbackTo[js.UndefOr[dom.html.Input]] =
        getRowComp(i) map (c => c.backend.ref(c))

      def tryToFocus(i: Int): Callback =
        getRowEditor(i).flatMap(_.tryFocus)

      def moveFocus(s: State, i: Int): Callback = {
        val newIndex = Util.fitCollectionIndex(i, s.values.length)
        E.focus(newIndex)(s)
      }

      def render(s: State) =
        <.table(
          ^.id := Id,
          <.tbody(
            s.values.zipWithIndex.map { case (v, i) =>

//              val dt = FM.dataThingy(s, i)
              val fc = FM.forChild(i, s.focus)

              val lens = State.forRow(i)
              val focusUp  : Callback = moveFocus(s, i-1)
              val focusDown: Callback = moveFocus(s, i+1)

              val commitForReal: String => Callback =
                n => $.modState(lens.set(None) compose State.forValue(i).set(n))

              val commitToFakeServer: String => Callback =
                n => RS.wrapRemoteCall(i, (succ, onFail) =>
                  CallbackTo[Callback] {
                    println(s"Server responded to [$n].")
                    if (n.contains("xxx"))
                      onFail("Fake failure!!")
                    else
                      commitForReal(n) >> succ
                  }
                  .flatten
                  .delayMs(2000).void << Callback.log("Fake-Calling server...")
                )

              val content: ReactElement =
                RS.getStatus(i)(s) match {
                  case None =>
                    val rp = Row.Props(E.forChild(s, i), fc, commitToFakeServer, focusUp, focusDown)
                    Row.Comp.withRef(ref(i))(rp)
                  case Some(RemoteDataStuff_NoRows_JustCells.Locked) =>
                    <.td(RemoteDataStuff_NoRows_JustCells.renderLocked)
                  case Some(f: RemoteDataStuff_NoRows_JustCells.Failed[String]) =>
                    <.td(f.failure, f.retryButton, f.resumeEditButton)
                }

              <.tr(^.key := i, content)
            }))
    }

    val Comp = ReactComponentB[Unit]("Outer")
      .initialState(State(sampleData, Map.empty, None, Map.empty))
      .renderBackend[Backend]
      .buildU
  }

  // ====================================================================================================
  object Row {

    case class Props(editStuff: EditorStuffForChild[String, String],
                     preview: PreviewForChild[Any],
                     commit: String => Callback, focusUp: Callback, focusDown: Callback) {
      def value = editStuff.value
      def edit = editStuff.edit
      def startEditor = editStuff.focusSelf
    }

    object SimpleParser {
      val token = """^(.*?)\[([^\[]+?)\](.*)$""".r

      def append(q: Vector[ReactTag], s: String) =
        if (s.isEmpty) q else q :+ <.span(s)

      @scala.annotation.tailrec
      def go(s: String, acc: Vector[ReactTag]): Vector[ReactTag] = {
        val m = token.pattern.matcher(s)
        if (m.matches) {
          var q = append(acc, m group 1)
          q :+= <.span(^.color := "red", ^.backgroundColor := "#ddd", ^.padding := "0 6px", m group 2)
          go(m group 3, q)
        }
        else
          append(acc, s)
      }

      def apply(s: String): ReactTag =
        <.span(go(s, Vector.empty): _*)
    }

    // Values here correspond to values in CSS in index.html
//    val tg = Addons.ReactCssTransitionGroup("fadeanim", enterTimeout = 110, leaveTimeout = 110, component = "div")

    class Backend($: BackendScope[Props, Unit]) {

      val ref = Ref[dom.html.Input]("i")

      def render(p: Props): ReactElement = {

        val inner = p.edit.value match {
          case None =>
            SimpleParser(p.value)

          case Some(es) =>
            def onKey(e: ReactKeyboardEventI): Callback =
              CallbackOption.keyCodeSwitch(e) {
                case KeyCode.Escape => p.editStuff.abort
                case KeyCode.Enter => p commit es
                case KeyCode.Down => p.focusDown
                case KeyCode.Up => p.focusUp
              }

//            val tagMod = p.preview.editorMods(EmptyTag)(
//              blur  = (t, cb) => t + (^.onBlur   --> (cb >> p.editStuff.onBlur)),
//              focus = (t, cb) => t + (^.onFocus  --> cb),
//              edit  = (t, cb) => t + (^.onChange ==> ((e: ReactEventI) =>
//                cb >> p.editStuff.onChange(e.target.value)
//                )))

            val input =
              <.input(
                ^.`type` := "text",
                ^.backgroundColor := (if (p.preview.focusData.isDefined) "#ffc" else "#f2f2d6"),
                ^.ref := ref,
                ^.value := es,
                ^.onKeyDown ==> onKey,
                ^.onBlur   --> (p.preview.onBlur >> p.editStuff.onBlur),
                ^.onFocus  --> p.preview.onFocus,
                ^.onChange ==> ((e: ReactEventI) => p.preview.onEdit >> p.editStuff.onChange(e.target.value)))

            def preview =
              ReactCollapse(p.preview.showPreview(es != p.value))(
                <.div(^.key := 9,
                  <.div("Preview:"),
                  <.div(^.backgroundColor := "#efe", SimpleParser(es))))

            <.div(input, preview)
        }

        <.td(
          ^.border := "solid 1px #444",
          ^.padding := "0.5ex 1ex",
          ^.width := "30ex",
          ^.onClick --> p.startEditor,
//          p.focusOnClick,
          inner)
      }
    }

    val Comp = ReactComponentB[Props]("Row")
      .renderBackend[Backend]
      .build
  }
}
