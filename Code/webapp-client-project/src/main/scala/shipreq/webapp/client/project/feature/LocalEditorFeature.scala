package shipreq.webapp.client.project.feature

/*
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.ExternalVar
import monocle.Lens
import monocle.function.At.at
import monocle.std.map.atMap
import MonocleReact._
import org.scalajs.dom.ext.KeyCode
import shipreq.base.util.UnivEq

/**
  * Logic for the common case of having a saved value in memory and allowing the user to switch into edit-mode and edit
  * it.
  *
  * Provided Logic
  * ==============
  * - Get the unedited value.
  * - Get the optional edit value.
  * - Set the edit value.
  * - Start editing.
  * - Focus the editor.
  * - Abort editing.
  * - Abort editing on escape.
  *
  * Out-of-Scope
  * ============
  * - Committing the edit value.
  * - Comparing the edit value with the pre-edit value.
  * - Validation (use the [[EditValidationFeature]] for that).
  */
object LocalEditorFeature {

  class Single[S, V, E]($: CompState.WriteAccess[S],
                        editLens: Lens[S, Option[E]],
                        getValue: S => V,
                        initEdit: (S, V) => E,
                        tryToFocus: Callback) {

    def startOrFocusEditor: Callback =
      $.modState(
        s => editLens.modify(_ orElse Some(initEdit(s, getValue(s))))(s),
        tryToFocus)

    def apply(s: S) =
      new ForChild[V, E] {
        override def value: V =
          getValue(s)

        override val edit: ExternalVar[Option[E]] =
          ExternalVar.at(editLens)(s, $)

        def startOrFocusEditor: Callback =
          Single.this.startOrFocusEditor

        //        override val onBlur: Callback =
        //          Callback byName Callback.ifTrue(edit.value.exists(isEditUseless(value, _)), $.modState(el set None))
      }
  }

  trait ForChild[V, E] {
    def value: V
    val edit: ExternalVar[Option[E]]
    def startOrFocusEditor: Callback

    def abort: Callback =
      edit set None

    def abortOnEscape(e: ReactKeyboardEvent) =
      CallbackOption.keyCodeSwitch(e) {
        case KeyCode.Escape => abort
      }

    def onChange(e: E): Callback =
      edit set Some(e)
  }

  //  class Keyed[S, K, V, E]($: CompState.WriteAccess[S],
  //                          editLens: K => Lens[S, Option[E]],
  //                          getValue: K => S => V,
  //                          initEdit: V => E,
  //                          tryToFocus: K => Callback) {
  //    def apply(k: K) =
  //      new Single[S, V, E](
  //        $,
  //        editLens(k),
  //        getValue(k),
  //        initEdit,
  //        tryToFocus(k))
  //  }

}
*/