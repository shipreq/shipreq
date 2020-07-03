package shipreq.webapp.base.feature

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Px
import org.scalajs.dom.html
import shipreq.webapp.base.jsfacade.TextComplete
import shipreq.webapp.base.ui.AutosizeTextarea

/**
  * Usage #1: One field (of any kind) in your component
  * ===================================================
  *
  * 1. `import shipreq.webapp.base.feature.AutoCompleteFeature._`
  *
  * 2. Have your component's backend extends `AutoComplete.BackendI` for `<input>`, or `BackendTA` for `<textarea>`
  *
  * 3. Add to backend:
  *    ```
  *    override val autoCompleteCtx: CallbackOption[AutoCompleteCtx] =
  *      inputDomRef.get.map(AutoCompleteCtx(pxAutoComplete.value(), _))
  *    ```
  *
  * 4. Add a dom ref to the backend: `private val inputDomRef = Ref[html.Input]`
  *    Add it to the dom using `.withRef`
  *
  * 5. Wire up your editor:
  *    ```
  *    ^.onBlur  --> autoCompleteOnBlur,
  *    ^.onClick ==> autoCompleteOnClick,
  *    ```
  *
  * 6. Add `.configure(AutoComplete.install)` to your component builder.
  *    If you're using an input instead of a textarea use
  *    `.configure(AutoComplete.install(autoCompletableInput))`
  *
  * 7. Add to backend: `private val pxAutoComplete: Px[AutoComplete.Strategies]`
  *    For the implementation, start with `pxProject.map` or `pxProjectConfig.map`,
  *    then use or compose values in `AutoComplete.Project.xxx`
  *
  *
  * Usage #2: Multiple inputs in your component
  * ===========================================
  *
  * 1. `import shipreq.webapp.base.feature.AutoCompleteFeature._`
  *
  * 2. For each input, store the following in your backend:
  *    - `val pxAutoComplete: Px[AutoComplete.Strategies] = ...`
  *    - `val renderWithAutoComplete = AutoComplete.InputComponent(pxAutoComplete.toCallback) _`
  *
  * 3. Wrap the rendering of your input in `renderWithAutoComplete(tagMod => ...)` and
  *    make sure to apply the given `tagMod` to your input tag.
  *
  *
  * Troubleshooting
  * ===============
  *
  * - if autocomplete isn't popping up...
  *   - ensure that you have reuse enabled on all the Pxs leading up to the `Px[AutoComplete.Strategies]`
  */
object AutoCompleteFeature extends autocomplete.Implicits {

  type AutoCompletable[D] = autocomplete.ForComponent.AutoCompletable[D]
  val  AutoCompletable    = autocomplete.ForComponent.AutoCompletable

  object AutoComplete {
    type Backend[D <: html.Element] = autocomplete.ForComponent.Backend[D]
    type BackendI                   = Backend[html.Input]
    type BackendTA                  = Backend[html.TextArea]
    type Ctx[D]                     = autocomplete.ForComponent.Ctx[D]
    val  Ctx                        = autocomplete.ForComponent.Ctx
    val  Project                    = autocomplete.strategies.ProjectStrategies
    val  Strategy                   = TextComplete.Strategy
    type Strategy                   = TextComplete.Strategy[_]
    type Strategies                 = autocomplete.strategies.Strategies
    type Query[A]                   = autocomplete.strategies.Query[A]
    val  Query                      = autocomplete.strategies.Query
    val  InputComponent             = autocomplete.InputComponent

    def install[P, C <: Children, S, B <: Backend[D], D <: html.Element : AutoCompletable] =
      autocomplete.ForComponent.install[P, C, S, B, D]

    /** Most editors just use this */
    trait EditorBackend extends BackendTA {
      protected val pxAutoComplete: Px[Strategies]

      final val editorRef =
        Ref.toScalaComponent(AutosizeTextarea.Component)

      override final val autoCompleteCtx =
        for {
          r <- editorRef.get
          n <- r.withEffectsPure.getDOMNode.map(_.toElement).asCBO
        } yield Ctx(pxAutoComplete.value(), n.domCast[html.TextArea])
    }

  }
}
