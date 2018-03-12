package shipreq.webapp.base.feature

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Px
import org.scalajs.dom.html
import shipreq.webapp.base.jsfacade.TextComplete
import shipreq.webapp.base.ui.AutosizeTextarea

/**
  * Usage
  * =====
  *
  * 1. `import AutoCompleteFeature._`
  * 2. Have your component's backend extends `AutoComplete.Backend[D]` where D is the dom type.
  * 3. Implement required method(s).
  * 4. Wire up your editor: `^.onBlur --> autoCompleteBlur`
  * 5. Add `.configure(AutoComplete.install)` to your component builder.
  */
object AutoCompleteFeature extends autocomplete.Implicits {

  type AutoCompletable[D] = autocomplete.ForComponent.AutoCompletable[D]
  val  AutoCompletable    = autocomplete.ForComponent.AutoCompletable

  object AutoComplete {
    type Backend[D <: AnyRef] = autocomplete.ForComponent.Backend[D]
    type BackendI             = Backend[html.Input]
    type BackendTA            = Backend[html.TextArea]
    type Ctx[D]               = autocomplete.ForComponent.Ctx[D]
    val  Ctx                  = autocomplete.ForComponent.Ctx
    val  Project              = autocomplete.ProjectStrategies
    val  Strategy             = TextComplete.Strategy
    type Strategy             = TextComplete.Strategy[_]
    type Strategies           = autocomplete.Utils.Strategies
    val  Utils                = autocomplete.Utils

    def install[P, C <: Children, S, B <: Backend[D], D <: AnyRef : AutoCompletable]: ScalaComponent.Config[P, C, S, B] =
      autocomplete.ForComponent.install[P, C, S, B, D]

    /** Most editors just use this */
    trait EditorBackend extends BackendTA {
      protected val pxAutoComplete: Px[Strategies]

      final val editorRef =
        Ref.toScalaComponent(AutosizeTextarea.Component)

      override final val autoCompleteCtx =
        for {
          r <- editorRef.get
          n <- r.withEffectsPure.getDOMNode.map(_.toOption).asCBO
        } yield Ctx(pxAutoComplete.value(), n.domCast[html.TextArea])
    }

  }
}
