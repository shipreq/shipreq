package shipreq.webapp.client.project.app.pages.config.reqtypes

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.scalajs.react.React
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.UiText.FieldNames
import shipreq.webapp.base.data.ReqType
import shipreq.webapp.base.ui.semantic.{Colour, Icon}
import shipreq.webapp.client.project.app.Style.{reqTypeConfig => *}

private[reqtypes] object Shared {

  val implication: VdomNode =
    React.Fragment(
      FieldNames.implication,
      Icon.HelpCircle.withColour(Colour.Grey).tag(
        *.implicationHelp,
        ^.title :=
          """
            |Determines whether reqs of certain req types need to be implied by other reqs, or not. Implication justifies the existence of reqs, and provides tracability.
            |
            |Optional means a req type is top-level or high-level, and not derived from, or dependent on anything else.
            |Mandatory means a req type is always downstream of something else, often because it's the result of analysis. Reqs without valid implication are highlighted as issues.
            |
            |Example: business rule = Optional, because it's a description of the real-world; you don't need to justify it.
            |Example: solution idea = Mandatory, implied by reqs and/or problems it proposes to address.
            |""".stripMargin.trim))

  def renderOldMnemonics(rt: ReqType): TagMod =
    MutableArray(rt.oldMnemonics.iterator.map(_.value))
      .sort
      .iterator()
      .map(mnemonic => <.span(*.deadMnemonic, mnemonic))
      .mkTagMod(", ")
}
