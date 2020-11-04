package shipreq.webapp.member.feature.autocomplete

import japgolly.scalajs.react.Reusability
import org.scalajs.dom.html
import shipreq.webapp.member.feature.autocomplete.ForComponent.AutoCompletable
import shipreq.webapp.member.feature.autocomplete.strategies.Strategies
import shipreq.webapp.member.jsfacade.TextComplete

trait Implicits0 {

  // DANGEROUS!
  implicit val autoCompletableInput: AutoCompletable[html.Input] =
    AutoCompletable(i => new TextComplete.TextArea(i.asInstanceOf[html.TextArea]))
}

object Implicits extends Implicits
trait Implicits extends Implicits0 {

  implicit val reusabilityAutoCompleteStrategies: Reusability[Strategies] =
    Reusability((a, b) => (a eq b) || a.corresponds(b)(_ eq _))

  implicit val reusabilityAutoCompleteOptionStrategies: Reusability[Option[Strategies]] =
    Reusability.option

  implicit val autoCompletableTextarea: AutoCompletable[html.TextArea] =
    AutoCompletable(new TextComplete.TextArea(_))
}
