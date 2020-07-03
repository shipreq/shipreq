package shipreq.webapp.base.feature.autocomplete

import shipreq.webapp.base.jsfacade.TextComplete.Strategy

package object strategies extends strategies.QueryModule {

  type Strategies = Vector[Strategy[_]]

  implicit def autoLiftTextCompleteStrategy(s: Strategy[_]): Strategies =
    (Vector.empty: Strategies) :+ s

}
