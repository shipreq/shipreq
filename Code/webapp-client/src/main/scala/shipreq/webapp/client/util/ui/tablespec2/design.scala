package shipreq.webapp.client.util.ui.tablespec2

import japgolly.scalajs.react.ScalazReact._
import shipreq.webapp.base.validation._
import monocle._
import scalaz.effect.IO
import scalaz._, Scalaz._

/*
  - Have a separate class or fn for each piece of behaviour.
  - Where ∀-types are concerned, rather than polluting the entire type hierarchy consider using abstract type members.
  - Types can be data representation like ADT, maybe impl should be considered separately.
  - Consider possible shape changes of each type.
  - Consider composability of each type.
*/
