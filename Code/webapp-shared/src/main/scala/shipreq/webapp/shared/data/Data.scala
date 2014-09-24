package shipreq.webapp.shared.data

sealed trait Alive
case object Alive extends Alive
case object Dead extends Alive
