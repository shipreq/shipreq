package shipreq.webapp.base.event

import shipreq.webapp.base.hash.DataHash

/**
 * A verified event is an event that has been validated by the server, proven applicable, and retains a hash expected
 * of the Project after application.
 */
case class VerifiedEvent(hashScheme: DataHash,
                         hash      : Int,
                         event     : Event)