package shipreq.webapp.base.event

import shipreq.webapp.base.hash.HashRec

/**
 * A verified event is an event that has been validated by the server, proven applicable, and retains hashes expected
 * of the Project after application.
 */
case class VerifiedEvent(event: Event, hashRecs: HashRec.Collection)