package shipreq.webapp.base.data

/** The user that first created a project.
 *
 * This is a thing because proper access controls weren't added until Phase 3, and having this immutable fact allows
 * us to populate `project.access` correctly before any project-access events are received.
 */
final case class ProjectCreator(userId: UserId.Public)

object ProjectCreator {
  implicit def univEq: UnivEq[ProjectCreator] = UnivEq.derive
}
