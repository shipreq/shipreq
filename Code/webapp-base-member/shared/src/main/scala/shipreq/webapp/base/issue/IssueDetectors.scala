package shipreq.webapp.base.issue

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.nonempty.NonEmptyVector
import shipreq.base.util.Util
import shipreq.webapp.base.data._

object IssueDetectors {
  import IssueDetector.{Increment, Init}

  /** This type exists just so that we can have .values and know it will be kept in sync by AdtMacros.
    */
  private[issue] sealed trait Instance {
    def init: Init => Unit
    def increment: Increment => Unit
    val instance = IssueDetector(init, increment)
  }

  private[issue] case object ConflictingTagDetector extends Instance {
    override def init = i => {
      import i._
      val exclusiveGroups = project.config.tags.exclusiveGroups
      for (r <- project.liveReqIterator()) {
        val tagIds    = project.content.reqTags(r.id)
        val conflicts = Util.uniqueDupsNested(tagIds)(exclusiveGroups)
        for (g <- conflicts)
          add(Issue.ConflictingTags(r.id, g))
      }
    }

    override def increment = i => {
      // remove all/some Issue.ConflictingTags
      // add
    }
  }

  private val instances = AdtMacros.adtValues[Instance]

  val values: NonEmptyVector[IssueDetector] =
    instances.map(_.instance)

  val composite: IssueDetector =
    IssueDetector.compose(values.whole: _*)
}
