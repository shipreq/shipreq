package shipreq.webapp.base.test

import shipreq.base.util.univeq._
import shipreq.webapp.base.data._

object DataTestExt {

  implicit class ProjectTestExt(private val p: Project) extends AnyVal {

    def useCaseStepsDeletableRestorable(liveFilter: Live): Iterator[UseCaseStep.Focus] =
      p.content.reqs.useCases.imap.valuesIterator
        .filter(_.liveUC is Live)
        .flatMap { uc =>
          val root = uc.rootStep.id
          uc.stepIterator
            .map(s => p.content.reqs.useCases.focusStep(s.id))
            .filter(f => f.live is liveFilter && f.id !=* root)
        }

    def useCaseStepsDeletable = useCaseStepsDeletableRestorable(Live)
    def useCaseStepsRestorable = useCaseStepsDeletableRestorable(Dead)
  }

}
