package com.beardedlogic.usecase.snippet

import net.liftweb.util.Helpers._
import net.liftweb.util.CssSel
import com.beardedlogic.usecase.app.{AppSiteMap, RequestVars, DI}
import com.beardedlogic.usecase.feature.uc.{UseCase, UseCasePersistence}
import com.beardedlogic.usecase.lib.Locks
import com.beardedlogic.usecase.lib.Types._
import com.beardedlogic.usecase.feature.publish.{Input, HtmlPublisher}
import AppSiteMap.Implicits._

object ReadOwnUcs {

  def render: CssSel = {
    val project = RequestVars.Project.get.value

    val ucs: List[UseCase] =
      DI.DaoProvider.withTransaction(dao =>
        Locks.UseCaseNumbers.read(project)(lock =>
          UseCasePersistence.loadAll(project, dao, lock)))

    if (ucs.isEmpty)
      "a [href]" #> AppSiteMap.Project.relativeUrl(project)
    else {
      val i = Input(None, ucs)
      val o = HtmlPublisher.publish(i)
      "* *" #> o
    }
  }
}
