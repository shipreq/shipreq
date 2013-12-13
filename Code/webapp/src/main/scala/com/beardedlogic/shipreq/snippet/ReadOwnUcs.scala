package com.beardedlogic.shipreq.snippet

import net.liftweb.util.Helpers._
import net.liftweb.util.CssSel
import com.beardedlogic.shipreq.app.{AppSiteMap, RequestVars, DI}
import com.beardedlogic.shipreq.feature.uc.persist.UseCasePersistence
import com.beardedlogic.shipreq.lib.Locks
import com.beardedlogic.shipreq.lib.Types._
import com.beardedlogic.shipreq.feature.publish.{Input, HtmlPublisher}
import AppSiteMap.Implicits._

object ReadOwnUcs extends DI {

  def render: CssSel = {
    val project = RequestVars.Project.get.value

    val ucs =
      daoProvider.withTransaction(dao =>
        Locks.UseCaseNumbers.read(project)(lock =>
          UseCasePersistence.loadAll(project).run(dao, lock)))
      .map(_.ucAndRev)

    if (ucs.isEmpty)
      "a [href]" #> AppSiteMap.Project.relativeUrl(project)
    else {
      val i = new Input(None, ucs)
      val o = HtmlPublisher.publish(i)
      "* *" #> o
    }
  }
}
