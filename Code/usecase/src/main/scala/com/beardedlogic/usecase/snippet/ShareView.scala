package com.beardedlogic.usecase.snippet

import net.liftweb.util.Helpers._
import scala.xml.NodeSeq
import com.beardedlogic.usecase.app.DI
import com.beardedlogic.usecase.db.{Share, Project}
import com.beardedlogic.usecase.feature.UcFilter
import com.beardedlogic.usecase.feature.publish.{DocHeader, HtmlPublisher, Input}
import com.beardedlogic.usecase.feature.uc.persist.UseCasePersistence
import com.beardedlogic.usecase.lib.Types._
import com.beardedlogic.usecase.lib.{Locks, SingleOpStatefulSnippet}
import com.beardedlogic.usecase.security.PermissionCheck

class ShareView(token: ShareUrlToken) extends SingleOpStatefulSnippet {

  sealed trait Page
  sealed trait PostAuthPage extends Page

  case object PasswordRequired extends Page
  case object ZeroUcs extends PostAuthPage
  case class ShowUcs(content: NodeSeq) extends PostAuthPage

  type OSP = Option[(Share, Project)]

  def loadIfCurrentUserIsOwner: OSP =
    for {
      _      <- currentUser
      (s, p) <- daoProvider.withSession(_.findShareAndProject(token))
      _      <- PermissionCheck.userCan.readAndUpdate(p).toOption
    } yield (s, p)

  def initialPage(osp: OSP): Page = {
    osp match {
      case None         => PasswordRequired
      case Some((s, p)) => postAuthPage(s, p)
    }
  }

  def postAuthPage(s: Share, p: Project): PostAuthPage = {
    val filter = UcFilter.fromJson(s.ucFilterJson)
    val ucs =
      DI.DaoProvider.withTransaction(dao =>
        Locks.UseCaseNumbers.read(p)(lock =>
          UseCasePersistence.loadAll(p, UcFilter(filter, _), dao, lock)))

    if (ucs.isEmpty)
      ZeroUcs
    else {
      val h = DocHeader(s.name, s.preface)
      val i = new Input(Some(h), ucs)
      val o = HtmlPublisher.publish(i)
      ShowUcs(o)
    }
  }

  def render =
    initialPage(loadIfCurrentUserIsOwner) match {
      case PasswordRequired =>
        "#passwordRequired ^^" #> ""

      case ZeroUcs =>
        "#share-view-none ^^" #> ""

      case ShowUcs(o) =>
        "#share-view ^^" #> "" andThen ".ucs-published *" #> o
    }

}
