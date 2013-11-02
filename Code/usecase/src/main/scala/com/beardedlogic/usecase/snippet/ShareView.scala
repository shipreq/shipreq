package com.beardedlogic.usecase.snippet

import net.liftweb.util.Helpers._
import scala.xml.NodeSeq
import scalaz.std.list.listInstance
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
  case class ZeroUcs(header: NodeSeq) extends PostAuthPage
  case class ShowUcs(content: NodeSeq) extends PostAuthPage

  type OSP = Option[(Share, Project)]

  // TODO opens 2 DB connections

  def loadIfCurrentUserIsOwner: OSP =
    for {
      _      <- currentUser
      (s, p) <- daoProvider.withSession(_.findShareAndProject(token))
      _      <- PermissionCheck.userCan.readAndUpdate(p).toOption
    } yield (s, p)

  def pageFor(osp: OSP): Page =
    osp match {
      case None         => PasswordRequired
      case Some((s, p)) => postAuthPage(s, p)
    }

  def postAuthPage(s: Share, p: Project): PostAuthPage = {
    val f = UcFilter.fromJson(s.ucFilterJson)
    val ucs = loadUcs(p, f)
    val h = DocHeader(s.name, s.preface)
    val i = new Input(Some(h), ucs)
    val q = new HtmlPublisher(i)

    if (ucs.isEmpty)
      ZeroUcs(q.optionalDocHeader)
    else
      ShowUcs(q.doc)
  }

  var loadUcs = (p: ProjectId, f: UcFilter) =>
    DI.DaoProvider.withTransaction(dao =>
      Locks.UseCaseNumbers.read(p)(lock =>
        UseCasePersistence.loadAll(p).filter(UcFilter(f)).run(dao, lock)))

  def initialPage = pageFor(loadIfCurrentUserIsOwner)

  def render = renderPage(initialPage)

  def renderPage(page: Page) =
    page match {
      case PasswordRequired =>
        "#passwordRequired ^^" #> ""

      case ZeroUcs(h) =>
        "#share-view-none ^^" #> "" andThen ".header" #> h

      case ShowUcs(o) =>
        "#share-view ^^" #> "" andThen ".ucs-published *" #> o
    }

}
