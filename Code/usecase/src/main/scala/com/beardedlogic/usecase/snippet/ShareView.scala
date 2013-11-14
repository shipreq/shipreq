package com.beardedlogic.usecase.snippet

import net.liftweb.http.js.{JsCmds, JsCmd}
import net.liftweb.http.{SessionVar, SHtml}
import net.liftweb.util.Helpers._
import org.joda.time.DateTime
import scala.xml.{Text, NodeSeq}

import com.beardedlogic.usecase.app.{AppConfig, DI}
import com.beardedlogic.usecase.db.Share
import com.beardedlogic.usecase.feature.UcFilter
import com.beardedlogic.usecase.feature.publish.{DocHeader, HtmlPublisher, Input}
import com.beardedlogic.usecase.feature.uc.persist.{UseCaseSaveCheckpoint, UseCasePersistence}
import com.beardedlogic.usecase.lib.ScalazSubset._
import com.beardedlogic.usecase.lib.Types._
import com.beardedlogic.usecase.lib.{LogShareView, Locks, SingleOpStatefulSnippet}
import com.beardedlogic.usecase.security.Permissions
import com.beardedlogic.usecase.util.HtmlTransformExt.ajaxSubmitOnClick
import ShareView._

object ShareView {
  type AuthMap = Map[ShareUrlToken, (DateTime, String @@ Hashed)]
  object AuthMapVar extends SessionVar[AuthMap](Map.empty)

  sealed trait Page
  sealed trait PostAuthPage extends Page
  case object PasswordRequired extends Page
  case class ZeroUcs(header: NodeSeq) extends PostAuthPage
  case class ShowUcs(content: NodeSeq) extends PostAuthPage

  type LoadResult = Option[Share]
}

class ShareView(token: ShareUrlToken) extends SingleOpStatefulSnippet {

  // TODO opens too many separate DB connections

  def render = renderPage(initialPage)

  def initialPage = pageFor(loadIfAlreadyAuth orElse loadIfCurrentUserIsOwner)

  def loadIfAlreadyAuth: LoadResult =
    for {
      (authTime, authPass) <- AuthMapVar.get.get(token)
                              if authTime <= AppConfig.ShareViewAuthPeriod
      (s, ps)              <- daoProvider.withSession(_ findShareAndPassword token)
                              if ps.hashedPassword == authPass
    } yield s

  def loadIfCurrentUserIsOwner: LoadResult =
    for {
      _       <- currentUser
      (s, pr) <- daoProvider.withSession(_ findShareAndProject token)
      _       <- Permissions.viewShare.using(project = Some(pr), share = Some(s)).pass
    } yield s

  def pageFor(o: LoadResult): Page =
    o match {
      case None    => PasswordRequired
      case Some(s) => postAuthPage(s)
    }

  def postAuthPage(s: Share): PostAuthPage = {
    val ucs = loadUcs(s.projectId, s.ucFilter).map(_.ucAndRev)
    val h = DocHeader(s.name, s.preface)
    val i = new Input(Some(h), ucs)
    val q = new HtmlPublisher(i)

    if (ucs.isEmpty)
      ZeroUcs(q.optionalDocHeader)
    else
      ShowUcs(q.doc)
  }

  var loadUcs: (ProjectId, UcFilter) => List[UseCaseSaveCheckpoint] = (p, f) =>
    DI.DaoProvider.withTransaction(dao =>
      Locks.UseCaseNumbers.read(p)(lock =>
        UseCasePersistence.loadAll(p).filter(UcFilter(f)).run(dao, lock)))

  def renderPage(page: Page) =
    page match {
      case PasswordRequired =>
        "#passwordRequired ^^" #> "" andThen renderPasswordForm

      case ZeroUcs(h) =>
        "#share-view-none ^^" #> "" andThen ".header" #> h

      case ShowUcs(o) =>
        "#share-view ^^" #> "" andThen "#X" #> o
    }

  def renderPasswordForm = {
    var passwordInput = ""
    "#password" #> SHtml.onSubmit(passwordInput = _) &
      ":submit" #> ajaxSubmitOnClick(() => onSubmitPassword(passwordInput))
  }

  def onSubmitPassword(password: String): JsCmd = {
    val possibleJs = for {
      (s, p) <- daoProvider.withSession(_ findShareAndPassword token)
      if p matches password
    } yield {
      onAuth(s, p.hashedPassword)
      JsCmds.Reload
    }
    possibleJs getOrElse jsShowError(Text("Access denied. Please verify the URL and password."))
  }

  def onAuth(s: Share, hashedPassword: String @@ Hashed): Unit = {
    val newAuthEntry = (token, (DateTime.now, hashedPassword))
    AuthMapVar.atomicUpdate(_ + newAuthEntry)

    statLogger ! LogShareView(s)
  }
}
