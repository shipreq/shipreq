package shipreq.webapp.snippet

import net.liftweb.http.js.{JsCmds, JsCmd}
import net.liftweb.http.{SessionVar, SHtml}
import net.liftweb.util.Helpers._
import org.joda.time.DateTime
import scala.xml.{Text, NodeSeq}
import scalaz.Need

import shipreq.webapp.app.AppSiteMap.mkTitle
import shipreq.webapp.app.AppConfig
import shipreq.webapp.db.{DaoS, Share}
import shipreq.webapp.feature.UcFilter
import shipreq.webapp.feature.publish.{DocHeader, HtmlPublisher, Input}
import shipreq.webapp.feature.uc.persist.{UseCaseSaveCheckpoint, UseCasePersistence}
import shipreq.webapp.lib.ScalazSubset._
import shipreq.webapp.lib.Types._
import shipreq.webapp.lib.{LogShareView, Locks, SingleOpStatefulSnippet}
import shipreq.webapp.security.Permissions
import shipreq.webapp.util.HtmlTransformExt.ajaxSubmitOnClick
import shipreq.webapp.util.JsExt
import ShareView._

object ShareView {
  type AuthMap = Map[ShareUrlToken, (DateTime, HashedStr)]
  object AuthMapVar extends SessionVar[AuthMap](Map.empty)

  sealed trait Page {
    def title: String
  }
  sealed trait PostAuthPage extends Page
  case object PasswordRequired extends Page {
    override def title = "Password Required"
  }
  case class ZeroUcs(title: String, header: NodeSeq) extends PostAuthPage
  case class ShowUcs(title: String, content: NodeSeq) extends PostAuthPage

  type LoadResult = Option[Share]
}

class ShareView(token: ShareUrlToken) extends SingleOpStatefulSnippet {

  def render = renderPage(initialPage)

  def initialPage =
    pageFor(
      daoProvider.withLazySession(dao =>
        loadIfAlreadyAuth(dao) orElse loadIfCurrentUserIsOwner(dao)))

  def loadIfAlreadyAuth(dao: Need[DaoS]): LoadResult =
    for {
      (authTime, authPass) <- AuthMapVar.get.get(token)
                              if authTime <= AppConfig.ShareViewAuthPeriod
      (s, ps)              <- dao.value.findShareAndPassword(token)
                              if ps.hashedPassword == authPass
    } yield s

  def loadIfCurrentUserIsOwner(dao: Need[DaoS]): LoadResult =
    for {
      _       <- currentUser
      (s, pr) <- dao.value.findShareAndProject(token)
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
      ZeroUcs(h.title, q.optionalDocHeader)
    else
      ShowUcs(h.title, q.doc)
  }

  var loadUcs: (ProjectId, UcFilter) => List[UseCaseSaveCheckpoint] = (p, f) =>
    daoProvider.withTransaction(dao =>
      Locks.UseCaseNumbers.read(p)(lock =>
        UseCasePersistence.loadAll(p).filter(UcFilter(f)).run(dao, lock)))

  def renderPage(page: Page) =
    (page match {
      case PasswordRequired =>
        "#passwordRequired ^^" #> "" andThen renderPasswordForm
      case ZeroUcs(_, h) =>
        "#share-view-none ^^" #> "" andThen ".header" #> h
      case ShowUcs(_, o) =>
        "#share-view ^^" #> "" andThen "#X" #> o
    }) andThen
      setPageTitle(page.title)

  def renderPasswordForm = {
    var passwordInput = ""
    "#password" #> SHtml.onSubmit(passwordInput = _) &
      ":submit" #> ajaxSubmitOnClick(() => onSubmitPassword(passwordInput))
  }

  def onSubmitPassword(password: String): JsCmd = {
    securityProvider.enforceHumanSpeed()

    val possibleJs = for {
      (s, p) <- daoProvider.withSession(_ findShareAndPassword token)
      if p matches password
    } yield {
      onAuthOk(s, p.hashedPassword)
      JsCmds.Reload
    }
    possibleJs getOrElse jsShowError(Text("Access denied. Please verify the URL and password."))
  }

  def onAuthOk(s: Share, hashedPassword: HashedStr): Unit = {
    val newAuthEntry = (token, (DateTime.now, hashedPassword))
    AuthMapVar.atomicUpdate(_ + newAuthEntry)
    statLogger ! LogShareView(s)
  }

  private def setPageTitle(title: String): NodeSeq => NodeSeq =
    (<script type="text/javascript">{JsExt.JsSetPageTitle(mkTitle(title)).toJsCmd}</script> ++ _)
}
