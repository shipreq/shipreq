package shipreq.webapp.snippet

import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._
import scala.xml.NodeSeq
import shipreq.base.util.TaggedTypes.JsonStr
import shipreq.webapp.app.RequestVars
import shipreq.webapp.feature.UcFilter
import shipreq.webapp.lib.{FormVar, NoticeFlash}
import shipreq.webapp.util.NonEmptyTemplate
import ShareCreateBase._
import ShareEditConsts._

object ShareEditConsts {

  val editFormXml: NodeSeq = {
    val createForm = NonEmptyTemplate.load("loggedin/share-create").extract("form")
                     .assertHeadType("form").assertSingleHead().get
    val transform = (
      ".password .form-group" #> "" &
      ":submit *" #> "Update Share"
    )
    transform(createForm)
  }

  val editForm = FormVar.merge(nameFV, prefaceFV)(Tuple2.apply)
}

/**
 * Allows a user to edit a new share.
 *
 * @since 6/11/2013
 */
class ShareEdit extends ShareCreateBase {

  val share = RequestVars.Share.value
  def projectId = share.projectId
  var vars: editForm.Var = (share.name, share.preface.getOrElse(""))

  def render =
    "#edit-form" #> editFormXml andThen (editForm.csssel(vars, vars = _) & render2(share.ucFilter))

  def onSubmit(ucFilterJson: () => JsonStr[UcFilter]): JsCmd =
    ifValid(editForm validate vars)(t => {
      val (name, preface) = t
      daoProvider.withSession(_.updateShare(share, name, preface, ucFilterJson()))
      NoticeFlash.notices.addS("Share updated successfully.")
      goBackToShareList()
    })
}
