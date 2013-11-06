package com.beardedlogic.usecase.snippet

import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._
import com.beardedlogic.usecase.app.RequestVars
import com.beardedlogic.usecase.feature.UcFilter
import com.beardedlogic.usecase.feature.validation.Validator
import com.beardedlogic.usecase.lib.NoticeFlash
import com.beardedlogic.usecase.lib.Types.Json
import com.beardedlogic.usecase.util.HtmlTransformExt.IfCssSel

/**
 * Allows a user to edit a new share.
 *
 * @since 6/11/2013
 */
class ShareEdit extends ShareCreateBase {

  val share = RequestVars.Share.value
  def projectId = share.projectId

  def render = (
    render2(share.ucFilter)
      & "#shareName [value]" #> share.name
      & IfCssSel(share.preface.isDefined)("#preface *" #> share.preface.get)
    )

  def onSubmit(ucFilterJson: () => Json[UcFilter]): JsCmd = {
    val v = Validator.Ap.apply2(nameV, prefaceV)(Tuple2.apply)
    ifValid(v)(r => {
      val (name, preface) = r
      daoProvider.withSession(_.updateShare(share, name, preface, ucFilterJson()))
      NoticeFlash.notices.addS("Share updated successfully.")
      goBackToShareList()
    })
  }
}
