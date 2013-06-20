package com.beardedlogic.usecase
package snippet

import net.liftweb.http.SHtml
import net.liftweb.http.js.{JsCmds, JsCmd}
import net.liftweb.json.Serialization.{write => jsonWrite}
import net.liftweb.json._
import net.liftweb.util.Helpers._
import net.liftweb.util.{CssSel, ClearClearable}

import lib._
import msg.{JavaScript, Reactor}
import model.DAO
import model.UseCaseSummary

object UseCaseIndex extends SnippetHelpers {

//  val EditTemplateId = "template-edit"
//  val EditTemplateCss = "#" + EditTemplateId
//  val EditTemplate = UseCaseIndexTemplate.extract(EditTemplateId)

  private implicit val jsonFormats = Serialization.formats(NoTypeHints)
//  def modelInit(knockoutModelName: String, model: AnyRef): Node = {
//    val json = jsonWrite(model)
//    val js = KnockoutJs.ApplyBindings(knockoutModelName, json)
//    JsCmds.Script(js)
//  }

  /** Invokes a JavaScript trigger. */
  def JsTriggerJson(triggerName: String, data: AnyRef): JsCmd =
    JsCmds.Run(s"$$(document).trigger('$triggerName',${jsonWrite(data)})")

  def InitVM(vmClassName: String, model: AnyRef): CssSel = {
    val json = jsonWrite(model)
    val js = JsCmds.Run(s"var VM=new $vmClassName($json)")
    "#initVM" #> JsCmds.Script(js)
  }

  def render = DAO.withSession(dao =>
    ClearClearable
//      & EditTemplateCss #> ""
      & InitVM("UCIViewModel", dao.findAllUseCaseSummaries)
      & ".new_uc button" #> SHtml.ajaxButton("+ New UC", jsCallbackWithDao(createNewUseCase))
  )

  def createNewUseCase(reactor: Reactor, dao: DAO): UseCaseSummary = {
    val uc = dao.createInitialUseCase(Defaults.Title, Defaults.FieldList.get)
//    val uc = UseCase(PlainValue(1, 2, 3), "UNTITLED", 4, 1000)
    val ucs = UseCaseSummary(uc.dataId, uc.valueId, uc.number, uc.title, Misc.currentTimeAsIso8601Str)
    reactor(JavaScript)(JsTriggerJson("new-uc", ucs))
    ucs
  }
}
